# title: "03_gbif_obis_check"
# scripts/prep/03_gbif_obis_check.R

# This script 

# ==== SETUP =============================================
library(tidyverse)
library(rgbif) # For GBIF data
library(robis)   # For OBIS data (Install via: install.packages("robis"))
library(PristineSeasR2)

# ==== CONFIGURATION =============================================
ps_paths <- ps_science_paths()
exp_path <- file.path(ps_paths$expeditions, "TUV-2025")
raw_dir       <- file.path(exp_path, "data/primary/raw/dscm") # where data comes from
processed_dir <- file.path(exp_path, "data/primary/processed/dscm") # where data goes

# Set GBIF's 2-letter ISO code for the country of interest
gbif_country_code <- "TV"

# Country of interest's area ID in OBIS
country_areaid <- 244 

# Output filename for new records check
records_output_filename <- "tuv_new_records_check.csv"

## ----- load cleaned data----
# Calling the refined taxa list generated in 02_refined_taxa_list.R.
# This refined list has already removed the redundant higher taxonomies
# so that we don't double-count "new records" if they're redundant,
# e.g. an un-IDed Coryphaenoides and Coryphaenoides longicirrhus 
# may both be TRUE for new record. But really, we definitively have only 1 new taxa. 

clean_taxa <- read_csv(file.path(processed_dir, "refined_taxa_list.csv"), show_col_types = FALSE)

# Get unique, non-NA scientific names to query
message("Extracting unique taxa...")
unique_names <- clean_taxa %>%
  filter(is_redundant == FALSE) %>%  # use only the non-redundant taxa
  filter(!is.na(scientificName), scientificName != "") %>%
  distinct(scientificName) %>%
  pull(scientificName)

total_unique_names <- length(unique_names)

# ===== CHECK GBIF OCCURRENCES =====================
message("\n--- STARTING GBIF QUERY ---")
gbif_counter <- 0

## ------ Set up GBIF function --------

get_gbif_status <- function(name, search_country) {
  gbif_counter <<- gbif_counter + 1
  message(sprintf("[%d/%d] Checking GBIF: %s", gbif_counter, total_unique_names, name))
  
  tryCatch({
    # Get the taxon key
    key_res <- rgbif::name_backbone(name = name)
    Sys.sleep(0.1) # give a pause
    
    # Flag if GBIF doesn't recognize the name
    if (!"usageKey" %in% names(key_res)) {
      return(tibble(
        scientificName = name, 
        gbif_backbone_match = FALSE, 
        found_in_gbif = FALSE
      ))
    }
    
    # If recognized, check Tuvalu occurrences
    # gbif_country of interest is set up in the Configuration
    occ_res <- rgbif::occ_search(taxonKey = key_res$usageKey, country = search_country, limit = 1)
    Sys.sleep(0.2) # pause
    
    found <- !is.null(occ_res$data) && nrow(occ_res$data) > 0
    
    return(tibble(
      scientificName = name, 
      gbif_backbone_match = TRUE, 
      found_in_gbif = found
    ))
    
  }, error = function(e) {
    message("  -> API Error on GBIF")
    return(tibble(
      scientificName = name, 
      gbif_backbone_match = NA, 
      found_in_gbif = NA
    ))
  })
}

## -------- Run GBIF check ------
# bind rows into a dataframe
gbif_results <- purrr::map_dfr(
  unique_names, 
  ~get_gbif_status(name = .x, search_country = gbif_country_code)
)


# ========== CHECK OBIS OCCURRENCES  ===============
message("\n--- STARTING OBIS QUERY ---")
obis_counter <- 0

## -------- Set up OBIS function --------

get_obis_status <- function(name, aphia_id) {
  obis_counter <<- obis_counter + 1
  message(sprintf("[%d/%d] Checking OBIS: %s", obis_counter, total_unique_names, name))
  
  tryCatch({
    # Search by taxonid (AphiaID) instead of scientificname string
    # (If an aphia_id is missing, fallback to the text string)
     # Check if aphia_id exists and is not the word "NA"
    if (!is.na(aphia_id) && aphia_id != "" && as.character(aphia_id) != "NA") {
      # Use as.numeric() in case the CSV imported the ID as text
      res <- robis::occurrence(taxonid = as.numeric(aphia_id), areaid = country_areaid)
    } else {
      res <- robis::occurrence(scientificname = name, areaid = country_areaid)
    }
    
    Sys.sleep(0.2)
    return(nrow(res) > 0)
    
  }, error = function(e) {
    # print what is causing failure
    message(paste("\n  -> SCRIPT ERROR for", name, ":", e$message))
    return(NA) # returns NA for final table
  })
}

## ----- Run OBIS check ----------
obis_results <- clean_taxa %>%
  filter(is_redundant == FALSE) %>%
  filter(!is.na(scientificName), scientificName != "") %>%
  select(scientificName, aphiaId) %>%
  distinct(scientificName, .keep_all = TRUE) %>%
  mutate(
    # map2_lgl passes both the name and AphiaID to the function
    found_in_obis = purrr::map2_lgl(scientificName, aphiaId, get_obis_status)
  ) %>%
  select(scientificName, found_in_obis)

# ---- MERGE RESULTS & FLAG NEW RECORDS ----
message("\n--- COMPILING RESULTS ---")

## ------ Run the merge ---------
taxa_record_status <- tibble(scientificName = unique_names) %>%
  left_join(gbif_results, by = "scientificName") %>%
  left_join(obis_results, by = "scientificName") %>%
  # CALL TAXON RANK HERE
  # retain taxonRank for QA/QC, to facilitate cause of any missed matched to databases (due to sub-level hierarchies)
  left_join(clean_taxa %>% select(scientificName, taxonRank) %>% distinct(), by = "scientificName") %>%
  mutate(
    # Clean up any NAs in case the API timed out
    found_in_gbif = if_else(is.na(found_in_gbif), FALSE, found_in_gbif),
    found_in_obis = if_else(is.na(found_in_obis), FALSE, found_in_obis),
    
    # Flag names that need manual taxonomy review (failed GBIF backbone)
    needs_name_review = !gbif_backbone_match,
    
    # "new record" if missing from both OBIS and GBIF
    new_record_for_country = !(found_in_gbif | found_in_obis)
  ) %>%
  # Sort so the ones needing review and the potential new records are at the top
  arrange(desc(needs_name_review), desc(new_record_for_country))

# Check the ones flagged for review or tagged as new
cat("\n=== ITEMS FLAGGED ===\n")
taxa_record_status %>% 
  filter(needs_name_review == TRUE | new_record_for_country == TRUE) %>% 
  print(n = Inf)

## ---- Manual overrides ----
# This is to log the taxa that were initially tagged as FALSE match to GBIF/OBIS
# but were manually checked and found to actually occur in the dataset.
# Manually correct these falsely-tagged "New Records."

message("Applying manual database corrections...")

### ------ Create hard-coded table of manual findings --------
# This is where to add/edit manual corrections
# for transparency and logging.
manual_corrections <- tribble(
  ~scientificName,    ~manual_gbif, ~manual_obis, ~correction_notes,
  "Decapodiformes",   TRUE,         NA,          "Manually confirmed in GBIF web portal",
  "Echinothuriinae",  FALSE,        TRUE,        "Missing in GBIF, Manually confirmed in OBIS (Echinothuriidae)"
)

### ----- Apply manual overrides to results --------
taxa_record_status <- taxa_record_status %>%
  left_join(manual_corrections, by = "scientificName") %>%
  mutate(
    # If a manual_gbif value exists, use it. Otherwise, keep API's original answer.
    found_in_gbif = if_else(!is.na(manual_gbif), manual_gbif, found_in_gbif),
    found_in_obis = if_else(!is.na(manual_obis), manual_obis, found_in_obis),
    
    # If a note was provided, it has been reviewed, so turn off the warning flag
    needs_name_review = if_else(!is.na(correction_notes), FALSE, needs_name_review),
    
    # Recalculate the final "New Record" status with the updated truths
    new_record_for_country = !(found_in_gbif | found_in_obis)
  ) %>%
  # re-order columns
  # Put taxonRank right next to scientificName so it's easy to read
  select(
    scientificName, 
    taxonRank, 
    found_in_gbif, 
    found_in_obis, 
    new_record_for_country, 
    needs_name_review, 
    correction_notes
  )

# ======= EXPORT DATA FOR MANUAL REVIEW ======
write_csv(taxa_record_status, file.path(processed_dir, records_output_filename))
message(sprintf("✅ Export complete! '%s' has been saved to '%s'.", records_output_filename, processed_dir))