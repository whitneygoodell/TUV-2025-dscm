# title: "04_iucn_status"
# scripts/prep/04_iucn_status.R

#This script currently doesn't work very well. Consult with Molly on a batter IUCN script.

# ==== SETUP =============================================
library(tidyverse)
library(httr)      # <--- The engine for the V4 API
library(jsonlite)  # <--- To read the new JSON format
library(PristineSeasR2)

# ==== CONFIGURATION =====================================
ps_paths <- ps_science_paths()
exp_path <- file.path(ps_paths$expeditions, "TUV-2025")
raw_dir       <- file.path(exp_path, "data/primary/raw/dscm")
processed_dir <- file.path(exp_path, "data/primary/processed/dscm")

# Your token works on V4!
my_key <- "itxf4rwjN7gGsLhxBtqyQNemqyHFm77rJ9uH" 

# Load refined list
taxa_list <- read_csv(file.path(processed_dir, "refined_taxa_list.csv"), show_col_types = FALSE)

# ==== FILTER FOR SPECIES ================================
message("Filtering list to species-level taxa only...")

species_to_check <- taxa_list %>%
  filter(tolower(taxonRank) == "species") %>%
  filter(is_redundant == FALSE) %>%
  distinct(scientificName) %>%
  pull(scientificName)

total_species <- length(species_to_check)
message(sprintf("Found %d distinct species to check against IUCN.", total_species))


# ==== QUERY IUCN V4 API =================================
message("\n--- STARTING IUCN V4 QUERY ---")
iucn_counter <- 0

get_iucn_status <- function(sci_name) {
  iucn_counter <<- iucn_counter + 1
  message(sprintf("[%d/%d] Checking IUCN V4: %s", iucn_counter, total_species, sci_name))
  
  tryCatch({
    # 1. Format the name for the V4 URL
    safe_name <- URLencode(str_trim(sci_name))
    url <- paste0("https://api.iucnredlist.org/api/v4/taxa/scientific_name/", safe_name)
    
    # 2. V4 Authentication: Send the token securely in the Header
    res <- httr::GET(
      url, 
      httr::add_headers(Authorization = paste("Bearer", my_key))
    )
    
    Sys.sleep(1.2) # API speed limit
    
    # 3. Check if the species exists (404 means it's not in the database)
    if (httr::status_code(res) == 404) {
      return("Not Evaluated")
    }
    
    # Check if the token was rejected
    if (httr::status_code(res) == 401 || httr::status_code(res) == 403) {
      return("Token Error")
    }
    
    # 4. Parse the V4 JSON response
    raw_text <- httr::content(res, as = "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(raw_text)
    
    # 5. Dig into the V4 nested data to find the status code
    if ("assessments" %in% names(parsed)) {
      # The category code is buried in a nested dataframe
      cat_code <- parsed$assessments$red_list_category$code[1]
      
      if (!is.null(cat_code) && !is.na(cat_code)) {
        return(cat_code)
      }
    }
    
    return("Not Evaluated")
    
  }, error = function(e) {
    message(paste("\n  !! ERROR for", sci_name, ":", e$message))
    return("Error")
  })
}

# Run the loop!
iucn_results <- tibble(scientificName = species_to_check) %>%
  mutate(
    iucn_status = purrr::map_chr(scientificName, get_iucn_status)
  )

# ==== FORMAT AND EXPORT =================================
message("\n--- COMPILING FINAL IUCN RESULTS ---")

final_iucn_table <- taxa_list %>%
  filter(tolower(taxonRank) == "species", is_redundant == FALSE) %>%
  left_join(iucn_results, by = "scientificName") %>%
  mutate(
    iucn_status_full = case_when(
      iucn_status == "LC" ~ "Least Concern",
      iucn_status == "NT" ~ "Near Threatened",
      iucn_status == "VU" ~ "Vulnerable",
      iucn_status == "EN" ~ "Endangered",
      iucn_status == "CR" ~ "Critically Endangered",
      iucn_status == "DD" ~ "Data Deficient",
      iucn_status == "Not Evaluated" ~ "Not Evaluated",
      TRUE ~ iucn_status 
    )
  ) %>%
  select(phylum, class, order, family, scientificName, iucn_status, iucn_status_full)

output_file <- file.path(processed_dir, "tuv_dscm_iucn_status.csv")
write_csv(final_iucn_table, output_file)
message(paste("IUCN check complete! Conservation data saved to:", output_file))