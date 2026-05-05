# title: "02_refined_taxa_list"
# scripts/prep/02_refined_taxa_list.R

# This script is to refine our taxonomic richness metrics, 
  # by refining our interpretation of what to count as a distinct taxon. 
  # We don't want to just count how many scientificName entries there are, 
  # but rather exclude higher-level taxonomic IDs if a lower-level taxon is present, 
  # e.g. we don't count both Macrouridae and Coryphaenoides longicirrhus for a species richness of 2.

# ==== SETUP =============================================
library(tidyverse)
library(readxl)
library(PristineSeasR2)


# ==== CONFIGURATION =============================================
ps_paths <- ps_science_paths()
exp_path <- file.path(ps_paths$expeditions, "TUV-2025")

# where data comes from and where it goes
raw_dir       <- file.path(exp_path, "data/primary/raw/dscm")
processed_dir <- file.path(exp_path, "data/primary/processed/dscm")


# Load cleanly processed dataset (from 01_data_prep)
# contains deployment metadata
clean_taxa <- read_rds(file.path(processed_dir, "clean_master_data.rds"))


# ==== REFINE GAMMA RICHNESS =============================================
# this flags redundant taxa, at expedition level (for gamma diversity calculation)
# Using existing taxonRank column from Tator_data_summary sheet

## --------- Tag redundant taxa: expedition-level -----------
taxa_dictionary_gamma <- clean_taxa %>%
  # 1. Get the distinct taxonomic paths (using taxonRank) 
  # and keep aphiaID for later use matching against GBIF and OBIS databases
  select(phylum, class, order, family, genus, scientificName, taxonRank, aphiaId) %>%
  distinct() %>%
  # Clean up NAs so counting works smoothly
  mutate(across(c(phylum, class, order, family, genus), ~replace_na(.x, ""))) %>%
  
    # 2. Count occurrences of each group in this unique list
  group_by(phylum) %>% mutate(n_phylum = n()) %>%
  group_by(class) %>% mutate(n_class = n()) %>%
  group_by(order) %>% mutate(n_order = n()) %>%
  group_by(family) %>% mutate(n_family = n()) %>%
  group_by(genus) %>% mutate(n_genus = n()) %>%
  ungroup() %>%
  
  # 3. Flag as redundant using taxonRank column
  mutate(
    rank_clean = tolower(taxonRank), # Normalizes text just in case
    is_redundant = case_when(
      rank_clean == "phylum" & n_phylum > 1 ~ TRUE,
      rank_clean == "class"  & n_class > 1 ~ TRUE,
      rank_clean == "order"  & n_order > 1 ~ TRUE,
      rank_clean == "family" & n_family > 1 ~ TRUE,
      rank_clean == "genus"  & n_genus > 1 ~ TRUE,
      TRUE ~ FALSE # Species and uniquely observed higher taxa are kept
    )
  )


## ---------- QA/QC the logic -----------
# see exactly what got flagged as redundant.
# This will print a list of every single higher-level taxon
# that was successfully tagged as redundant (like Macrouridae, or Ophiuroidea)
# because a lower-level ID bumped it out.

redundancy_check <- taxa_dictionary_gamma %>%
  filter(is_redundant == TRUE) %>%
  # Following logic applied one row at a time
  rowwise() %>%
  mutate(
    superseded_by = case_when(
      # If the redundant taxon is a Phylum, find all valid taxa in that Phylum
      rank_clean == "phylum" ~ paste(valid_taxa$scientificName[valid_taxa$phylum == phylum], collapse = ", "),
      
      # If it's a Class, find all valid taxa in that Class
      rank_clean == "class"  ~ paste(valid_taxa$scientificName[valid_taxa$class == class], collapse = ", "),
      
      # If it's an Order, find all valid taxa in that Order
      rank_clean == "order"  ~ paste(valid_taxa$scientificName[valid_taxa$order == order], collapse = ", "),
      
      # If it's a Family, find all valid taxa in that Family
      rank_clean == "family" ~ paste(valid_taxa$scientificName[valid_taxa$family == family], collapse = ", "),
      
      # If it's a Genus, find all valid taxa in that Genus
      rank_clean == "genus"  ~ paste(valid_taxa$scientificName[valid_taxa$genus == genus], collapse = ", "),
      
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  # Select the columns to display, including a'superseded_by' column
  select(scientificName, taxonRank, superseded_by) %>%
  arrange(taxonRank, scientificName)

# Print the full diagnostic list
print(redundancy_check, n = Inf)


## ------------ Extract gamma richness -------------------
# taxonomic richness across the expedition
# for use to generate expedition taxa list

# Filter out the redundant taxa
gamma_richness_list <- taxa_dictionary_gamma %>%
  filter(is_redundant == FALSE)

# Calculate refined metrics for total, fish, and invert richness
total_taxa_refined <- nrow(gamma_richness_list)
fish_taxa_refined <- nrow(filter(gamma_richness_list, phylum == "Chordata"))
invert_taxa_refined <- nrow(filter(gamma_richness_list, phylum != "Chordata"))

# Print richness values
cat("\n--- GAMMA DIVERSITY (EXPEDITION TOTALS) ---\n")
cat("Refined Total Taxa:", total_taxa_refined, "\n")
cat("Refined Fish Taxa:", fish_taxa_refined, "\n")
cat("Refined Invertebrate Taxa:", invert_taxa_refined, "\n")


# ============= REFINE ALPHA RICHNESS =====================
# taxonomic richness per deployment
# refined by removing redundant taxa (at deployment level)
# Using existing taxonRank column
# Need to exclude partial deployments here.

## --------- Tag redundant taxa: deployment-level -----------
# 1. Load partial deployment exclusions
exclusions <- read_csv(file.path(raw_dir, "freq_exclusions.csv"), show_col_types = FALSE)

alpha_taxa <- clean_taxa %>%
  # filter out partial drops first
  # anti_join removes any row in clean_taxa where the deployment matches the exclusions list
  anti_join(exclusions, by = c("deployment" = "deployment")) %>%
  
  # Keep Deployment ID ('deployment') in the distinct selection
  select(deployment, phylum, class, order, family, scientificName, taxonRank) %>%
  distinct() %>%
  mutate(across(c(phylum, class, order, family), ~replace_na(.x, ""))) %>%
  mutate(first_word = str_extract(scientificName, "^[^ ]+")) %>%
  
  # Group by deployment and taxon level
  group_by(deployment, phylum) %>% mutate(n_phylum = n()) %>%
  group_by(deployment, class) %>% mutate(n_class = n()) %>%
  group_by(deployment, order) %>% mutate(n_order = n()) %>%
  group_by(deployment, family) %>% mutate(n_family = n()) %>%
  group_by(deployment, first_word) %>% mutate(n_genus = n()) %>%
  ungroup() %>%
  
  # Flag as redundant using taxonRank column
    mutate(
    rank_clean = tolower(taxonRank),
    is_redundant_alpha = case_when(
      rank_clean == "phylum" & n_phylum > 1 ~ TRUE,
      rank_clean == "class"  & n_class > 1 ~ TRUE,
      rank_clean == "order"  & n_order > 1 ~ TRUE,
      rank_clean == "family" & n_family > 1 ~ TRUE,
      rank_clean == "genus"  & n_genus > 1 ~ TRUE,
      TRUE ~ FALSE 
    )
  )

## ------------ Extract alpha richness -------------------
# taxonomic richness per deployment
# for use in generating average richness metrics (overall, by depth, by region)

# Calculate deployment totals
deployment_richness_list <- alpha_taxa %>%
  # Filter out taxa that are redundant within each specific deployment
  filter(is_redundant_alpha == FALSE) %>%
  group_by(deployment) %>% 
  summarize(
    alpha_total = n(),
    alpha_fish = sum(phylum == "Chordata", na.rm = TRUE),
    alpha_invert = sum(phylum != "Chordata", na.rm = TRUE),
    .groups = "drop"
  )

# ======= EXPORT PROCESSED GAMMA DATA =================================

# Save refined expedition taxa list to directory defined in configuration
# output used for downstream OBIS/GBIF/IUCN scripts
# write_csv(taxa_dictionary_gamma, file.path(processed_dir, "refined_taxa_list_gamma.csv"))


## NOTE: If downstream scripts break, may need to rename back to "is_redundant"
# uncomment below if necessary
#export_gamma <- taxa_dictionary_gamma %>% 
#  rename(is_redundant = is_redundant_gamma)

#message(paste("Data prep complete! Richness data saved to:", file.path(processed_dir, "refined_taxa_list_gamma.rds")))

# NOTE: not exporting .csv for deployment_richness_list, as this df can simply be called in downstream scripts
# for further outputs
