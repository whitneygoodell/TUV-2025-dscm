# title: "02_refined_taxa_list"
# scripts/prep/02_refined_taxa_list.R

# This script is to refine our taxonomic richness metrics, 
  # by refining our interpretation of what to count as a distinct taxon. 
  # Currently, we just count how many scientificName entries are fish or inverts. 
  # But we want to exclude higher-level taxonomic IDs if a lower-level taxon is present, 
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
clean_taxa <- read_rds(file.path(processed_dir, "clean_master_data.rds"))


# ==== REFINE TAXA =============================================

# Using existing taxonRank column from Tator_data_summary sheet

refined_taxa_list <- clean_taxa %>%
  # 1. Get the distinct taxonomic paths (using taxonRank)
  select(phylum, class, order, family, scientificName, taxonRank) %>%
  distinct() %>%
  # Clean up NAs so counting works smoothly
  mutate(across(c(phylum, class, order, family), ~replace_na(.x, ""))) %>%
  
  # Extract the first word (the Genus) to group species by Genus
  mutate(first_word = str_extract(scientificName, "^[^ ]+")) %>%
  
  # 2. Count occurrences of each group in this unique list
  group_by(phylum) %>% mutate(n_phylum = n()) %>%
  group_by(class) %>% mutate(n_class = n()) %>%
  group_by(order) %>% mutate(n_order = n()) %>%
  group_by(family) %>% mutate(n_family = n()) %>%
  group_by(first_word) %>% mutate(n_genus = n()) %>%
  ungroup() %>%
  
  # 3. Flag as redundant using YOUR taxonRank column
  mutate(
    rank_clean = tolower(taxonRank), # Normalizes text just in case!
    is_redundant = case_when(
      rank_clean == "phylum" & n_phylum > 1 ~ TRUE,
      rank_clean == "class"  & n_class > 1 ~ TRUE,
      rank_clean == "order"  & n_order > 1 ~ TRUE,
      rank_clean == "family" & n_family > 1 ~ TRUE,
      rank_clean == "genus"  & n_genus > 1 ~ TRUE,
      TRUE ~ FALSE # Species and uniquely observed higher taxa are kept!
    )
  )


# ==== EXTRACT RICHNESS =============================================

# Filter out the redundant taxa
true_richness_list <- refined_taxa_list %>%
  filter(is_redundant == FALSE)

# Calculate refined metrics
total_taxa_refined <- nrow(true_richness_list)
fish_taxa_refined <- nrow(filter(true_richness_list, phylum == "Chordata"))
invert_taxa_refined <- nrow(filter(true_richness_list, phylum != "Chordata"))

# Print the results to the console so you can see them:
cat("Refined Total Taxa:", total_taxa_refined, "\n")
cat("Refined Fish Taxa:", fish_taxa_refined, "\n")
cat("Refined Invertebrate Taxa:", invert_taxa_refined, "\n")


# ===== QA/QC the logic ============
# see exactly what the code decided to throw out  
# This will print a list of every single higher-level taxon 
# that was successfully dropped (like Macrouridae, or Ophiuroidea)
# because a lower-level ID bumped it out.

refined_taxa_list %>%
  filter(is_redundant == TRUE) %>%
  select(scientificName, family, taxonRank) %>%
  print(n = Inf)



