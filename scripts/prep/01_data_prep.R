# title: "01_data_prep"
# scripts/prep/01_data_prep.R

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


# ==== LOAD DATA =========
excel_file <- file.path(raw_dir, "TUV_DOEX0112_dscm_Data_Summary_v2.xlsx")
tator_data <- read_excel(excel_file, sheet = "Tator_Data_Summary")
metadata   <- read_excel(excel_file, sheet = "Field_Log_Metadata", skip = 1) # Skip  first row (CATAMI header group name)


# ==== MERGE AND CLEAN =========

# Select columns of interest from metadata
metadata_subset <- metadata %>%
  select(Deployment, `Depth (m)`, Location, `Substrate (Hard/Soft)`) %>%
  rename(deployment = Deployment)

# Merge metadata with annotation data
master_data <- tator_data %>%
  select(-`depth(m)`) %>% 
  left_join(metadata_subset, by = "deployment") %>%
  rename(depth_m = `Depth (m)`, 
         substrate_type = `Substrate (Hard/Soft)`, 
         location = Location)


# ====  MANUAL EXCLUSIONS ========

## Taxa exclusions (QA/QC for Species List)

# Manual exclusions are defined in a .csv created in the process of inital data review
# and is used to exclude records from analysis, for whatver reason,
# such as an observation on descent or at the surface.
# The directory .csv file for which records to exclude lives in the raw data folder.

exclusion_file <- file.path(raw_dir, "manual_exclusions.csv")

if (file.exists(exclusion_file)) {
  exclusions <- read_csv(exclusion_file, show_col_types = FALSE)
  rows_before <- nrow(master_data) # Store row count before exclusion for a helpful message
  
  master_data <- master_data %>%   # Drop the matching records from master_data
    anti_join(exclusions, by = c("deployment", "scientificName"))
    
  rows_after <- nrow(master_data)   
  message(paste("Applied manual exclusions. Dropped", (rows_before - rows_after), "records."))
} else {
  message("No manual_exclusions.csv file found. Proceeding with all records.")
}

## Deployment exclusions (For freq. of occ. calculations; Flagging Partial Deployments)

freq_exclusion_file <- file.path(raw_dir, "freq_exclusions.csv")

if (file.exists(freq_exclusion_file)) {
  freq_exclusions <- read_csv(freq_exclusion_file, show_col_types = FALSE)
  
  # Create the flag: FALSE if it's in the bad list, TRUE otherwise
  master_data <- master_data %>%
    mutate(valid_for_freq = !deployment %in% freq_exclusions$deployment)
  
  message(paste("Flagged", nrow(freq_exclusions), "deployments as invalid for frequency calculations."))
} else {
  # If the file doesn't exist yet, assume all deployments are valid
  master_data <- master_data %>%
    mutate(valid_for_freq = TRUE)
  message("No freq_exclusions.csv found. All deployments flagged as valid_for_freq = TRUE.")
}

# ======= EXPORT PROCESSED DATA =================================

# Just in case the processed folder doesn't exist on the Drive yet, this creates it:
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# Save clean dataset using directory defined in configuration
output_file <- file.path(processed_dir, "clean_master_data.rds")
write_rds(master_data, output_file)

message(paste("Data prep complete! Clean data saved to:", output_file))