# Title and description --------------------------------------------

# Making skim outputs for LLM assistance
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   11 Mar 2025


# Packages -----------------------------------------

library(tidyverse)
library(skimr)
library(here)

# Function to generate and save skim summary
generate_skim_summary <- function(dataset, dataset_name) {
  output_dir <- here("data-processed", "skim_summary")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  skim_summary <- skim(dataset)
  csv_path <- file.path(output_dir, paste0(dataset_name, "_skim.csv"))
  write_csv(skim_summary, csv_path)
  
  message("Skim summary saved for: ", dataset_name)
  return(csv_path)
}

datasets <- list(
  screening_data = screening_data,
  household_data = household_data,
  ea_data = ea_data,
  treatment_data = treatment_data
)

skim_files <- map2(datasets, names(datasets), ~ generate_skim_summary(.x, .y))
