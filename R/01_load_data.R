# Title and description --------------------------------------------

# Getting PEARL data from REDCap
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   16 Feb 2025


# Packages -----------------------------------------

library(tidyverse)
library(here)
library(REDCapR)

# Parameters -----------------------------

uri <- "https://redcap.sydney.edu.au/api/"
token_screen <- Sys.getenv("RCAPI_PEARL_screen")
token_hh <- Sys.getenv("RCAPI_PEARL_hh")
token_treat <- Sys.getenv("RCAPI_PEARL_treat")
report_screen_all <- 40643  # https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=19019&report_id=40643
report_hh_all <- 50913
report_treat_all <- 47495
datestamp <- format(Sys.time(), "%y%m%d")  # Create a datestamp to include in filenames

# Check if the token was successfully retrieved
if (token_screen == "" || token_hh == "" || token_treat == "") {
  stop("API token not found in environment. Please set REDCAP_API_TOKEN in your .Renviron file.")
}

## Retrieve each report and save as raw data ------------------------

# Create a named list for each report configuration

reports <- list(
  screening = list(
    token    = token_screen,
    report_id = report_screen_all
  ),
  household = list(
    token    = token_hh,
    report_id = report_hh_all
  ),
  treatment = list(
    token    = token_treat,
    report_id = report_treat_all
  )
)

# Loop over each report and process the report download and saving

for (report in names(reports)) {
  config <- reports[[report]]
  
  message("Downloading ", report, " report...")
  
  # Download the report
  report_data <- REDCapR::redcap_report(
    redcap_uri     = uri,
    token          = config$token,
    report_id      = config$report_id,
    raw_or_label   = "label",
    verbose        = TRUE,
    config_options = list(timeout = 5)  # Increase timeout to 5 seconds
  )$data
  
  # Build the subdirectory path and ensure it exists
  subdir <- here("data-raw", report)
  if (!dir.exists(subdir)) {
    dir.create(subdir, recursive = TRUE)
  }
  
  # Build the filename with the datestamp
  file_path <- here("data-raw", report, paste0(report, "_", datestamp, ".rds"))
  
  # Save the downloaded report data as an .rds file
  saveRDS(report_data, file = file_path)
  message("Saved to: ", file_path, "\n")
  
  # Now retrieve the data dictionary (metadata) for the project
  message("Downloading data dictionary for ", report, " project...")
  dd_data <- REDCapR::redcap_metadata_read(
    redcap_uri = uri,
    token = config$token,
    verbose = TRUE,
    config_options = list(timeout = 5)
  )$data
  
  # Build the filename for the data dictionary CSV file (e.g., "screening_dd.csv")
  dd_file_path <- here("data-raw", paste0(report, "_dd.csv"))
  
  # Save the data dictionary as a CSV file (this will overwrite the previous file)
  write_csv(dd_data, file = dd_file_path)
  message("Saved data dictionary to: ", dd_file_path, "\n")
}
