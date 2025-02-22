# Title and description --------------------------------------------

# Loading PEARL data from REDCap
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
report_screen_all <- 40643
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
    report_id = report_screen_all,
    prefix   = "screening_all_results"
  ),
  household = list(
    token    = token_hh,
    report_id = report_hh_all,
    prefix   = "hh_all_enum"
  ),
  treatment = list(
    token    = token_treat,
    report_id = report_treat_all,
    prefix   = "treat_all_reg"
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
    verbose        = TRUE,
    config_options = list(timeout = 60)  # Increase timeout to 60 seconds
  )$data
  
  # Build the filename with the datestamp
  file_path <- here("data-raw", paste0(config$prefix, "_", datestamp, ".rds"))
  
  # Save the downloaded report data as an .rds file
  saveRDS(report_data, file = file_path)
  message("Saved to: ", file_path, "\n")
}