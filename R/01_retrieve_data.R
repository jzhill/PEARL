# Title and Description --------------------------------------------

# Getting PEARL data from REDCap
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   16 Feb 2025
# Last modified:    15 Mar 2025

# Packages -----------------------------------------

library(tidyverse)
library(here)
library(REDCapR)

# Parameters -----------------------------

uri <- "https://redcap.sydney.edu.au/api/"

# Ensure API tokens are stored securely in .Renviron and loaded via Sys.getenv()

tokens <- list(
  screening = Sys.getenv("RCAPI_PEARL_screen"),
  household = Sys.getenv("RCAPI_PEARL_hh"),
  treatment = Sys.getenv("RCAPI_PEARL_treat"),
  ea = Sys.getenv("RCAPI_PEARL_ea")
)

report_ids <- list(
  screening = 40643,  # https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=19019&report_id=40643
  household = 50913,  # https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=19007&report_id=50913
  treatment = 47495,  # https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=19018&report_id=47495
  ea = 54859  # https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=24148&report_id=54859
)

datestamp <- format(Sys.time(), "%Y-%m-%d_%H%M")  

# Required directories for this script
required_dirs <- c(
  here("data-raw"),
  here("data-raw/dds"),
  here("data-raw/screening"),
  here("data-raw/household"),
  here("data-raw/treatment"),
  here("data-raw/ea")
)

# Ensure all required directories exist; create them if missing
for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message("Created missing directory: ", dir)
  }
}

# Check if API tokens are retrieved
if (any(map_chr(tokens, ~ .x) == "")) {
  stop("One or more API tokens not found in environment. Please check your .Renviron file.")
}

# Loop through reports
for (report in names(tokens)) {
  
  message("Downloading ", report, " report...")
  
  # Download the REDCap report
  report_data <- REDCapR::redcap_report(
    redcap_uri = uri,
    token = tokens[[report]],
    report_id = report_ids[[report]],
    raw_or_label_headers = "label",
    raw_or_label = "label",
    verbose = TRUE,
    config_options = list(timeout = 5)
  )$data
  
  # Save the report data
  file_path <- here("data-raw", report, paste0(report, "_", datestamp, ".csv"))
  write_csv(report_data, file_path)
  message("Saved to: ", file_path, "\n")
  
  # Download the data dictionary
  message("Downloading data dictionary for ", report, " project...")
  dd_data <- REDCapR::redcap_metadata_read(
    redcap_uri = uri,
    token = tokens[[report]],
    verbose = TRUE,
    config_options = list(timeout = 10)
  )$data
  
  # Save the data dictionary in the /dds/ folder
  dd_file_path <- here("data-raw/dds", paste0(report, "_dd.csv"))
  write_csv(dd_data, dd_file_path)
  message("Saved data dictionary to: ", dd_file_path, "\n")
}
