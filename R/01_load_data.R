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
screen_all_results <- 40643
datestamp <- format(Sys.time(), "%y%m%d")  # Create a datestamp to include in filenames

# Check if the token was successfully retrieved
if (token_screen == "") {
  stop("API token not found in environment. Please set REDCAP_API_TOKEN in your .Renviron file.")
}

# Return screening all results report as a tibble
report_data <- REDCapR::redcap_report(
  redcap_uri     = uri,
  token          = token_screen,
  report_id      = screen_all_results,
  verbose        = TRUE,
  config_options = list(timeout = 60)  # Increase timeout to 60 seconds
)$data

# Save the downloaded report data as an .rds file
saveRDS(report_data, file = here("data-raw", paste0("screening_all_results_", datestamp, ".rds")))
