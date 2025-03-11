# Title and description --------------------------------------------

# Aggregating screening data at household level and uploading to the hh project
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   09 March 2025


# Packages -----------------------------------------

library(tidyverse)
library(REDCapR)
library(here)
library(purrr)

# Parameters ------------------------------------

uri <- "https://redcap.sydney.edu.au/api/"
token_ea <- Sys.getenv("RCAPI_PEARL_ea")

## Pivot with count or sum for each field per household -----------------------------

# Aggregate screening data: count the number of screened individuals per dwelling_id
hh_pivot_ul_reg <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_reg = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

# Aggregate screening data: count the number of individuals with TB decision per dwelling_id
hh_pivot_ul_tbdec <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  filter(!is.na(tb_decision) & tb_decision != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_tbdec = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

hh_pivot_ul <- full_join(hh_pivot_ul_reg, hh_pivot_ul_tbdec, by = "record_id")

## Optionally write pivoted data to household project ------------------------------

# Validate that the hh_pivot can be written
REDCapR::validate_for_write(hh_pivot)

if (interactive()) {
  choice <- menu(choices = c("Yes", "No"),
                 title = "Do you want to write the pivoted household data to REDCap?")
  
  if (choice == 1) {
    message("Writing data to REDCap...")
    
    result <- REDCapR::redcap_write(
      hh_pivot_ul,
      redcap_uri = uri,
      token = token_hh,
      overwrite_with_blanks = TRUE,
      verbose = TRUE
    )
    
    message("Data written to REDCap. Response: ", result)
  } else {
    message("Data was not written to REDCap.")
  }
} else {
  message("Non-interactive mode detected; data was not written to REDCap.")
}

