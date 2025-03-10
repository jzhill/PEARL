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
token_screen <- Sys.getenv("RCAPI_PEARL_screen")
token_hh <- Sys.getenv("RCAPI_PEARL_hh")
token_treat <- Sys.getenv("RCAPI_PEARL_treat")
token_ea <- Sys.getenv("RCAPI_PEARL_ea")

## Pivot with count or sum for each field per household -----------------------------

# Aggregate screening data: count the number of screened individuals per dwelling_id
hh_reg_pivot <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_reg_new = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

# Aggregate screening data: count the number of individuals with TB decision per dwelling_id
hh_tbdec_pivot <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  filter(!is.na(tb_decision) & tb_decision != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_tbdec_new = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

hh_pivot <- full_join(hh_reg_pivot, hh_tbdec_pivot, by = "record_id")

household_data <- household_data %>%
  select(-any_of(c("hh_reg_new", "hh_tbdec_new"))) %>% 
  left_join(hh_pivot, by = "record_id")

## Optionally write pivoted data to household project ------------------------------

# Validate that the hh_pivot can be written
REDCapR::validate_for_write(hh_pivot)

if (interactive()) {
  choice <- menu(choices = c("Yes", "No"),
                 title = "Do you want to write the pivoted household data to REDCap?")
  
  if (choice == 1) {
    message("Writing data to REDCap...")
    
    hh_pivot <- hh_pivot %>%
      rename(hh_reg = hh_reg_new,
             hh_tbdec = hh_tbdec_new)
    
    result <- REDCapR::redcap_write(
      hh_pivot,
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

