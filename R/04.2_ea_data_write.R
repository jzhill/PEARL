# Title and description --------------------------------------------

# Aggregating screening and hh data for writing to EA project
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   10 March 2025


# Packages -----------------------------------------

library(tidyverse)
library(REDCapR)
library(here)
library(purrr)

# Parameters ------------------------------

uri <- "https://redcap.sydney.edu.au/api/"
token_ea <- Sys.getenv("RCAPI_PEARL_ea")

## Pivot with count or sum for each field per EA -----------------------------

# Aggregate screening data
ea_pivot_ul_screen <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "") %>%
  group_by(ea_id) %>%
  summarise(
    pop_reg_screen = n(),
    date_screen = min(en_date_visit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(record_id = ea_id)

ea_pivot_ul_screen_mf <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "" & en_sex %in% c("M", "F")) %>%
  mutate(en_sex = tolower(en_sex)) %>%
  group_by(ea_id, en_sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = en_sex, values_from = count, names_prefix = "pop_reg_screen_") %>%
  mutate(
    pop_reg_screen_m = replace_na(pop_reg_screen_m, 0),
    pop_reg_screen_f = replace_na(pop_reg_screen_f, 0)
  ) %>%
  rename(record_id = ea_id)

# Aggregate household data
ea_pivot_ul_hh <- household_data %>%
  filter(!is.na(hh_ea_id) & hh_ea_id != "") %>%
  group_by(hh_ea_id) %>%
  summarise(
    pop_all = sum(hh_all, na.rm = TRUE),
    pop_current = sum(hh_size, na.rm = TRUE),
    pop_elig = sum(hh_size_elig, na.rm = TRUE),
    pop_reg_enum = sum(hh_reg, na.rm = TRUE),
    hh_enum = n(),
    date_enum = min(hh_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(record_id = hh_ea_id)

ea_pivot_ul <- reduce(c(list(ea_pivot_ul_screen, ea_pivot_ul_screen_mf, ea_pivot_ul_hh)), full_join, by = "record_id")

## Optionally write pivoted data to EA project ------------------------------

# Validate that the ea_pivot can be written
REDCapR::validate_for_write(ea_pivot)

if (interactive()) {
  choice <- menu(choices = c("Yes", "No"),
                 title = "Do you want to write the pivoted EA data to REDCap?")
  
  if (choice == 1) {
    message("Writing data to REDCap...")

  result <- REDCapR::redcap_write(
    ea_pivot_ul,
    redcap_uri = uri,
    token = token_ea,
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