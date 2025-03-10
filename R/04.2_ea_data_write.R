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
token_screen <- Sys.getenv("RCAPI_PEARL_screen")
token_hh <- Sys.getenv("RCAPI_PEARL_hh")
token_treat <- Sys.getenv("RCAPI_PEARL_treat")
token_ea <- Sys.getenv("RCAPI_PEARL_ea")

## Pivot with count or sum for each field per EA -----------------------------

# Aggregate screening data: count the number of registered individuals per EA
reg <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "") %>%
  group_by(ea_id) %>%
  summarise(pop_reg_screen_new = n(), .groups = "drop") %>%
  rename(record_id = ea_id)

# Aggregate screening data: count the number of registered males per EA
reg_m <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "") %>%
  filter(en_sex == "M") %>%
  group_by(ea_id) %>%
  summarise(pop_reg_screen_m_new = n(), .groups = "drop") %>%
  rename(record_id = ea_id)

# Aggregate screening data: count the number of registered females per EA
reg_f <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "") %>%
  filter(en_sex == "F") %>%
  group_by(ea_id) %>%
  summarise(pop_reg_screen_f_new = n(), .groups = "drop") %>%
  rename(record_id = ea_id)

# Aggregate enumeration data: population belonging to households
pop_all <- household_data %>%
  filter(!is.na(hh_ea_id) & hh_ea_id != "") %>%
  group_by(hh_ea_id) %>%
  summarise(pop_all_new = sum(hh_all, na.rm = TRUE), .groups = "drop") %>%
  rename(record_id = hh_ea_id)

# Aggregate enumeration data: population currently resident
pop_current <- household_data %>%
  filter(!is.na(hh_ea_id) & hh_ea_id != "") %>%
  group_by(hh_ea_id) %>%
  summarise(pop_current_new = sum(hh_size, na.rm = TRUE), .groups = "drop") %>%
  rename(record_id = hh_ea_id)

# Aggregate enumeration data: population eligible
pop_elig <- household_data %>%
  filter(!is.na(hh_ea_id) & hh_ea_id != "") %>%
  group_by(hh_ea_id) %>%
  summarise(pop_elig_new = sum(hh_size_elig, na.rm = TRUE), .groups = "drop") %>%
  rename(record_id = hh_ea_id)

# Aggregate enumeration data: population eligible
pop_reg_enum <- household_data %>%
  filter(!is.na(hh_ea_id) & hh_ea_id != "") %>%
  group_by(hh_ea_id) %>%
  summarise(pop_reg_enum_new = sum(hh_reg, na.rm = TRUE), .groups = "drop") %>%
  rename(record_id = hh_ea_id)

# Aggregate enumeration data: households enumerated
hh_enum <- household_data %>%
  filter(!is.na(hh_ea_id) & hh_ea_id != "") %>%
  group_by(hh_ea_id) %>%
  summarise(hh_enum_new = n(), .groups = "drop") %>%
  rename(record_id = hh_ea_id)

ea_pivot <- reduce(list(reg, reg_m, reg_f, pop_all, pop_current, pop_elig, pop_reg_enum, hh_enum), full_join, by = "record_id")

ea_data <- ea_data %>%
  left_join(ea_pivot, by = "record_id")

## Optionally write pivoted data to EA project ------------------------------

# Validate that the ea_pivot can be written
REDCapR::validate_for_write(ea_pivot)

if (interactive()) {
  choice <- menu(choices = c("Yes", "No"),
                 title = "Do you want to write the pivoted EA data to REDCap?")
  
  if (choice == 1) {
    message("Writing data to REDCap...")

    ea_pivot <- ea_pivot %>%
    rename(pop_reg_screen = pop_reg_screen_new,
           pop_reg_screen_m = pop_reg_screen_m_new,
           pop_reg_screen_f = pop_reg_screen_f_new,
           pop_all = pop_all_new,
           pop_current = pop_current_new,
           pop_elig = pop_elig_new,
           pop_reg_enum = pop_reg_enum_new,
           hh_enum = hh_enum_new)
  
  result <- REDCapR::redcap_write(
    ea_pivot,
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