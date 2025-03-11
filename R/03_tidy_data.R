# Title and description --------------------------------------------

# Cleaning, tidying and transforming PEARL data ready for analysis
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   23 Feb 2025


# Packages -----------------------------------------

library(here)
library(stringr)
library(lubridate)
library(tidyverse)
library(epikit)
library(dplyr)
library(purrr)

# Screening data checking and consolidating columns ----------------------

# Create helper column for TST read
screening_data <- screening_data %>%
  mutate(tst_read_bin = ifelse(!is.na(tst_read_mm) | !is.na(tst_read_positive), TRUE, FALSE))

# Age category in 10 year groups -------------------------------

screening_data <- screening_data %>%
  mutate(age_cat = epikit::age_categories(en_cal_age, by = 10, upper = 80))

treatment_data <- treatment_data %>%
  mutate(age_cat = epikit::age_categories(tpt_age, by = 10, upper = 80))

# Weeks for all dates ---------------------------------

max_week <- floor_date(Sys.Date(), unit = "week", week_start = 1)

screening_data <- screening_data %>%
  mutate(week_reg = floor_date(en_date_visit, unit = "week", week_start = 1)) %>%
  mutate(week_reg = if_else(week_reg > max_week, NA_Date_, week_reg))

treatment_data <- treatment_data %>%
  mutate(week_reg = floor_date(tpt_reg_date, unit = "week", week_start = 1)) %>%  
  mutate(week_reg = if_else(week_reg > max_week, NA_Date_, week_reg)) %>% 
  mutate(week_start = floor_date(tpt_start_date, unit = "week", week_start = 1)) %>%  
  mutate(week_start = if_else(week_start > max_week, NA_Date_, week_start))

household_data <- household_data %>%
  mutate(week_enum = floor_date(hh_date, unit = "week", week_start = 1)) %>%  
  mutate(week_enum = if_else(week_enum > max_week, NA_Date_, week_enum))

# EA numbers removing any text -------------------------------

screening_data <- screening_data %>% 
  mutate(ea_id = str_sub(ea_number, 1, 8))

household_data <- household_data %>% 
  mutate(hh_ea_id = str_sub(hh_ea, 1, 8))

treatment_data <- treatment_data %>% 
  mutate(tpt_ea_id = str_sub(tpt_ea, 1, 8))

# Village from EA database to household --------------------------

household_data <- household_data %>%
  select(-any_of("hh_village_ea")) %>%
  left_join(
    ea_data %>% select(record_id, village),
    by = c("hh_ea_id" = "record_id")
  ) %>%
  rename(hh_village_ea = village)

# Village and EA from household database to screening -----------------------

screening_data <- screening_data %>%
  select(-any_of(c("res_village_hh", "ea_number_hh"))) %>%
  left_join(
    household_data %>% select(record_id, hh_ea_id, hh_village_ea),
    by = c("dwelling_id" = "record_id")
  ) %>%
  rename(
    ea_number_hh = hh_ea_id,
    res_village_hh = hh_village_ea
  )

# Pivot with count or sum for each field per household into the household dataset -----------------------------

# Aggregate screening data: count the number of screened individuals per dwelling_id
hh_pivot_reg <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_reg_new = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

# Aggregate screening data: count the number of individuals with TB decision per dwelling_id
hh_pivot_tbdec <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  filter(!is.na(tb_decision) & tb_decision != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_tbdec_new = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

hh_pivot <- full_join(hh_pivot_reg, hh_pivot_tbdec, by = "record_id")

household_data <- household_data %>%
  select(-any_of(c("hh_reg_new", "hh_tbdec_new"))) %>% 
  left_join(hh_pivot, by = "record_id")


# Pivot with count or sum for each field per EA into the EA dataset -----------------------------

# Aggregate screening data
ea_pivot_screen <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "") %>%
  group_by(ea_id) %>%
  summarise(
    pop_reg_screen_new = n(),
    date_screen_new = min(en_date_visit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(record_id = ea_id)

ea_pivot_screen_mf <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "" & en_sex %in% c("M", "F")) %>%
  mutate(en_sex = tolower(en_sex)) %>%
  group_by(ea_id, en_sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = en_sex, values_from = count, names_prefix = "pop_reg_screen_") %>%
  mutate(
    pop_reg_screen_m = replace_na(pop_reg_screen_m, 0),
    pop_reg_screen_f = replace_na(pop_reg_screen_f, 0)
  ) %>%
  rename(
    record_id = ea_id,
    pop_reg_screen_m_new = pop_reg_screen_m,
    pop_reg_screen_f_new = pop_reg_screen_f
  )

# Aggregate household data
ea_pivot_hh <- household_data %>%
  filter(!is.na(hh_ea_id) & hh_ea_id != "") %>%
  group_by(hh_ea_id) %>%
  summarise(
    pop_all_new = sum(hh_all, na.rm = TRUE),
    pop_current_new = sum(hh_size, na.rm = TRUE),
    pop_elig_new = sum(hh_size_elig, na.rm = TRUE),
    pop_reg_enum_new = sum(hh_reg, na.rm = TRUE),
    hh_enum_new = n(),
    date_enum_new = min(hh_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(record_id = hh_ea_id)

ea_pivot <- reduce(c(list(ea_pivot_screen, ea_pivot_screen_mf, ea_pivot_hh)), full_join, by = "record_id")

ea_data <- ea_data %>%
  select(-any_of(c("pop_reg_screen_new",
                   "pop_reg_screen_m_new",
                   "pop_reg_screen_f_new",
                   "pop_all_new",
                   "pop_current_new",
                   "pop_elig_new",
                   "pop_reg_enum_new",
                   "hh_enum_new",
                   "date_enum_new",
                   "date_screen_new"))) %>%
  left_join(ea_pivot, by = "record_id")

# EA level aggregation columns ------------------------------------

## Create proportion columns in EA data, round to 2dp ---------------------------

ea_data <- ea_data %>%
  mutate(prop_reg = round(ifelse(pop_elig_new == 0, NA, pop_reg_enum_new / pop_elig_new), 2))

# Daily data aggregation columns -------------------------------------

## Initial daily data with n reg --------------------------------

# Get the date range and fill
start_date <- min(screening_data$en_date_visit, na.rm = TRUE)
end_date <- Sys.Date()
full_dates <- tibble(en_date_visit = seq.Date(start_date, end_date, by = "day"))

# Aggregate screening counts by day
daily_counts <- screening_data %>%
  group_by(en_date_visit) %>%
  summarise(reg = n(), .groups = "drop")

# Aggregate TST successfully placed counts by day
tst_counts <- screening_data %>%
  filter(tst_success == "Yes") %>%
  group_by(en_date_visit) %>%
  summarise(tst_placed = n(), .groups = "drop")

# Aggregate TST successfully read counts by day
tst_read_counts <- screening_data %>%
  filter(tst_read_bin == TRUE) %>%
  group_by(en_date_visit) %>%
  summarise(tst_read = n(), .groups = "drop")

# Merge with the full date sequence and fill missing days with 0 counts
daily_data <- full_dates %>%
  left_join(daily_counts, by = "en_date_visit") %>%
  left_join(tst_counts, by = "en_date_visit") %>%
  left_join(tst_read_counts, by = "en_date_visit") %>%
  replace_na(list(n = 0))

# Create additional time groupings
daily_data <- daily_data %>%
  mutate(
    week = lubridate::floor_date(en_date_visit, "week"),
    month = lubridate::floor_date(en_date_visit, "month"),
    quarter = lubridate::floor_date(en_date_visit, "quarter"),
    year = lubridate::floor_date(en_date_visit, "year")
  )

