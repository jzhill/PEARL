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

# Create helper column for TB outcome
screening_data <- screening_data %>%
  mutate(tbdec_bin = ifelse(!is.na(tb_decision), TRUE, FALSE))

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

## Weekly Aggregation ------------------------------------------------------

# Aggregate all required counts at the weekly level
weekly_data <- screening_data %>%
  mutate(
    tst_placed = ifelse(tst_success == "Yes", 1, 0),
    tst_read = ifelse(tst_read_bin == TRUE, 1, 0),
    tbdec = ifelse(tbdec_bin == TRUE, 1, 0),
    sdr = ifelse(calc_sdr == "Given", 1, 0)
  ) %>%
  group_by(week_reg) %>%
  summarise(
    reg = n(),
    tst_placed = sum(tst_placed, na.rm = TRUE),
    tst_read = sum(tst_read, na.rm = TRUE),
    tbdec = sum(tbdec, na.rm = TRUE),
    sdr = sum(sdr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(week_reg = seq.Date(
    from = min(screening_data$week_reg, na.rm = TRUE),
    to = max(screening_data$week_reg, na.rm = TRUE),
    by = "week"
  ), fill = list(reg = 0, tst_placed = 0, tst_read = 0, tbdec = 0, sdr = 0)) %>%
  mutate(
    month = lubridate::floor_date(week_reg, "month"),
    quarter = lubridate::floor_date(week_reg, "quarter"),
    year = lubridate::floor_date(week_reg, "year")
  )

# Aggregate Presumptive TB counts by week_reg, ensuring factor labels are retained
tb_decision_counts <- screening_data %>%
  mutate(
    tb_decision = as.character(tb_decision),  # Convert factor to character to retain labels
    tb_decision = ifelse(is.na(tb_decision), "Missing", tb_decision) # Label NA as "Missing"
  ) %>%
  count(week_reg, tb_decision) %>%
  pivot_wider(names_from = tb_decision, values_from = n, values_fill = list(n = 0)) %>%
  rename_with(~paste0("tbdec_", make.names(.)), -week_reg) # Ensure column names are readable

# Aggregate TST Read Positive counts by week_reg, ensuring factor labels are retained
tst_read_positive_counts <- screening_data %>%
  mutate(
    tst_read_positive = as.character(tst_read_positive), # Convert factor to character to retain labels
    tst_read_positive = ifelse(is.na(tst_read_positive), "Missing", tst_read_positive) # Label NA as "Missing"
  ) %>%
  count(week_reg, tst_read_positive) %>%
  pivot_wider(names_from = tst_read_positive, values_from = n, values_fill = list(n = 0)) %>%
  rename_with(~paste0("tst_", make.names(.)), -week_reg) # Ensure column names are readable

# Merge with existing weekly_data
weekly_data <- weekly_data %>%
  left_join(tb_decision_counts, by = "week_reg") %>%
  left_join(tst_read_positive_counts, by = "week_reg")
