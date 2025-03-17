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

# Parameters ----------------------------------------

min_week <- floor_date(min(screening_data$en_date_visit, na.rm = TRUE), unit = "week", week_start = 1)
max_week <- floor_date(max(screening_data$en_date_visit, na.rm = TRUE), unit = "week", week_start = 1)
current_week <- floor_date(Sys.Date(), "week", week_start = 1)
current_date <- format(Sys.Date(), "%Y-%m-%d")

# Screening data checking and consolidating columns ----------------------

# Create helper binary column for TST read done, regardless of result
screening_data <- screening_data %>%
  mutate(tst_read_bin = ifelse(!is.na(tst_read_mm) | !is.na(tst_read_positive), TRUE, FALSE))

# Create helper binary column for TB outcome done, regardless of result
screening_data <- screening_data %>%
  mutate(tbdec_bin = ifelse(!is.na(tb_decision), TRUE, FALSE))

# Age category in 10 year groups -------------------------------

screening_data <- screening_data %>%
  mutate(age_cat = epikit::age_categories(en_cal_age, by = 10, upper = 80))

treatment_data <- treatment_data %>%
  mutate(age_cat = epikit::age_categories(tpt_age, by = 10, upper = 80))

# Weeks for all dates ---------------------------------
# Use this for all future weekly data analysis

screening_data <- screening_data %>%
  mutate(week_reg = floor_date(en_date_visit, unit = "week", week_start = 1)) %>%
  mutate(week_reg = if_else(week_reg > max_week, NA_Date_, week_reg))

treatment_data <- treatment_data %>%
  mutate(week_reg = floor_date(tpt_reg_date, unit = "week", week_start = 1)) %>%  
  mutate(week_reg = if_else(week_reg > max_week, NA_Date_, week_reg)) %>% 
  mutate(week_start = floor_date(tpt_start_date, unit = "week", week_start = 1)) %>%  
  mutate(week_start = if_else(week_start > max_week, NA_Date_, week_start)) %>% 
  mutate(week_outcome = floor_date(tpt_outcome_date, unit = "week", week_start = 1)) %>%  
  mutate(week_outcome = if_else(week_start > max_week, NA_Date_, week_outcome))

household_data <- household_data %>%
  mutate(week_enum = floor_date(hh_date, unit = "week", week_start = 1)) %>%  
  mutate(week_enum = if_else(week_enum > max_week, NA_Date_, week_enum))

# EA numbers removing any text -------------------------------
# Use this for all future EA data analysis

screening_data <- screening_data %>% 
  mutate(ea_id = str_sub(ea_number, 1, 8))

household_data <- household_data %>% 
  mutate(hh_ea_id = str_sub(hh_ea, 1, 8))

treatment_data <- treatment_data %>% 
  mutate(tpt_ea_id = str_sub(tpt_ea, 1, 8))

# Lookup village for each household from EA data --------------------------

household_data <- household_data %>%
  select(-any_of("hh_village_ea")) %>%
  left_join(
    ea_data %>% select(record_id, village),
    by = c("hh_ea_id" = "record_id")
  ) %>%
  rename(hh_village_ea = village)

# Lookup village and EA for each participant from household database -----------------------
# NOTE: village and EA data capture in screening data is unreliable

screening_data <- screening_data %>%
  select(-any_of(c("res_village_hh", "ea_number_hh"))) %>%
  left_join(
    household_data %>% select(record_id, hh_ea_id, hh_village_ea),
    by = c("dwelling_id" = "record_id")
  ) %>%
  rename(
    ea_number_hh = hh_ea_id,
    res_village_hh = hh_village_ea
  ) %>%
  mutate(
    ea = coalesce(ea_number_hh, ea_id),
    village = coalesce(res_village_hh, res_village)
  )

# Household level aggregation ------------------------------------

## Pivot with count or sum for each field per household into the household dataset -----------------------------

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


# EA level aggregation ------------------------------------

## Pivot with count or sum for each field per EA into the EA dataset -----------------------------

# Aggregate screening data
ea_pivot_screen <- screening_data %>%
  filter(!is.na(ea) & ea != "") %>%
  group_by(ea) %>%
  summarise(
    pop_reg_screen_new = n(),
    date_screen_new = min(en_date_visit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(record_id = ea)

ea_pivot_screen_mf <- screening_data %>%
  filter(!is.na(ea) & ea != "" & en_sex %in% c("M", "F")) %>%
  mutate(en_sex = tolower(en_sex)) %>%
  group_by(ea, en_sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = en_sex, values_from = count, names_prefix = "pop_reg_screen_") %>%
  mutate(
    pop_reg_screen_m = replace_na(pop_reg_screen_m, 0),
    pop_reg_screen_f = replace_na(pop_reg_screen_f, 0)
  ) %>%
  rename(
    record_id = ea,
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

## Create proportion columns in EA data, round to 2dp ---------------------------

# Note denominator is eligible population
ea_data <- ea_data %>%
  mutate(
    prop_reg = round(ifelse(pop_elig_new == 0, NA, pop_reg_enum_new / pop_elig_new), 2) %>% replace_na(NA_real_)
  )

# Weekly Aggregation ------------------------------------------------------

## Aggregate ---------------------------------

# Aggregate all required counts at the weekly level
weekly_data <- screening_data %>%
  filter(week_reg <= max_week) %>%  # drop future weeks
  group_by(week_reg) %>%
  summarise(
    reg = n(),
    tst_placed = sum(as.integer(tst_success == "Yes"), na.rm = TRUE),
    tst_read = sum(as.integer(tst_read_bin), na.rm = TRUE),
    cxr_elig = sum(as.integer(calc_xr_elig), na.rm = TRUE),
    cxr_done = sum(as.integer(cxr_done == "Yes"), na.rm = TRUE),
    tbdec = sum(as.integer(tbdec_bin), na.rm = TRUE),
    anyrx = sum(!is.na(calc_any_treatment), na.rm = TRUE),
    xpert = sum(as.integer(spuxpt_labreq_lab), na.rm = TRUE),
    tst_place_pct = if_else(sum(reg, na.rm = TRUE) > 0, sum(tst_placed, na.rm = TRUE) / sum(reg, na.rm = TRUE) * 100, NA_real_),
    tst_read_pct = if_else(sum(tst_placed, na.rm = TRUE) > 0, sum(tst_read, na.rm = TRUE) / sum(tst_placed, na.rm = TRUE) * 100, NA_real_),
    tbdec_pct = if_else(sum(reg, na.rm = TRUE) > 0, sum(tbdec, na.rm = TRUE) / sum(reg, na.rm = TRUE) * 100, NA_real_),
    anyrx_pct = if_else(sum(reg, na.rm = TRUE) > 0, sum(anyrx, na.rm = TRUE) / sum(reg, na.rm = TRUE) * 100, NA_real_),
    xpert_pct = if_else(sum(reg, na.rm = TRUE) > 0, sum(xpert, na.rm = TRUE) / sum(reg, na.rm = TRUE) * 100, NA_real_),
    cxr_pct = if_else(sum(cxr_elig, na.rm = TRUE) > 0, sum(cxr_done, na.rm = TRUE) / sum(cxr_elig, na.rm = TRUE) * 100, NA_real_),
    .groups = "drop"
  ) %>%
  complete(
    week_reg = seq.Date(from = min_week, to = max_week, by = "week")  # Ensure all weeks exist
  )

# Aggregate Household Data using week_enum
household_weekly <- household_data %>%
  drop_na(week_enum) %>%
  count(week_enum, name = "hh_enum") %>%
  rename(week_reg = week_enum)

# Aggregate Treatment Data using week_start
treatment_weekly <- treatment_data %>%
  drop_na(week_start) %>%
  count(week_start, name = "tpt_start") %>%
  rename(week_reg = week_start)  # Rename for consistency

# Aggregate Presumptive TB counts by week_reg, ensuring factor labels are retained
tb_decision_counts <- screening_data %>%
  mutate(tb_decision = fct_explicit_na(factor(tb_decision), na_level = "Missing")) %>%
  count(week_reg, tb_decision) %>%
  pivot_wider(names_from = tb_decision, values_from = n, values_fill = list(n = 0)) %>%
  rename_with(~paste0("tbdec_", make.names(.)), -week_reg) # Ensure column names are readable

# Aggregate TST Read Positive counts by week_reg, ensuring factor labels are retained
tst_read_positive_counts <- screening_data %>%
  mutate(tst_read_positive = fct_explicit_na(factor(tst_read_positive), na_level = "Missing")) %>%
  count(week_reg, tst_read_positive) %>%
  pivot_wider(names_from = tst_read_positive, values_from = n, values_fill = list(n = 0)) %>%
  rename_with(~paste0("tst_", make.names(.)), -week_reg) # Ensure column names are readable

## Join and complete ---------------------------

weekly_data <- weekly_data %>%
  left_join(household_weekly, by = "week_reg") %>%
  left_join(treatment_weekly, by = "week_reg") %>%
  left_join(tb_decision_counts, by = "week_reg") %>%
  left_join(tst_read_positive_counts, by = "week_reg") %>%
  mutate(week_reg = as.Date(week_reg))%>%
  mutate(across(
    c(reg, tst_placed, tst_read, cxr_elig, cxr_done, tbdec, anyrx, xpert, hh_enum, tpt_start),
    ~ replace_na(., 0)
  ))

## Additional processing ------------------------

# Replace with NA for plotting
weekly_data_na <- weekly_data %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .))) %>%
  mutate(week_reg = as.Date(week_reg))

# Prepare data for plotting
weekly_long <- weekly_data %>%
  select(week_reg, reg, tpt_start, tst_place_pct, tst_read_pct, tbdec_pct, anyrx_pct, xpert_pct, cxr_pct) %>%  # Ensure only needed columns
  pivot_longer(
    cols = -week_reg,
    names_to = "Indicator",
    values_to = "Value"
  )

# Village level aggregation ----------------------------

## Village data ---------------------------------

# Aggregate data from ea_data
village_data <- ea_data %>%
  group_by(village) %>%
  summarise(
    pop_2020 = sum(pop_2020, na.rm = TRUE),
    hh_2020 = sum(hh_2020, na.rm = TRUE)
  ) %>%
  ungroup()

# Aggregate data from household_data
village_data_hh <- household_data %>%
  group_by(hh_village_ea) %>%
  summarise(
    hh_enum = n(),
    pop_elig = sum(hh_size_elig, na.rm = TRUE),
    pop_enum = sum(hh_size, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  rename(village = hh_village_ea)

# Aggregate data from screening_data
village_data_screen <- screening_data %>%
  group_by(village) %>%
  summarise(
    pop_reg = n(),
    date_started = min(en_date_visit, na.rm = TRUE)
  ) %>%
  ungroup()

# Merge datasets using left_join to retain all villages from ea_data
village_data <- village_data %>%
  select(-any_of(c("hh_enum", "pop_elig", "pop_enum", "pop_reg"))) %>% 
  left_join(village_data_hh, by = "village") %>%
  left_join(village_data_screen, by = "village")

# Create columns for village proportions
village_data <- village_data %>% 
  mutate(prop_reg = round(ifelse(pop_2020 == 0, NA, pop_reg / pop_2020), 2)) %>%  # Note denominator is 2020 population
  mutate(prop_hh_enum = round(ifelse(hh_2020 == 0, NA, hh_enum / hh_2020), 2)) %>%   # Note denominator is 2020 n households
  mutate(prop_reg_elig = round(ifelse(pop_elig == 0, NA, pop_reg / pop_elig), 2))   # Note denominator is 2020 n households
  
# Create variable for village ordered by date_started
village_order <- village_data %>%
  arrange(date_started) %>%
  pull(village)

# Convert village to a factor with levels in order of first reached
village_data <- village_data %>%
  mutate(village = factor(village, levels = village_order))


## Cumulative village data ---------------------------------

# Identify villages with a maximum cumulative total >= 100
villages_gte_100 <- village_data %>%
  filter(pop_reg >= 100) %>%
  pull(village)

# Define overall date range based on week_reg in screening_data
week_range_complete <- seq.Date(
  from = min(screening_data$week_reg, na.rm = TRUE),
  to = max(screening_data$week_reg, na.rm = TRUE),
  by = "week"
)

# Calculate cumulative screened counts by village and week from screening_data
village_data_cum <- screening_data %>%
  mutate(
    village = if_else(is.na(village) | village == "" | !(village %in% villages_gte_100),
    "Other or unknown", village)
  ) %>%
  filter(!is.na(week_reg)) %>%
  group_by(village, week_reg) %>%
  summarise(n_screened = n(), .groups = "drop") %>%
  arrange(village, week_reg) %>%
  complete(village, week_reg = week_range_complete, fill = list(n_screened = 0)) %>% # Fill missing weeks first
  group_by(village) %>%
  mutate(cum_screened = cumsum(n_screened)) %>%
  ungroup()

# Join village population and calculate cumulative proportion safely
# Note that we know the 2020 population for all villages
# The eligible village population is only known AFTER completing the village
# Proportions for villages in progress are not correct!
village_data_cum <- village_data_cum %>%
  left_join(village_data, by = "village") %>%
  mutate(
    pop_2020 = ifelse(is.na(pop_2020) | pop_2020 == 0, NA, pop_2020),
    cum_prop = ifelse(is.na(pop_2020), NA, cum_screened / pop_2020),  # Denominator is 2020 population
    pop_elig = ifelse(is.na(pop_elig) | pop_elig == 0, NA, pop_elig),
    cum_prop_elig = ifelse(is.na(pop_elig), NA, cum_screened / pop_elig)  # Denominator is eligible population
  )

# Determine first week each village was reached
village_order_cum <- village_data_cum %>%
  filter(cum_screened > 0) %>%
  group_by(village) %>%
  summarise(first_week = min(week_reg, na.rm = TRUE), .groups = "drop") %>%
  arrange(first_week) %>%
  pull(village)

# Ensure 'Other or unknown' is always the first level
village_order_cum <- c(setdiff(village_order_cum, "Other or unknown"), "Other or unknown")

# Convert village to a factor with levels in order of first reached
village_data_cum <- village_data_cum %>%
  mutate(village = factor(village, levels = village_order_cum))

# GIS data tidy -------------------------

## Join EA data to EA layer -------------------------------

# Ensure both columns have the same type for joining
layer_ki_ea_3832$ea_2020 <- as.character(layer_ki_ea_3832$ea_2020)
ea_data$record_id <- as.character(ea_data$record_id)

# Remove previously joined columns, then join
layer_ki_ea_3832 <- layer_ki_ea_3832 %>%
  select(-all_of(grep("^joined_", names(.), value = TRUE))) %>%  # Remove previous joins
  left_join(ea_data, by = c("ea_2020" = "record_id")) %>%  # Perform join
  rename_with(~ paste0("joined_", .), .cols = any_of(setdiff(names(ea_data), "record_id")))  # Rename only newly added columns

# Filter to only include EAs with vid of 716
layer_betio_ea_3832 <- layer_ki_ea_3832 %>%
  filter(vid == 716)

# Metrics for tables ----------------------------

## Headline numbers for current week and total -------------------------------------

# Function to compute weekly and total metrics
compute_metrics <- function(data, week_col = NULL, filter_expr = NULL, distinct_col = NULL) {
  
  # Apply weekly filter if week_col is provided
  if (!is.null(week_col)) {
    data_week <- data %>% filter(.data[[week_col]] == current_week)
  } else {
    data_week <- data
  }
  
  # Apply distinct filtering if distinct_col is provided
  if (!is.null(distinct_col)) {
    week_count <- data_week %>% distinct(.data[[distinct_col]]) %>% nrow()
    total_count <- data %>% distinct(.data[[distinct_col]]) %>% nrow()
  } else if (!is.null(filter_expr)) {  # Apply additional filter condition
    week_count <- data_week %>% filter(eval(filter_expr, data_week)) %>% nrow()
    total_count <- data %>% filter(eval(filter_expr, data)) %>% nrow()
  } else {  # If no filter condition, just count rows
    week_count <- nrow(data_week)
    total_count <- nrow(data)
  }
  
  return(list(week = week_count, total = total_count))
}

# Compute indicators for current week and total
indicators <- list(
  "Households Enumerated" = compute_metrics(household_data, week_col = "week_enum"),
  "Households Reached" = compute_metrics(screening_data, week_col = "week_reg", distinct_col = "dwelling_name"),
  "People Registered" = compute_metrics(screening_data, week_col = "week_reg"),
  "TSTs Completed" = compute_metrics(screening_data, week_col = "week_reg", filter_expr = expression(tst_read_bin == TRUE)),
  "Referred to NTP" = compute_metrics(screening_data, week_col = "week_reg", filter_expr = expression(tb_decision == "Presumptive TB")),
  "Referred to NLP" = compute_metrics(screening_data, week_col = "week_reg", filter_expr = expression(lep_refer == TRUE)),
  "Referred to Hep B" = compute_metrics(screening_data, week_col = "week_reg", filter_expr = expression(prerx_hbv_1 == "Positive")),
  "X-Rays Performed" = compute_metrics(screening_data, week_col = "week_reg", filter_expr = expression(cxr_done == "Yes")),
  "Xpert Tests Done" = compute_metrics(screening_data, week_col = "week_reg", filter_expr = expression(spuxpt_labreq_lab == TRUE)),
  "Started on TPT" = compute_metrics(treatment_data, week_col = "week_start"),
  "Completed TPT" = compute_metrics(treatment_data, week_col = "week_outcome", filter_expr = expression(tpt_outcome_reason == "Completed"))
)

