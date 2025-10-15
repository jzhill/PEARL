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
library(sf)

# Parameters ----------------------------------------

min_week <- floor_date(min(screening_data$en_date_visit, na.rm = TRUE), unit = "week", week_start = 1)
min_month <- floor_date(min(screening_data$en_date_visit, na.rm = TRUE), unit = "month")
max_week <- floor_date(max(screening_data$en_date_visit, na.rm = TRUE), unit = "week", week_start = 1)
max_month <- floor_date(max(screening_data$en_date_visit, na.rm = TRUE), unit = "month")
current_date <- format(Sys.Date(), "%Y-%m-%d")

# Helper binary columns creation ----------------------

xr_result_values   <- c("1 cw TB","2 CAD 50 plus","3 Uncertain","4 Unlikely","5 CAD under 50")
nlp_outcome_values <- c("Confirmed","Ruled out","Already on MDT")
ntp_outcome_values <- c("Confirmed","Ruled out","Currently on TB treatment")

screening_data <- screening_data %>%
  mutate(
    # TST read done
    tst_read_bin = !is.na(tst_read_mm) | !is.na(tst_read_positive),
    
    # TB decision recorded (any value present)
    tbdec_bin = !is.na(tb_decision),
    
    # Leprosy “decision” recorded (here: referral field present)
    lepdec_bin = !is.na(lep_refer),
    
    # XR result assigned (numbered options 1–5)
    xr_resulted = calc_xr %in% xr_result_values,
    
    # Referrals (explicit logicals, never NA)
    referred_ntp = coalesce(tb_decision == "Presumptive TB", FALSE),
    referred_nlp = coalesce(lep_refer, FALSE),
    
    # Outcome recorded flags (only if referred)
    nlp_outcome = referred_nlp &
      (nlp_diagnosis %in% nlp_outcome_values | !is.na(exit_reason_screen)),
    ntp_outcome = referred_ntp &
      (ntp_diagnosis %in% ntp_outcome_values | !is.na(exit_reason_screen))
  )

# Age category in 10 year groups -------------------------------

screening_data <- screening_data %>%
  mutate(age_cat = age_categories(en_cal_age, by = 10, upper = 80))

treatment_data <- treatment_data %>%
  mutate(age_cat = age_categories(tpt_age, by = 10, upper = 80))

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
  mutate(week_outcome = if_else(week_outcome > max_week, NA_Date_, week_outcome))

household_data <- household_data %>%
  mutate(week_enum = floor_date(hh_date, unit = "week", week_start = 1)) %>%  
  mutate(week_enum = if_else(week_enum > max_week, NA_Date_, week_enum))

# Months for all dates ---------------------------------
# Use this for all future monthly data analysis

screening_data <- screening_data %>%
  mutate(month_reg = floor_date(en_date_visit, unit = "month")) %>%
  mutate(month_reg = if_else(month_reg > max_month, NA_Date_, month_reg))

treatment_data <- treatment_data %>%
  mutate(month_reg   = floor_date(tpt_reg_date,   unit = "month")) %>%
  mutate(month_reg   = if_else(month_reg > max_month, NA_Date_, month_reg)) %>%
  mutate(month_start = floor_date(tpt_start_date, unit = "month")) %>%
  mutate(month_start = if_else(month_start > max_month, NA_Date_, month_start)) %>%
  mutate(month_outcome = floor_date(tpt_outcome_date, unit = "month")) %>%
  mutate(month_outcome = if_else(month_outcome > max_month, NA_Date_, month_outcome))

household_data <- household_data %>%
  mutate(month_enum = floor_date(hh_date, unit = "month")) %>%
  mutate(month_enum = if_else(month_enum > max_month, NA_Date_, month_enum))

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
# Coalesce will ensure we use the household EA first, and fill the captured EA only if household is empty
# Likewise for village

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
    prop_reg_enum = round(ifelse(pop_elig_new == 0, NA, pop_reg_enum_new / pop_elig_new), 2) %>% replace_na(NA_real_),
    prop_reg_screen = round(ifelse(pop_elig_new == 0, NA, pop_reg_screen_new / pop_elig_new), 2) %>% replace_na(NA_real_),
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

# ---- Treatment end date, duration, expected visits, forms done --------

treatment_data <- treatment_data %>%
  # ensure key date fields are Date (if they might be character, uncomment ymd())
  # mutate(
  #   across(c(tpt_start_date, tpt_outcome_date, tpt_1m_date, tpt_3m_date, tpt_4m_date), ymd)
  # ) %>%
  mutate(
    # Candidate end dates: outcome date; OR the date of review that completed TPT
    tpt_3m_complete_date = if_else(tpt_3m_outcome == "Complete TPT", tpt_3m_date, as.Date(NA)),
    tpt_4m_complete_date = if_else(tpt_4m_outcome == "Complete TPT", tpt_4m_date, as.Date(NA)),
    
    # Cleaned end date = earliest of available candidates
    tpt_end_date = pmin(tpt_outcome_date, tpt_3m_complete_date, tpt_4m_complete_date, na.rm = TRUE),
    tpt_end_date = if_else(is.infinite(tpt_end_date), as.Date(NA), tpt_end_date),
    
    # Duration in days (integer); NA if start or end missing
    tpt_dur = as.integer(difftime(tpt_end_date, tpt_start_date, units = "days")),
    
    # EXPECTED routine reviews based on duration achieved
    # Assumption: thresholds reflect approx 1, 3, 4 months = 21, 77, 105 days respectively.
    tpt_1m_expected = !is.na(tpt_dur) & tpt_dur >= 21,
    tpt_3m_expected = !is.na(tpt_dur) & tpt_dur >= 77,
    tpt_4m_expected = !is.na(tpt_dur) & tpt_dur >= 105,
    
    # DONE flags: consider the review done if the corresponding date is present
    tpt_1m_done = !is.na(tpt_1m_date),
    tpt_3m_done = !is.na(tpt_3m_date),
    tpt_4m_done = !is.na(tpt_4m_date)
  )

# ---- Side-effect analysis columns ------------------------------

timepoints <- c("1m", "3m", "4m", "ae")
groups     <- c("dili_sx", "rhs_sx", "csx")

# 1) Long table of side-effect checkbox values (logical)
sx_long <- treatment_data %>%
  select(record_id,
         matches(paste0("^tpt_(", paste(timepoints, collapse="|"), ")_(",
                        paste(groups, collapse="|"), ")_.+$"))) %>%
  pivot_longer(
    cols = -record_id,
    names_to = c("tp", "grp", "opt"),
    names_pattern = paste0("^tpt_(", paste(timepoints, collapse="|"),
                           ")_(", paste(groups, collapse="|"), ")_(.+)$"),
    values_to = "val"
  ) %>%
  mutate(val = as.logical(val))

# (1) per timepoint × group: ANY TRUE among non-"none" options
# (2) per timepoint × group: ALL FALSE across *all* options (incl. "none")
#     - TRUE when true_cnt == 0 and at least one FALSE observed
#     - NA when every option at that tp+grp is NA (rare; included for completeness)

sx_tp_grp <- sx_long %>%
  group_by(record_id, tp, grp) %>%
  summarise(
    any_true_non_none = any(val[opt != "none"], na.rm = TRUE),
    true_cnt_all = sum(val %in% TRUE,  na.rm = TRUE),
    false_cnt_all = sum(val %in% FALSE, na.rm = TRUE),
    non_na_all = sum(!is.na(val)),
    .groups = "drop"
  ) %>%
  mutate(
    all_false = case_when(
      non_na_all == 0 ~ NA,     # no info at all (unlikely w/ REDCap exports)
      true_cnt_all == 0 & false_cnt_all > 0 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Wide columns for (1) and (2)
sx_tp_grp_any_true_wide <- sx_tp_grp %>%
  transmute(record_id,
            name = paste0(tp, "_", grp, "_any_true"),
            value = any_true_non_none) %>%
  pivot_wider(names_from = name, values_from = value)

sx_tp_grp_all_false_wide <- sx_tp_grp %>%
  transmute(record_id,
            name = paste0(tp, "_", grp, "_all_false"),
            value = all_false) %>%
  pivot_wider(names_from = name, values_from = value)

# (3) per option across timepoints: *_ever (exclude "none")

sx_ever_wide <- sx_long %>%
  filter(opt != "none") %>%
  group_by(record_id, grp, opt) %>%
  summarise(any_true = any(val, na.rm = TRUE), .groups = "drop") %>%
  transmute(record_id, name = paste0(grp, "_", opt, "_ever"), value = any_true) %>%
  pivot_wider(names_from = name, values_from = value)

# (4) per group across timepoints: any side-effect ever (exclude "none")

sx_grp_any_ever_wide <- sx_long %>%
  filter(opt != "none") %>%
  group_by(record_id, grp) %>%
  summarise(any_true = any(val, na.rm = TRUE), .groups = "drop") %>%
  transmute(record_id, name = paste0(grp, "_any_ever"), value = any_true) %>%
  pivot_wider(names_from = name, values_from = value)

# (5) per timepoint (across *all* groups): any side-effect true at that tp (exclude "none")

sx_tp_any_true_wide <- sx_long %>%
  filter(opt != "none") %>%
  group_by(record_id, tp) %>%
  summarise(any_true = any(val, na.rm = TRUE), .groups = "drop") %>%
  transmute(record_id, name = paste0(tp, "_any_true"), value = any_true) %>%
  pivot_wider(names_from = name, values_from = value)

# (6) overall ever: any side-effect TRUE at any timepoint & any group (exclude "none")

sx_any_ever_wide <- sx_long %>%
  filter(opt != "none") %>%
  group_by(record_id) %>%
  summarise(sx_any_ever = any(val, na.rm = TRUE), .groups = "drop")

# Attach everything to treatment_data

treatment_data <- treatment_data %>%
  left_join(sx_tp_grp_any_true_wide, by = "record_id") %>%
  left_join(sx_tp_grp_all_false_wide, by = "record_id") %>%
  left_join(sx_ever_wide, by = "record_id") %>%
  left_join(sx_grp_any_ever_wide, by = "record_id") %>%
  left_join(sx_tp_any_true_wide, by = "record_id") %>%
  left_join(sx_any_ever_wide, by = "record_id")


# GIS data tidy ----------------------------------------------------------------------

## Join EA data to EA layer -------------------------------------------------------

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

## Clean household coordinates -------------------------------------------------

# Define office location as sf point (in EPSG:4326 first)
office_point <- st_sfc(
  st_point(c(172.943185325, 1.352152047)),
  crs = 4326
) %>% st_transform(crs = 3832)

# Create spatial object of GPS coordinates only
hh_gps_sf <- household_data %>%
  mutate(
    gps_lat  = as.numeric(hh_latitude),
    gps_long = as.numeric(hh_longitude)
  ) %>%
  filter(!is.na(gps_lat), !is.na(gps_long)) %>%
  st_as_sf(coords = c("gps_long", "gps_lat"), crs = 4326) %>%
  st_transform(crs = 3832)

# Flag whether the GPS record is valid or not
hh_gps_sf <- hh_gps_sf %>%
  mutate(
    dist_to_office = as.numeric(st_distance(geometry, office_point)),
    near_office    = dist_to_office < 200,
    inside_ea      = lengths(st_within(geometry, layer_betio_ea_3832)) > 0,
    gps_valid      = !near_office & inside_ea
  )

# Prepare a plain data frame of the flags
gps_flags_df <- hh_gps_sf %>%
  st_drop_geometry() %>%
  transmute(record_id, gps_valid = as.logical(gps_valid))

# Join back to household_data using record_id (IDEMPOTENT)
household_data <- household_data %>%
  # drop previously-derived cols to avoid .x/.y suffixing on re-runs
  select(-any_of(c("gps_valid", "lat", "long", "coord_source"))) %>%
  left_join(gps_flags_df, by = "record_id") %>%
  mutate(
    # ensure gps_valid exists even if hh_gps_sf had 0 rows
    gps_valid = coalesce(gps_valid, FALSE),
    
    # coerce possible character exports to numeric
    hh_latitude    = as.numeric(hh_latitude),
    hh_longitude   = as.numeric(hh_longitude),
    hh_census_lat  = as.numeric(hh_census_lat),
    hh_census_long = as.numeric(hh_census_long),
    
    # prefer census coords; otherwise use gps only if valid
    lat  = coalesce(hh_census_lat,  if_else(gps_valid, hh_latitude,  NA_real_)),
    long = coalesce(hh_census_long, if_else(gps_valid, hh_longitude, NA_real_)),
    
    coord_source = case_when(
      !is.na(hh_census_lat) & !is.na(hh_census_long) ~ "census",
      gps_valid                                       ~ "gps",
      TRUE                                            ~ "none"
    )
  )

## Create hh layer -----------------------------------------------------------------

layer_hh_betio_3832 <- household_data %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(layer_betio_ea_3832))

## Join clean coordinates to screening data for mapping --------------------------------------

screening_data <- screening_data %>%
  left_join(
    household_data %>% select(record_id, lat, long, coord_source),
    by = c("dwelling_id" = "record_id")
  )


# --- Unifying metrics builder for tables and time aggregation --------------

build_time_agg <- function(screening_data, household_data, treatment_data,
                           freq = c("week","month"), week_start = 1,
                           min_date = NULL, max_date = NULL) {
  freq <- match.arg(freq)
  
  # key columns by frequency
  key_col <- if (freq == "week") "week_reg"     else "month_reg"
  key_hh  <- if (freq == "week") "week_enum"    else "month_enum"
  key_ts  <- if (freq == "week") "week_start"   else "month_start"
  key_to  <- if (freq == "week") "week_outcome" else "month_outcome"
  
  # bounds
  min_key <- if (freq == "week") min(screening_data$week_reg,  na.rm = TRUE) else min(screening_data$month_reg,  na.rm = TRUE)
  max_key <- if (freq == "week") max(screening_data$week_reg,  na.rm = TRUE) else max(screening_data$month_reg,  na.rm = TRUE)
  min_key <- if (is.null(min_date)) min_key else as.Date(min_date)
  max_key <- if (is.null(max_date)) max_key else as.Date(max_date)
  
  # full sequence
  full_seq <- if (freq == "week") {
    seq.Date(from = floor_date(min_key, "week", week_start = week_start),
             to   = floor_date(max_key, "week", week_start = week_start),
             by   = "week")
  } else {
    seq.Date(from = floor_date(min_key, "month"),
             to   = floor_date(max_key, "month"),
             by   = "month")
  }
  
  # Check fields are logical, should be from loading and tidying already
  stopifnot(
    all(vapply(
      screening_data[c("tst_read_bin","calc_xr_elig","tbdec_bin","spuxpt_labreq_lab",
                       "lep_refer","lepdec_bin","xr_resulted","referred_ntp",
                       "referred_nlp","nlp_outcome","ntp_outcome")],
      is.logical, logical(1)
    ))
  )
  
  ## Enumeration (households) -------------------------
  
  hh_counts <- household_data %>%
    drop_na(all_of(key_hh)) %>%
    count(.data[[key_hh]], name = "hh_enum") %>%
    transmute(period_start = .data[[key_hh]], hh_enum = hh_enum) %>%
    complete(period_start = full_seq, fill = list(hh_enum = 0))
  
  ## Screening (activity & pathway) ---------------------------------
  
  # One grouped summarise over screening_data: all core counts, percentages,
  # households reached, referrals, follow-up outcomes, XR resulted.
  scr_core <- screening_data %>%
    filter(.data[[key_col]] <= max_key) %>%
    group_by(period_start = .data[[key_col]]) %>%
    summarise(
      # activity
      reg        = n(),
      tst_placed = sum(tst_success == "Yes", na.rm = TRUE),
      tst_read   = sum(tst_read_bin,       na.rm = TRUE),
      cxr_elig   = sum(calc_xr_elig,       na.rm = TRUE),
      cxr_done   = sum(cxr_done   == "Yes", na.rm = TRUE),
      cxr_result = sum(xr_resulted,        na.rm = TRUE),
      tbdec      = sum(tbdec_bin,          na.rm = TRUE),
      anyrx      = sum(!is.na(calc_any_treatment), na.rm = TRUE),
      xpert      = sum(spuxpt_labreq_lab,  na.rm = TRUE),
      
      # households reached
      households_reached = n_distinct(dwelling_name),
      
      # referrals
      ref_ntp    = sum(referred_ntp, na.rm = TRUE),
      ref_nlp    = sum(referred_nlp, na.rm = TRUE),
      ref_hbv    = sum(prerx_hbv_1 == "Positive", na.rm = TRUE),
      
      # follow-up outcomes
      nlp_outcome_recorded = sum(nlp_outcome, na.rm = TRUE),
      ntp_outcome_recorded = sum(ntp_outcome, na.rm = TRUE),
      
      # percentages (use the scalars defined above)
      tst_place_pct = if_else(reg        > 0, tst_placed / reg        * 100, NA_real_),
      tst_read_pct  = if_else(tst_placed > 0, tst_read   / tst_placed * 100, NA_real_),
      tbdec_pct     = if_else(reg        > 0, tbdec      / reg        * 100, NA_real_),
      anyrx_pct     = if_else(reg        > 0, anyrx      / reg        * 100, NA_real_),
      xpert_pct     = if_else(reg        > 0, xpert      / reg        * 100, NA_real_),
      cxr_pct       = if_else(cxr_elig   > 0, cxr_done   / cxr_elig   * 100, NA_real_),
      
      # new percentages
      cxr_res_pct   = if_else(cxr_done   > 0, cxr_result / cxr_done   * 100, NA_real_),
      ntp_out_pct   = if_else(ref_ntp    > 0, ntp_outcome_recorded / ref_ntp * 100, NA_real_),
      nlp_out_pct   = if_else(ref_nlp    > 0, nlp_outcome_recorded / ref_nlp * 100, NA_real_),
      
      .groups = "drop"
    ) %>%
    complete(period_start = full_seq)
  
  ## Screening results (distributions) -------------------------------
  
  tb_dist <- screening_data %>%
    mutate(tb_decision = fct_explicit_na(factor(tb_decision), na_level = "Missing")) %>%
    count(.data[[key_col]], tb_decision, name = "n") %>%
    mutate(period_start = .data[[key_col]]) %>%
    select(-all_of(key_col)) %>%
    pivot_wider(names_from = tb_decision, values_from = n, values_fill = 0) %>%
    rename(tbdec_prestb = `Presumptive TB`,
           tbdec_ro     = `Ruled out TB`,
           tbdec_unc    = `TB status uncertain`,
           tbdec_missing= `Missing`) %>%
    complete(period_start = full_seq,
             fill = list(tbdec_prestb = 0, tbdec_ro = 0, tbdec_unc = 0, tbdec_missing = 0))
  
  tst_dist <- screening_data %>%
    mutate(tst_read_positive = fct_explicit_na(factor(tst_read_positive), na_level = "Missing")) %>%
    count(.data[[key_col]], tst_read_positive, name = "n") %>%
    mutate(period_start = .data[[key_col]]) %>%
    select(-all_of(key_col)) %>%
    pivot_wider(names_from = tst_read_positive, values_from = n, values_fill = 0) %>%
    rename(tst_neg = `Negative TST`,
           tst_pos = `Positive TST`,
           tst_missing = `Missing`) %>%
    complete(period_start = full_seq,
             fill = list(tst_neg = 0, tst_pos = 0, tst_missing = 0))
  
  ## Treatment assessment pathway ----------------
  
  tpt_ax <- screening_data %>%
    filter(.data[[key_col]] <= max_key) %>%
    group_by(period_start = .data[[key_col]]) %>%
    summarise(
      
      # Who should be assessed for TPT?
      tpt_should_assess = sum(
        (tst_read_positive == "Positive TST") &
          (tb_decision == "Ruled out TB" | ntp_diagnosis == "Ruled out"),
        na.rm = TRUE
      ),
      
      # Who completed TPT assessment?
      tpt_assessment_done = sum(
        (prerx_eligible %in% c("Yes","No")) |
          (exit_reason_screen == "TPT - withdraw consent and prefer not to continue"),
        na.rm = TRUE
      ),
      
      # Helper denominators
      tpt_eligible_n = sum(prerx_eligible == "Yes", na.rm = TRUE),
      tpt_started_n  = sum(prerx_start == TRUE,     na.rm = TRUE),
      
      # Percentages
      tpt_assessed_of_should_pct = if_else(
        tpt_should_assess > 0,
        100 * tpt_assessment_done / tpt_should_assess,
        NA_real_
      ),
      
      tpt_started_of_eligible_pct = if_else(
        tpt_eligible_n > 0,
        100 * sum(prerx_start == TRUE, na.rm = TRUE) / tpt_eligible_n,
        NA_real_
      ),
      
      tpt_eligible_of_started_pct = if_else(
        tpt_started_n > 0,
        100 * sum(prerx_eligible == "Yes" & prerx_start == TRUE, na.rm = TRUE) / tpt_started_n,
        NA_real_
      ),
      
      .groups = "drop"
    ) %>%
    complete(period_start = full_seq)
  
  ## Treatment (starts & completions) ----------------
  
  tpt_rx <- treatment_data %>%
    mutate(period_start = .data[[key_ts]]) %>%
    drop_na(period_start) %>%
    group_by(period_start) %>%
    summarise(
      tpt_start               = n(),
      tpt_outcome_assigned    = sum(!is.na(tpt_outcome_reason), na.rm = TRUE),
      tpt_completed           = sum(tpt_outcome_reason == "Completed", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(tibble(period_start = full_seq), by = "period_start") %>%
    mutate(
      across(c(tpt_start, tpt_outcome_assigned, tpt_completed), ~ replace_na(., 0)),
      tpt_outcome_assigned_pct = if_else(tpt_start > 0, 100 * tpt_outcome_assigned / tpt_start, NA_real_)
    )
  
  ## Assemble & variants ----------------------------
  
  out <- scr_core %>%
    left_join(hh_counts, by = "period_start") %>%
    left_join(tpt_ax, by = "period_start") %>%
    left_join(tb_dist, by = "period_start") %>%
    left_join(tst_dist, by = "period_start") %>%
    left_join(tpt_rx, by = "period_start") %>%
    mutate(period_start = as.Date(period_start)) %>%
    mutate(across(
      c(reg, tst_placed, tst_read, cxr_elig, cxr_done, cxr_result, tbdec, anyrx, xpert,
        households_reached, ref_ntp, ref_nlp, ref_hbv,
        nlp_outcome_recorded, ntp_outcome_recorded,
        hh_enum, tpt_start, tpt_completed, tpt_outcome_assigned,
        tbdec_prestb, tbdec_ro, tbdec_unc, tbdec_missing,
        tst_neg, tst_pos, tst_missing, tpt_should_assess, tpt_assessment_done),
      ~ replace_na(., 0)
    ))
  
  out_na <- out %>%
    mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .)))
  
  out_long <- out %>%
    select(
      period_start,
      # phases: activity & treatment alongside %
      reg, households_reached,
      tpt_start, tpt_completed, tpt_outcome_assigned,
      tst_place_pct, tst_read_pct, tbdec_pct, anyrx_pct, xpert_pct, cxr_pct,
      cxr_res_pct, ntp_out_pct, nlp_out_pct, 
      # TPT pathway indicators
      tpt_should_assess, tpt_assessment_done,
      tpt_assessed_of_should_pct, tpt_started_of_eligible_pct, tpt_eligible_of_started_pct,
      # counts & distributions
      tst_placed, tst_read, cxr_elig, cxr_done, cxr_result,
      tbdec, anyrx, xpert,
      tbdec_prestb, tbdec_ro, tbdec_unc, tbdec_missing,
      tst_neg, tst_pos, tst_missing,
      # referrals & follow-up
      ref_ntp, ref_nlp, ref_hbv,
      nlp_outcome_recorded, ntp_outcome_recorded,
      # households
      hh_enum
    ) %>%
    pivot_longer(-period_start, names_to = "Indicator", values_to = "Value")
  
  list(data = out, data_na = out_na, data_long = out_long)
}

# --- Build both series -----------------------------------------------------

weekly <- build_time_agg(screening_data, household_data, treatment_data, freq = "week",  week_start = 1)
weekly_data <- weekly$data
weekly_data_na <- weekly$data_na
weekly_long <- weekly$data_long

monthly <- build_time_agg(screening_data, household_data, treatment_data, freq = "month")
monthly_data <- monthly$data
monthly_data_na <- monthly$data_na
monthly_long <- monthly$data_long
