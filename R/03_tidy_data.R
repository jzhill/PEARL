# Title and description --------------------------------------------

# Cleaning, tidying and transforming PEARL data ready for analysis
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   23 Feb 2025


# Packages -----------------------------------------

library(here)
library(glue)
library(stringr)
library(lubridate)
library(tidyverse)
library(epikit)
library(dplyr)
library(purrr)
library(sf)
library(qs)

# Parameters ----------------------------------------

min_week <- floor_date(min(screening_data$en_date_visit, na.rm = TRUE), unit = "week", week_start = 1)
min_month <- floor_date(min(screening_data$en_date_visit, na.rm = TRUE), unit = "month")
max_week <- floor_date(max(screening_data$en_date_visit, na.rm = TRUE), unit = "week", week_start = 1)
max_month <- floor_date(max(screening_data$en_date_visit, na.rm = TRUE), unit = "month")
current_date <- format(Sys.Date(), "%Y-%m-%d")

# Household record flags -----------------

household_data <- household_data %>%
  mutate(
    hh_size = suppressWarnings(as.numeric(hh_size)),
    # Reached if status is enumerated, invited or refused or if status is blank or not visited, but hh_size is filled
    # Not include: "4 Removed", status blank or not visited, and hh_size not filled
    hh_reached = case_when(
      hh_status %in% c("2 Enumerated", "3 Invited", "5 Refused") ~ TRUE,
      hh_status == "1 Not visited" & !is.na(hh_size) & hh_size > 0 ~ TRUE,
      is.na(hh_status) & !is.na(hh_size) & hh_size > 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    hh_active = !(hh_status %in% c("4 Removed"))
  )

# Helper binary columns creation ----------------------

xr_result_values   <- c("1 cw TB","2 CAD 50 plus","3 Uncertain","4 Unlikely","5 CAD under 50")
nlp_outcome_values <- c("Confirmed","Ruled out","Already on MDT")
ntp_outcome_values <- c("Confirmed","Ruled out","Currently on TB treatment")

screening_data <- screening_data %>%
  mutate(
    
    # TST placed (Yes = TRUE; all other values OR NA = FALSE)
    tst_placed_bin = case_when(
      tst_success == "Yes" ~ TRUE,
      TRUE                 ~ FALSE
    ),
    
    # TST read done
    tst_read_bin = !is.na(tst_read_mm) | !is.na(tst_read_positive),
    
    # TST ≥10 mm: TRUE/FALSE, but NA if no reading
    tst_10mm_bin = case_when(
      is.na(tst_read_mm)    ~ NA,           # no reading
      tst_read_mm >= 10     ~ TRUE,
      tst_read_mm < 10      ~ FALSE
    ),
    
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

# Xpert availability flag ----------------------------------------------

screening_data <- screening_data %>%
  mutate(
    xpert_available = case_when(
      # Missing visit date → keep as NA
      is.na(en_date_visit) ~ NA, 
      
      # Windows when Xpert was NOT available
      en_date_visit >= as.Date("2023-04-17") & en_date_visit <= as.Date("2023-04-30") ~ FALSE,
      en_date_visit >= as.Date("2023-06-26") & en_date_visit <= as.Date("2023-08-06") ~ FALSE,
      en_date_visit >= as.Date("2023-08-14") & en_date_visit <= as.Date("2023-08-20") ~ FALSE,
      en_date_visit >= as.Date("2024-10-21") & en_date_visit <= as.Date("2024-12-15") ~ FALSE,
      en_date_visit >= as.Date("2025-01-20") & en_date_visit <= as.Date("2025-02-09") ~ FALSE,
      en_date_visit >= as.Date("2025-04-14") & en_date_visit <= as.Date("2025-07-20") ~ FALSE,
      en_date_visit >= as.Date("2025-07-28") & en_date_visit <= as.Date("2025-11-30") ~ FALSE,
      
      # All other dates → Xpert available
      TRUE ~ TRUE
    )
  )

# TB clinical / infectiousness flags -----------------------------------------

screening_data <- screening_data %>%
  mutate(
    # Confirmed TB flag (helper)
    tb_confirmed = ntp_diagnosis == "Confirmed",
    
    # Clinical vs subclinical among confirmed TB
    # TRUE  = clinical (symptomatic)
    # FALSE = subclinical (asymptomatic)
    # NA    = not confirmed TB or missing sx summary
    tb_clinical_bin = case_when(
      tb_confirmed & calc_sx == "Sx Positive" ~ TRUE,
      tb_confirmed & calc_sx == "Sx Negative" ~ FALSE,
      tb_confirmed                            ~ NA,   # confirmed but sx summary missing/other
      TRUE                                    ~ NA    # not confirmed TB
    ),
    
    # Xpert positive (excluding trace) – helper
    xpert_pos_ex_trace = spuxpt_mtb_res_lab == "Detected" &
      spuxpt_lab_det_level %in% c("High", "Medium", "Low", "Very low"),
    
    # New TB case – helper:
    #   - confirmed TB
    #   - no previous TB recorded
    #   - no previous register number recorded
    new_tb_case = case_when(
      tb_confirmed & prev_tb == FALSE & is.na(ntp_prev_reg) ~ TRUE,
      tb_confirmed                                         ~ FALSE,
      TRUE                                                 ~ FALSE
    ),
    
    # XR consistent with TB – helper
    xr_consistent_tb = calc_xr == "1 cw TB",
    
    # More vs less infectious among confirmed TB:
    # TRUE  = Xpert pos (excl trace) OR (new case AND XR cw TB)
    # FALSE = other confirmed TB
    # NA    = not confirmed TB
    tb_moreinf_bin = case_when(
      tb_confirmed & (xpert_pos_ex_trace | (new_tb_case & xr_consistent_tb)) ~ TRUE,
      tb_confirmed                                                           ~ FALSE,
      TRUE                                                                   ~ NA
    )
  )


# Age category in 10 year groups -------------------------------

screening_data <- screening_data %>%
  mutate(age_cat = age_categories(en_cal_age, by = 10, upper = 80))

treatment_data <- treatment_data %>%
  mutate(age_cat = age_categories(tpt_age, by = 10, upper = 80))

# Age categories for modelling ------------------------------------------------

screening_data <- screening_data %>%
  mutate(
    age_cat_model = case_when(
      is.na(en_cal_age)                 ~ NA_character_,
      en_cal_age >=   0 & en_cal_age <=  2 ~ "0-2",
      en_cal_age >=   3 & en_cal_age <=  9 ~ "3-9",
      en_cal_age >=  10 & en_cal_age <= 14 ~ "10-14",
      en_cal_age >=  15 & en_cal_age <= 64 ~ "15-64",
      en_cal_age >=  65                  ~ "65+",
      TRUE ~ NA_character_
    ),
    age_cat_model = factor(
      age_cat_model,
      levels = c("0-2", "3-9", "10-14", "15-64", "65+"),
      ordered = TRUE
    )
  )

treatment_data <- treatment_data %>%
  mutate(
    age_cat_model = case_when(
      is.na(tpt_age)                   ~ NA_character_,
      tpt_age >=   0 & tpt_age <=  2 ~ "0-2",
      tpt_age >=   3 & tpt_age <=  9 ~ "3-9",
      tpt_age >=  10 & tpt_age <= 14 ~ "10-14",
      tpt_age >=  15 & tpt_age <= 64 ~ "15-64",
      tpt_age >=  65                ~ "65+",
      TRUE ~ NA_character_
    ),
    age_cat_model = factor(
      age_cat_model,
      levels = c("0-2", "3-9", "10-14", "15-64", "65+"),
      ordered = TRUE
    )
  )
 


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


# ---- TPT risk assessment flags (aligned with calc_tptrfs) --------------------

# Expected levels of screening_data$calc_tptrfs:
# "01 ALT done - HIGH RISK"
# "02 ALT done - MODERATE HIGH RISK"
# "03 ALT done - MODERATE RISK"
# "04 ALT not needed, no RFs - low risk"
# "05 RFs present, baseline ALT needed - cannot assess TPT risk until done"
# "06 RFs not complete"

# canonical labels we’ll use for comparison
risk_levels <- c("High","Moderate high","Moderate","Low")

screening_data <- screening_data %>%
  mutate(
    calc_tptrfs_chr = str_squish(as.character(calc_tptrfs)),
    prerx_riskcat_chr = str_squish(as.character(prerx_riskcat)),
    
    # A) risk factors assessed: anything except explicit "not complete"
    tptrf_assessed = !is.na(calc_tptrfs_chr) & calc_tptrfs_chr != "06 RFs not complete",
    
    # B) baseline ALT needed: codes 01/02/03/05 from calc_tptrfs
    tptrf_alt_needed = str_detect(calc_tptrfs_chr, "^(01|02|03|05)\\b"),
    
    # C) baseline ALT requested: blood collected?
    tptrf_alt_requested = coalesce(as.logical(prerx_lft_1), FALSE),
    
    # D) baseline ALT result reported: have a value for most recent ALT
    tptrf_alt_result = !is.na(suppressWarnings(as.numeric(calc_alt_last))),
    
    # Expected risk level from calc_tptrfs code (01–04 only)
    tptrf_expected_level = case_when(
      str_detect(calc_tptrfs_chr, "^01\\b") ~ "High",
      str_detect(calc_tptrfs_chr, "^02\\b") ~ "Moderate high",
      str_detect(calc_tptrfs_chr, "^03\\b") ~ "Moderate",
      str_detect(calc_tptrfs_chr, "^04\\b") ~ "Low",
      TRUE ~ NA_character_
    ),
    
    # E) risk group assigned? (= present and not "Not yet known")
    tptrf_risk_assigned = !is.na(prerx_riskcat_chr) & prerx_riskcat_chr != "Not yet known",
    
    # Normalize assigned level to the same canonical set; NA if "Not yet known"
    tptrf_assigned_level = case_when(
      prerx_riskcat_chr %in% risk_levels ~ prerx_riskcat_chr,
      TRUE ~ NA_character_
    ),
    
    # F) assigned level equals expected level (only compare when both present)
    tptrf_risk_as_expected = !is.na(tptrf_expected_level) &
      !is.na(tptrf_assigned_level) &
      tptrf_expected_level == tptrf_assigned_level
  )

# ---- Treatment end date, duration, expected visits, forms done --------

treatment_data <- treatment_data %>%
  mutate(
    # Dates that can constitute a REAL end (i.e., course actually ended)
    tpt_3m_complete_date = if_else(tpt_3m_outcome == "Complete TPT", tpt_3m_date, as.Date(NA)),
    tpt_4m_complete_date = if_else(tpt_4m_outcome == "Complete TPT", tpt_4m_date, as.Date(NA)),
    
    # --- REAL end date: earliest of outcome/complete dates; NA if none exist
    .tpt_end_real_num = do.call(pmin, c(
      list(tpt_outcome_date, tpt_3m_complete_date, tpt_4m_complete_date),
      list(na.rm = TRUE)
    )),
    tpt_end_date_real = as.Date(ifelse(is.infinite(.tpt_end_real_num), NA, .tpt_end_real_num), origin = "1970-01-01"),
    
    # --- PROVISIONAL end date: real end if known, else today()
    tpt_end_date_prov = coalesce(tpt_end_date_real, today()),
    
    # Durations
    tpt_dur_real = as.integer(difftime(tpt_end_date_real, tpt_start_date, units = "days")),  # NA if still active
    tpt_dur_prov = as.integer(difftime(tpt_end_date_prov, tpt_start_date, units = "days")),
    
    # EXPECTED reviews are based on PROVISIONAL duration (i.e., progress to date)
    # thresholds ≈ 1, 3, 4 months
    tpt_1m_expected = !is.na(tpt_dur_prov) & tpt_dur_prov >= 21,
    tpt_3m_expected = !is.na(tpt_dur_prov) & tpt_dur_prov >= 77,
    tpt_4m_expected = !is.na(tpt_dur_prov) & tpt_dur_prov >= 105,
    
    # DONE flags (as recorded)
    tpt_1m_done = !is.na(tpt_1m_date),
    tpt_3m_done = !is.na(tpt_3m_date),
    tpt_4m_done = !is.na(tpt_4m_date)
  ) %>%
  select(-.tpt_end_real_num)

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

# Attach everything to treatment_data, *idempotently* --------------------

# Collect the names of all derived side-effect columns we are about to join
sx_derived_cols <- c(
  setdiff(names(sx_tp_grp_any_true_wide), "record_id"),
  setdiff(names(sx_tp_grp_all_false_wide), "record_id"),
  setdiff(names(sx_ever_wide),             "record_id"),
  setdiff(names(sx_grp_any_ever_wide),     "record_id"),
  setdiff(names(sx_tp_any_true_wide),      "record_id"),
  setdiff(names(sx_any_ever_wide),         "record_id")
) %>% unique()

treatment_data <- treatment_data %>%
  # 1) drop any previous side-effect derived columns
  select(-any_of(sx_derived_cols)) %>%
  # 2) re-attach fresh ones
  left_join(sx_tp_grp_any_true_wide, by = "record_id") %>%
  left_join(sx_tp_grp_all_false_wide, by = "record_id") %>%
  left_join(sx_ever_wide,             by = "record_id") %>%
  left_join(sx_grp_any_ever_wide,     by = "record_id") %>%
  left_join(sx_tp_any_true_wide,      by = "record_id") %>%
  left_join(sx_any_ever_wide,         by = "record_id")


# Metrics aggregated across time and geography --------------

## Helper functions for each group of metrics -----------------

# SCREENING: core counts + percentages by .key
screen_core_by_key <- function(df_keyed) {
  df_keyed %>%
    group_by(.key) %>%
    summarise(
      
      # date first screening recorded
      first_screen_date = suppressWarnings(min(en_date_visit, na.rm = TRUE)),
      
      # activity
      reg        = n(),
      reg_m      = sum(en_sex == "M", na.rm = TRUE),
      reg_f      = sum(en_sex == "F", na.rm = TRUE),
      tst_placed = sum(tst_success == "Yes", na.rm = TRUE),
      tst_read   = sum(tst_read_bin,        na.rm = TRUE),
      cxr_elig   = sum(calc_xr_elig,        na.rm = TRUE),
      cxr_done   = sum(cxr_done == "Yes",   na.rm = TRUE),
      cxr_result = sum(xr_resulted,         na.rm = TRUE),
      tbdec      = sum(tbdec_bin,           na.rm = TRUE),
      anyrx      = sum(!is.na(calc_any_treatment), na.rm = TRUE),
      xpert      = sum(spuxpt_labreq_lab,   na.rm = TRUE),
      
      # households screened = unique dwellings recorded by screening teams
      hh_screened = n_distinct(dwelling_name, na.rm = TRUE),
      
      # referrals & outcomes
      ref_ntp    = sum(referred_ntp, na.rm = TRUE),
      ref_nlp    = sum(referred_nlp, na.rm = TRUE),
      ref_hbv    = sum(prerx_hbv_1 == "Positive", na.rm = TRUE),
      nlp_outcome_recorded = sum(nlp_outcome, na.rm = TRUE),
      ntp_outcome_recorded = sum(ntp_outcome, na.rm = TRUE),
      
      # percentages
      tst_place_pct = if_else(reg        > 0, 100 * tst_placed / reg,        NA_real_),
      tst_read_pct  = if_else(tst_placed > 0, 100 * tst_read   / tst_placed, NA_real_),
      tbdec_pct     = if_else(reg        > 0, 100 * tbdec      / reg,        NA_real_),
      anyrx_pct     = if_else(reg        > 0, 100 * anyrx      / reg,        NA_real_),
      xpert_pct     = if_else(reg        > 0, 100 * xpert      / reg,        NA_real_),
      cxr_pct       = if_else(cxr_elig   > 0, 100 * cxr_done   / cxr_elig,   NA_real_),
      cxr_res_pct   = if_else(cxr_done   > 0, 100 * cxr_result / cxr_done,   NA_real_),
      ntp_out_pct   = if_else(ref_ntp    > 0, ntp_outcome_recorded / ref_ntp * 100, NA_real_),
      nlp_out_pct   = if_else(ref_nlp    > 0, nlp_outcome_recorded / ref_nlp * 100, NA_real_),
      .groups = "drop"
    )
}

# SCREENING: TB decision distribution by .key (wide)
tb_dist_by_key <- function(df_keyed) {
  df_keyed %>%
    mutate(tb_decision = fct_na_value_to_level(factor(tb_decision), "Missing")) %>%
    count(.key, tb_decision, name = "n") %>%
    pivot_wider(names_from = tb_decision, values_from = n, values_fill = 0) %>%
    rename(
      tbdec_prestb  = `Presumptive TB`,
      tbdec_ro      = `Ruled out TB`,
      tbdec_unc     = `TB status uncertain`,
      tbdec_missing = `Missing`
    )
}

# SCREENING: TST distribution by .key (wide)
tst_dist_by_key <- function(df_keyed) {
  df_keyed %>%
    mutate(tst_read_positive = fct_na_value_to_level(factor(tst_read_positive), "Missing")) %>%
    count(.key, tst_read_positive, name = "n") %>%
    pivot_wider(names_from = tst_read_positive, values_from = n, values_fill = 0) %>%
    rename(
      tst_neg     = `Negative TST`,
      tst_pos     = `Positive TST`,
      tst_missing = `Missing`
    )
}

# SCREENING: TPT assessment pathway counts/percents by .key
tpt_assess_by_key <- function(df_keyed) {
  df_keyed %>%
    group_by(.key) %>%
    summarise(
      tpt_should_assess = sum(
        (tst_read_positive == "Positive TST") &
          (tb_decision == "Ruled out TB" | ntp_diagnosis == "Ruled out"),
        na.rm = TRUE
      ),
      tpt_assessment_done = sum(
        (prerx_eligible %in% c("Yes","No")) |
          (exit_reason_screen == "TPT - withdraw consent and prefer not to continue"),
        na.rm = TRUE
      ),
      tpt_eligible_n = sum(prerx_eligible == "Yes", na.rm = TRUE),
      tpt_started_n  = sum(prerx_start == TRUE,     na.rm = TRUE),
      
      tpt_assessed_of_should_pct = if_else(
        tpt_should_assess > 0, 100 * tpt_assessment_done / tpt_should_assess, NA_real_
      ),
      tpt_started_of_eligible_pct = if_else(
        tpt_eligible_n > 0, 100 * sum(prerx_start == TRUE, na.rm = TRUE) / tpt_eligible_n, NA_real_
      ),
      tpt_eligible_of_started_pct = if_else(
        tpt_started_n  > 0, 100 * sum(prerx_eligible == "Yes" & prerx_start == TRUE, na.rm = TRUE) / tpt_started_n, NA_real_
      ),
      
      # risk assessment steps
      tptrf_assessed_n       = sum(tptrf_assessed,         na.rm = TRUE),
      tptrf_alt_needed_n     = sum(tptrf_alt_needed,       na.rm = TRUE),
      tptrf_alt_requested_n  = sum(tptrf_alt_requested,    na.rm = TRUE),
      tptrf_alt_result_n     = sum(tptrf_alt_result,       na.rm = TRUE),
      tptrf_risk_assigned_n  = sum(tptrf_risk_assigned,    na.rm = TRUE),
      tptrf_as_expected_n    = sum(tptrf_risk_as_expected, na.rm = TRUE),
      .groups = "drop"
    )
}

# TREATMENT: cohort metrics by .key (pre-keyed to start period or area)
tpt_cohort_by_key <- function(tpt_keyed) {
  tpt_keyed %>%
    group_by(.key) %>%
    summarise(
      tpt_start               = n(),
      tpt_outcome_assigned    = sum(!is.na(tpt_outcome_reason), na.rm = TRUE),
      tpt_completed           = sum(tpt_outcome_reason == "Completed", na.rm = TRUE),
      
      tpt_1m_expected         = sum(tpt_1m_expected, na.rm = TRUE),
      tpt_1m_done             = sum(tpt_1m_done,     na.rm = TRUE),
      tpt_3m_expected         = sum(tpt_3m_expected, na.rm = TRUE),
      tpt_3m_done             = sum(tpt_3m_done,     na.rm = TRUE),
      tpt_4m_expected         = sum(tpt_4m_expected, na.rm = TRUE),
      tpt_4m_done             = sum(tpt_4m_done,     na.rm = TRUE),
      
      tpt_outcome_assigned_pct = if_else(tpt_start > 0, 100 * tpt_outcome_assigned / tpt_start, NA_real_),
      tpt_completed_pct        = if_else(tpt_start > 0, 100 * tpt_completed        / tpt_start, NA_real_),
      tpt_1m_done_pct          = if_else(tpt_1m_expected > 0, 100 * tpt_1m_done / tpt_1m_expected, NA_real_),
      tpt_3m_done_pct          = if_else(tpt_3m_expected > 0, 100 * tpt_3m_done / tpt_3m_expected, NA_real_),
      tpt_4m_done_pct          = if_else(tpt_4m_expected > 0, 100 * tpt_4m_done / tpt_4m_expected, NA_real_),
      .groups = "drop"
    )
}

# HOUSEHOLDS: counts by .key (expects hh_reached filtered in caller or here)
hh_counts_by_key <- function(hh_keyed, require_reached = TRUE) {
  df <- if (require_reached) filter(hh_keyed, hh_reached) else hh_keyed
  df %>%
    count(.key, name = "hh_enum_new")
}

# HOUSEHOLDS: enumeration denominators by .key
hh_denoms_by_key <- function(hh_keyed) {
  hh_keyed %>%
    group_by(.key) %>%
    summarise(
      pop_all_new      = sum(hh_all,       na.rm = TRUE),
      pop_current_new  = sum(hh_size,      na.rm = TRUE),
      pop_elig_new     = sum(hh_size_elig, na.rm = TRUE),
      pop_reg_enum_new = sum(hh_reg,       na.rm = TRUE),
      date_enum_new    = suppressWarnings(min(hh_date, na.rm = TRUE)),
      .groups = "drop"
    )
}

# EA-level census denominators: straight from ea_data
census_denoms_ea <- function(ea_data) {
  ea_data %>%
    transmute(
      .key            = record_id,
      pop_2023_ex02,
      pop_2023_m_ex02,
      pop_2023_f_ex02,
      hh_2023
    )
}

# Village-level census denominators: sum over EAs within each village
census_denoms_village <- function(ea_data) {
  ea_data %>%
    group_by(village) %>%
    summarise(
      pop_2023_ex02   = sum(pop_2023_ex02,   na.rm = TRUE),
      pop_2023_m_ex02 = sum(pop_2023_m_ex02, na.rm = TRUE),
      pop_2023_f_ex02 = sum(pop_2023_f_ex02, na.rm = TRUE),
      hh_2023         = sum(hh_2023,         na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(.key = village)
}

# Utility: zero-fill selected pure-count columns, keep % as NA
zero_fill_counts <- function(df, cols) {
  df %>% mutate(across(all_of(cols), ~ replace_na(., 0)))
}

## Aggregation functions for time and area, using helper metric functions ------------------

build_time_agg <- function(screening_data, household_data, treatment_data,
                           freq = c("week","month"), week_start = 1,
                           min_date = NULL, max_date = NULL) {
  freq <- match.arg(freq)
  
  # keys by frequency
  key_col <- if (freq == "week") "week_reg"     else "month_reg"
  key_hh  <- if (freq == "week") "week_enum"    else "month_enum"
  key_ts  <- if (freq == "week") "week_start"   else "month_start"
  
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
  
  # Safety: logical columns present
  stopifnot(
    all(vapply(
      screening_data[c("tst_read_bin","calc_xr_elig","tbdec_bin","spuxpt_labreq_lab",
                       "lep_refer","lepdec_bin","xr_resulted","referred_ntp",
                       "referred_nlp","nlp_outcome","ntp_outcome")],
      is.logical, logical(1)
    ))
  )
  
  # Standardise to ".key"
  scr_keyed <- screening_data %>%
    filter(.data[[key_col]] <= max_key) %>%
    mutate(.key = .data[[key_col]])
  
  hh_keyed <- household_data %>%
    filter(!is.na(.data[[key_hh]])) %>%
    mutate(.key = .data[[key_hh]])
  
  tpt_keyed <- treatment_data %>%
    filter(!is.na(.data[[key_ts]])) %>%
    mutate(.key = .data[[key_ts]])
  
  # Build via helpers
  scr_core <- screen_core_by_key(scr_keyed) %>%
    complete(.key = full_seq)
  
  tb_dist  <- tb_dist_by_key(scr_keyed) %>%
    complete(.key = full_seq,
             fill = list(tbdec_prestb = 0, tbdec_ro = 0, tbdec_unc = 0, tbdec_missing = 0))
  
  tst_dist <- tst_dist_by_key(scr_keyed) %>%
    complete(.key = full_seq,
             fill = list(tst_neg = 0, tst_pos = 0, tst_missing = 0))
  
  tpt_ax   <- tpt_assess_by_key(scr_keyed) %>%
    complete(.key = full_seq)
  
  tpt_rx   <- tpt_cohort_by_key(tpt_keyed) %>%
    complete(.key = full_seq,
             fill = list(
               tpt_start = 0, tpt_outcome_assigned = 0, tpt_completed = 0,
               tpt_1m_expected = 0, tpt_1m_done = 0,
               tpt_3m_expected = 0, tpt_3m_done = 0,
               tpt_4m_expected = 0, tpt_4m_done = 0
             ))
  
  hh_counts <- hh_counts_by_key(hh_keyed) %>%
    complete(.key = full_seq, fill = list(hh_enum_new = 0))
  
  # Assemble
  out <- scr_core %>%
    left_join(hh_counts, by = ".key") %>%
    left_join(tpt_ax,    by = ".key") %>%
    left_join(tb_dist,   by = ".key") %>%
    left_join(tst_dist,  by = ".key") %>%
    left_join(tpt_rx,    by = ".key") %>%
    rename(period_start = .key) %>%
    zero_fill_counts(c(
      "reg","reg_m","reg_f","tst_placed","tst_read","cxr_elig","cxr_done","cxr_result","tbdec","anyrx","xpert",
      "hh_screened","ref_ntp","ref_nlp","ref_hbv",
      "nlp_outcome_recorded","ntp_outcome_recorded","hh_enum_new",
      "tpt_start","tpt_completed","tpt_outcome_assigned",
      "tpt_1m_expected","tpt_1m_done","tpt_3m_expected","tpt_3m_done","tpt_4m_expected","tpt_4m_done",
      "tbdec_prestb","tbdec_ro","tbdec_unc","tbdec_missing",
      "tst_neg","tst_pos","tst_missing",
      "tptrf_assessed_n","tptrf_alt_needed_n","tptrf_alt_requested_n",
      "tptrf_alt_result_n","tptrf_risk_assigned_n","tptrf_as_expected_n"
    ))
  
  out_na <- out %>%
    mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .)))
  
  out_long <- out %>%
    select(
      period_start,
      # phases: activity & treatment alongside %
      reg, reg_m, reg_f, hh_screened,
      tpt_start, tpt_completed, tpt_outcome_assigned,
      tst_place_pct, tst_read_pct, tbdec_pct, anyrx_pct, xpert_pct, cxr_pct,
      cxr_res_pct, ntp_out_pct, nlp_out_pct, 
      # TPT pathway indicators
      tpt_should_assess, tpt_assessment_done,
      tpt_assessed_of_should_pct, tpt_started_of_eligible_pct, tpt_eligible_of_started_pct,
      # TPT risk indicators
      tptrf_assessed_n, tptrf_alt_needed_n, tptrf_alt_requested_n,
      tptrf_alt_result_n, tptrf_risk_assigned_n, tptrf_as_expected_n,
      # review counts & percentages
      tpt_1m_expected, tpt_1m_done, tpt_1m_done_pct,
      tpt_3m_expected, tpt_3m_done, tpt_3m_done_pct,
      tpt_4m_expected, tpt_4m_done, tpt_4m_done_pct,
      # treatment support percent
      tpt_outcome_assigned_pct, tpt_completed_pct,
      # counts & distributions
      tst_placed, tst_read, cxr_elig, cxr_done, cxr_result,
      tbdec, anyrx, xpert,
      tbdec_prestb, tbdec_ro, tbdec_unc, tbdec_missing,
      tst_neg, tst_pos, tst_missing,
      # referrals & follow-up
      ref_ntp, ref_nlp, ref_hbv,
      nlp_outcome_recorded, ntp_outcome_recorded,
      # households
      hh_enum_new
    ) %>%
    pivot_longer(-period_start, names_to = "Indicator", values_to = "Value")
  
  list(data = out, data_na = out_na, data_long = out_long)
}

build_area_agg <- function(screening_data, household_data, treatment_data,
                           level = c("ea","village")) {
  level <- match.arg(level)
  
  # Safety: logical columns present
  stopifnot(
    all(vapply(
      screening_data[c("tst_read_bin","calc_xr_elig","tbdec_bin","spuxpt_labreq_lab",
                       "lep_refer","lepdec_bin","xr_resulted","referred_ntp",
                       "referred_nlp","nlp_outcome","ntp_outcome")],
      is.logical, logical(1)
    ))
  )
  
  # Standardise to ".key"
  if (level == "ea") {
    scr_keyed <- screening_data %>%
      filter(!is.na(ea), ea != "") %>%
      mutate(.key = ea)
    
    hh_keyed <- household_data %>%
      filter(!is.na(hh_ea_id), hh_ea_id != "") %>%
      mutate(.key = hh_ea_id)
    
    tpt_keyed <- treatment_data %>%
      filter(!is.na(tpt_ea_id), tpt_ea_id != "") %>%
      mutate(.key = tpt_ea_id)
    
    den_census <- census_denoms_ea(ea_data)
    
  } else { # village
    scr_keyed <- screening_data %>%
      filter(!is.na(village), village != "") %>%
      mutate(.key = village)
    
    hh_keyed <- household_data %>%
      filter(!is.na(hh_village_ea), hh_village_ea != "") %>%
      mutate(.key = hh_village_ea)
    
    # Attach village to treatment via screening (record_id linkage)
    tpt_keyed <- treatment_data %>%
      left_join(screening_data %>% select(record_id, village), by = "record_id") %>%
      filter(!is.na(village), village != "") %>%
      mutate(.key = village)
    
    den_census <- census_denoms_village(ea_data)
  }
  
  # Build via helpers
  scr_core <- screen_core_by_key(scr_keyed)
  tb_dist  <- tb_dist_by_key(scr_keyed)
  tst_dist <- tst_dist_by_key(scr_keyed)
  tpt_ax   <- tpt_assess_by_key(scr_keyed)
  tpt_rx   <- tpt_cohort_by_key(tpt_keyed)
  hh_counts<- hh_counts_by_key(hh_keyed)
  hh_denoms <- hh_denoms_by_key(hh_keyed)
  
  # Assemble
  out <- den_census %>%
    full_join(hh_denoms, by = ".key") %>%
    full_join(scr_core,  by = ".key") %>%
    full_join(hh_counts, by = ".key") %>%
    full_join(tpt_ax,    by = ".key") %>%
    full_join(tb_dist,   by = ".key") %>%
    full_join(tst_dist,  by = ".key") %>%
    full_join(tpt_rx,    by = ".key") %>%
    arrange(.key) %>%
    rename(area = .key) %>%
    zero_fill_counts(c(
      # counts (screening)
      "hh_enum_new","reg","reg_m","reg_f","tst_placed","tst_read","cxr_elig",
      "cxr_done","cxr_result","tbdec","anyrx","xpert",
      "hh_screened","ref_ntp","ref_nlp","ref_hbv",
      "nlp_outcome_recorded","ntp_outcome_recorded",
      # TPT cohorts / reviews
      "tpt_start","tpt_completed","tpt_outcome_assigned",
      "tpt_1m_expected","tpt_1m_done",
      "tpt_3m_expected","tpt_3m_done",
      "tpt_4m_expected","tpt_4m_done",
      # TB / TST distributions
      "tbdec_prestb","tbdec_ro","tbdec_unc","tbdec_missing",
      "tst_neg","tst_pos","tst_missing",
      # risk pathway
      "tptrf_assessed_n","tptrf_alt_needed_n","tptrf_alt_requested_n",
      "tptrf_alt_result_n","tptrf_risk_assigned_n","tptrf_as_expected_n"
    )) %>%
    mutate(
      
      # Coverage indicators using new denominators
      # Census-based denominators: pop_2023_ex02 excludes 0–2y as per protocol
      
      prop_reg_2023_ex02 = if_else(
        !is.na(pop_2023_ex02) & pop_2023_ex02 > 0,
        reg / pop_2023_ex02,
        NA_real_
      ),
      prop_reg_enum_2023_ex02 = if_else(
        !is.na(pop_2023_ex02) & pop_2023_ex02 > 0,
        pop_reg_enum_new / pop_2023_ex02,
        NA_real_
      ),
      
      # Household-based denominators (eligibility from hh_size_elig)
      prop_reg_enum_hh = if_else(
        !is.na(pop_elig_new) & pop_elig_new > 0,
        pop_reg_enum_new / pop_elig_new,
        NA_real_
      ),
      prop_reg_screen_hh = if_else(
        !is.na(pop_elig_new) & pop_elig_new > 0,
        reg / pop_elig_new,
        NA_real_
      )
    )
  
  # Long format
  
  out_long <- out %>%
    select(area, where(is.numeric)) %>%   # <-- drop dates, characters, etc
    pivot_longer(
      -area,
      names_to  = "Indicator",
      values_to = "Value"
    )
  
  list(data = out, data_long = out_long, level = level)
}


# --- Build both series -----------------------------------------------------

# Build time aggregations

weekly <- build_time_agg(screening_data, household_data, treatment_data, freq = "week",  week_start = 1)
weekly_data <- weekly$data
weekly_data_na <- weekly$data_na
weekly_long <- weekly$data_long

monthly <- build_time_agg(screening_data, household_data, treatment_data, freq = "month")
monthly_data <- monthly$data
monthly_data_na <- monthly$data_na
monthly_long <- monthly$data_long

# Build EA aggregations

ea_agg      <- build_area_agg(screening_data, household_data, treatment_data, level = "ea")
ea_agg_wide <- ea_agg$data
ea_agg_long <- ea_agg$data_long

# Columns in ea_agg_wide that are *not* the key or census 2023 fields
ea_agg_cols <- setdiff(
  names(ea_agg_wide),
  c("area",               # key
    "pop_2023_ex02",      # census ex 0–2
    "pop_2023_m_ex02",
    "pop_2023_f_ex02",
    "hh_2023")
)

# Prepare agg data for join: drop overlapping census fields only
ea_agg_join <- ea_agg_wide %>%
  select(-any_of(c(
    "pop_2023_ex02", "pop_2023_m_ex02",
    "pop_2023_f_ex02", "hh_2023"
  )))

# Make ea_data idempotent: strip previous agg cols, then re-join fresh ones
ea_data <- ea_data %>%
  select(-any_of(ea_agg_cols)) %>%   # <- key step for idempotence
  left_join(
    ea_agg_join %>% rename(record_id = area),
    by = "record_id"
  )

# Build village aggregations

village_agg      <- build_area_agg(screening_data, household_data, treatment_data, level = "village")
village_agg_wide <- village_agg$data
village_agg_long <- village_agg$data_long

# Rename as village_data

village_data <- village_agg_wide %>%
  rename(village = area)

# --- Village ordering based on first_screen_date --------------------------

village_order <- village_data %>%
  arrange(first_screen_date) %>%
  filter(!is.na(first_screen_date)) %>%
  pull(village)

village_data <- village_data %>%
  mutate(village = factor(village, levels = village_order))

## Cumulative village data (using 2023 ex 0–2 denominators) ----------------

# Villages with total registrations >= 100 (canonical reg from agg)
villages_gte_100 <- village_data %>%
  filter(reg >= 100) %>%
  pull(village) %>%
  as.character()

# Overall date range based on week_reg in screening_data
week_range_complete <- seq.Date(
  from = min(screening_data$week_reg, na.rm = TRUE),
  to   = max(screening_data$week_reg, na.rm = TRUE),
  by   = "week"
)

# Cumulative screened counts by village and week
village_data_cum <- screening_data %>%
  mutate(
    village = if_else(
      is.na(village) | village == "" | !(village %in% villages_gte_100),
      "Other or unknown",
      as.character(village)
    )
  ) %>%
  filter(!is.na(week_reg)) %>%
  group_by(village, week_reg) %>%
  summarise(n_screened = n(), .groups = "drop") %>%
  arrange(village, week_reg) %>%
  complete(
    village,
    week_reg = week_range_complete,
    fill = list(n_screened = 0)
  ) %>%
  group_by(village) %>%
  mutate(cum_screened = cumsum(n_screened)) %>%
  ungroup()

# Join village_data for denominators and calculate cumulative proportions
# Using 2023 ex 0–2 pop as the main denominator

village_data_cum <- village_data_cum %>%
  left_join(village_data, by = "village") %>%
  mutate(
    # 2023 ex-0–2 census denominator
    pop_2023_ex02 = ifelse(is.na(pop_2023_ex02) | pop_2023_ex02 == 0, NA, pop_2023_ex02),
    cum_prop_2023_ex02 = ifelse(
      is.na(pop_2023_ex02),
      NA,
      cum_screened / pop_2023_ex02
    ),
    
    # Programme eligible denominator from HH aggregation
    pop_elig_new = ifelse(is.na(pop_elig_new) | pop_elig_new == 0, NA, pop_elig_new),
    cum_prop_elig_new = ifelse(
      is.na(pop_elig_new),
      NA,
      cum_screened / pop_elig_new
    )
  )

# Determine first week each village was reached
village_order_cum <- village_data_cum %>%
  filter(cum_screened > 0) %>%
  group_by(village) %>%
  summarise(first_week = min(week_reg, na.rm = TRUE), .groups = "drop") %>%
  arrange(first_week) %>%
  pull(village)

# Ensure 'Other or unknown' is always the first level
village_order_cum <- c(setdiff(village_order_cum, "Other or unknown"),
                       "Other or unknown")

village_data_cum <- village_data_cum %>%
  mutate(village = factor(village, levels = village_order_cum))

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
  select(-any_of(c("lat", "long", "coord_source"))) %>%
  left_join(
    household_data %>% select(record_id, lat, long, coord_source),
    by = c("dwelling_id" = "record_id")
  )

# Persist a versioned analysis bundle for downstream scripts -----------------

# Data bundle paths ---------------------------------------------------------

# Use a dated bundle plus a rolling "latest" pointer so downstream scripts
# (and Quarto) can load a consistent tidy dataset without re-running 01_/02_/03_.
tidy_bundle_dir <- here("data-processed")
dir.create(tidy_bundle_dir, recursive = TRUE, showWarnings = FALSE)

analysis_data <- list(
  screening_data   = screening_data,
  household_data   = household_data,
  treatment_data   = treatment_data,
  ea_data          = ea_data,
  weekly_data      = weekly_data,
  weekly_data_na   = weekly_data_na,
  weekly_long      = weekly_long,
  monthly_data     = monthly_data,
  monthly_data_na  = monthly_data_na,
  monthly_long     = monthly_long,
  ea_agg_wide      = ea_agg_wide,
  ea_agg_long      = ea_agg_long,
  village_agg_wide = village_agg_wide,
  village_agg_long = village_agg_long,
  village_data     = village_data,
  village_data_cum = village_data_cum,
  village_order_cum = village_order_cum,
  layer_ki_ea_3832   = layer_ki_ea_3832,
  layer_betio_ea_3832 = layer_betio_ea_3832,
  layer_hh_betio_3832 = layer_hh_betio_3832,
  min_week = min_week,
  max_week = max_week,
  min_month = min_month,
  max_month = max_month,
  current_date = current_date
)

tidy_data_path <- file.path(tidy_bundle_dir, glue::glue("tidy_data_{current_date}.qs"))
tidy_data_latest_path <- file.path(tidy_bundle_dir, "tidy_data_latest.qs")

qs::qsave(analysis_data, tidy_data_path)
qs::qsave(analysis_data, tidy_data_latest_path)

message("Saved tidy data bundle to ", tidy_data_path)

