library(tidyverse)
library(openxlsx)
library(here)

age_cat_levels <- c("0-2", "3-9", "10-14", "15-64", "65+", "Missing", "All ages")

# Age × Xpert availability table -----------------------------

# Base: by xpert_group and age_cat_model
tbl_xpert_age_base <- screening_data %>%
  mutate(
    age_cat_model = fct_explicit_na(age_cat_model, na_level = "Missing"),
    xpert_group = if_else(
      xpert_available,
      "Xpert available",
      "Xpert not available"
    )
  ) %>%
  group_by(xpert_group, age_cat_model) %>%
  summarise(
    n_screened        = n(),
    n_screen_positive = sum(tb_decision == "Presumptive TB", na.rm = TRUE),
    n_confirmed_tb    = sum(ntp_diagnosis == "Confirmed",    na.rm = TRUE),
    .groups = "drop"
  )

# 1a. "All ages" row within each Xpert group
tbl_xpert_all_by_group <- tbl_xpert_age_base %>%
  group_by(xpert_group) %>%
  summarise(
    age_cat_model     = "All ages",
    n_screened        = sum(n_screened),
    n_screen_positive = sum(n_screen_positive),
    n_confirmed_tb    = sum(n_confirmed_tb),
    .groups = "drop"
  )

# 1b. "All" Xpert group for each age band (pooled across availability)
tbl_xpert_all_group_by_age <- tbl_xpert_age_base %>%
  group_by(age_cat_model) %>%
  summarise(
    xpert_group       = "All",
    n_screened        = sum(n_screened),
    n_screen_positive = sum(n_screen_positive),
    n_confirmed_tb    = sum(n_confirmed_tb),
    .groups = "drop"
  )

# 1c. "All" Xpert group + "All ages" (grand total)
tbl_xpert_all_group_all_age <- tbl_xpert_age_base %>%
  summarise(
    xpert_group       = "All",
    age_cat_model     = "All ages",
    n_screened        = sum(n_screened),
    n_screen_positive = sum(n_screen_positive),
    n_confirmed_tb    = sum(n_confirmed_tb),
    .groups = "drop"
  )

# 1d. Combine everything and add proportions
tbl_xpert_age <- bind_rows(
  tbl_xpert_age_base,
  tbl_xpert_all_by_group,
  tbl_xpert_all_group_by_age,
  tbl_xpert_all_group_all_age
) %>%
  mutate(
    n_screen_positive_prop = if_else(
      n_screened > 0,
      n_screen_positive / n_screened,
      NA_real_
    ),
    n_confirmed_tb_prop = if_else(
      n_screened > 0,
      n_confirmed_tb / n_screened,
      NA_real_
    ),
    xpert_group = factor(
      xpert_group,
      levels = c("Xpert available", "Xpert not available", "All")
    ),
    age_cat_model = factor(age_cat_model, levels = age_cat_levels, ordered = TRUE)
  ) %>%
  arrange(xpert_group, age_cat_model)

# Age × TST table -------------------------------------

tbl_tst_age <- screening_data %>%
  mutate(age_cat_model = fct_explicit_na(age_cat_model, na_level = "Missing")) %>%
  group_by(age_cat_model) %>%
  summarise(
    n_screened   = n(),
    n_tst_placed = sum(tst_placed_bin, na.rm = TRUE),
    n_tst_read   = sum(tst_read_bin,   na.rm = TRUE),
    n_tst_10mm   = sum(tst_10mm_bin,   na.rm = TRUE),
    .groups = "drop"
  )

# Add All ages row
tbl_tst_all <- tbl_tst_age %>%
  summarise(
    age_cat_model = "All ages",
    n_screened    = sum(n_screened),
    n_tst_placed  = sum(n_tst_placed),
    n_tst_read    = sum(n_tst_read),
    n_tst_10mm    = sum(n_tst_10mm)
  )

tbl_tst_age <- bind_rows(tbl_tst_age, tbl_tst_all) %>%
  mutate(
    n_tst_10mm_prop = if_else(
      n_tst_read > 0,
      n_tst_10mm / n_tst_read,
      NA_real_
    ),
    age_cat_model = factor(age_cat_model, levels = age_cat_levels, ordered = TRUE)
  ) %>%
  arrange(age_cat_model)

# TB clinical x infectious table (confirmed TB only) --------------

# We classify:
# - tb_clinical_bin   TRUE  -> "Clinical"
#                      FALSE -> "Subclinical"
#                      NA    -> "Unknown"
# - tb_moreinf_bin    TRUE  -> "More infectious"
#                      FALSE -> "Less infectious"
#                      NA    -> "Unknown"

tb_alloc_base <- screening_data %>%
  filter(ntp_diagnosis == "Confirmed") %>%
  mutate(
    clinical_cat = case_when(
      tb_clinical_bin == TRUE  ~ "Clinical",
      tb_clinical_bin == FALSE ~ "Subclinical",
      TRUE                     ~ "Unknown"
    ),
    infectious_cat = case_when(
      tb_moreinf_bin == TRUE  ~ "More infectious",
      tb_moreinf_bin == FALSE ~ "Less infectious",
      TRUE                    ~ "Unknown"
    )
  ) %>%
  count(clinical_cat, infectious_cat, name = "n")

# Set order of rows/columns
tb_alloc_wide <- tb_alloc_base %>%
  mutate(
    clinical_cat   = factor(clinical_cat,
                            levels = c("Subclinical", "Clinical", "Unknown")),
    infectious_cat = factor(infectious_cat,
                            levels = c("Less infectious", "More infectious", "Unknown"))
  ) %>%
  arrange(clinical_cat, infectious_cat) %>%
  pivot_wider(
    names_from  = infectious_cat,
    values_from = n,
    values_fill = 0
  )

# Add row totals
tb_alloc_wide <- tb_alloc_wide %>%
  mutate(Row_total = rowSums(across(where(is.numeric))))

# Add column totals (including row_total)
tb_alloc_totals <- tb_alloc_wide %>%
  summarise(
    clinical_cat = "All",
    across(where(is.numeric), sum)
  )

tb_alloc_wide <- bind_rows(tb_alloc_wide, tb_alloc_totals) %>%
  mutate(
    clinical_cat = factor(clinical_cat, levels = c("Subclinical", "Clinical", "Unknown", "All"))
  ) %>%
  arrange(clinical_cat)

# Algorithm sensitivity tables ------------------------

# (overall + 4 TB subtypes)
# Denominator: confirmed TB cases screened when Xpert available
# CIs: exact binomial (binom.test)

# Denominator cohort (gold-standard opportunity)

tb_cases <- screening_data %>%
  filter(
    xpert_available == TRUE,
    ntp_diagnosis == "Confirmed"
  ) %>%
  mutate(
    # Hypothetical algorithms (treat missing as NOT positive)
    alg_symptoms_only = coalesce(calc_sx == "Sx Positive", FALSE),
    alg_symptoms_or_cxr = coalesce(calc_sx == "Sx Positive", FALSE) |
      coalesce(calc_xr == "1 cw TB", FALSE),
    alg_pearl = TRUE
  )

# Overall sensitivity table (all confirmed TB in Xpert-available periods)

sens_overall <- tb_cases %>%
  transmute(
    stratum = "All confirmed TB (Xpert available)",
    alg_symptoms_only,
    alg_symptoms_or_cxr,
    alg_pearl
  ) %>%
  pivot_longer(
    cols = starts_with("alg_"),
    names_to = "algorithm",
    values_to = "screen_positive"
  ) %>%
  group_by(stratum, algorithm) %>%
  summarise(
    denominator = n(),
    numerator = sum(screen_positive, na.rm = TRUE),
    .groups = "drop"
  )

# Clinical/subclinical only sensitivity table (ignore infectiousness)

sens_clinical <- tb_cases %>%
  filter(!is.na(tb_clinical_bin)) %>%
  mutate(
    stratum = if_else(tb_clinical_bin, "Clinical", "Subclinical")
  ) %>%
  transmute(
    stratum,
    alg_symptoms_only,
    alg_symptoms_or_cxr,
    alg_pearl
  ) %>%
  pivot_longer(
    cols = starts_with("alg_"),
    names_to = "algorithm",
    values_to = "screen_positive"
  ) %>%
  group_by(stratum, algorithm) %>%
  summarise(
    denominator = n(),
    numerator = sum(screen_positive, na.rm = TRUE),
    .groups = "drop"
  )

# More/less infectious only sensitivity table (ignore clinical status)

sens_infectious <- tb_cases %>%
  filter(!is.na(tb_moreinf_bin)) %>%
  mutate(
    stratum = if_else(tb_moreinf_bin, "More infectious", "Less infectious")
  ) %>%
  transmute(
    stratum,
    alg_symptoms_only,
    alg_symptoms_or_cxr,
    alg_pearl
  ) %>%
  pivot_longer(
    cols = starts_with("alg_"),
    names_to = "algorithm",
    values_to = "screen_positive"
  ) %>%
  group_by(stratum, algorithm) %>%
  summarise(
    denominator = n(),
    numerator = sum(screen_positive, na.rm = TRUE),
    .groups = "drop"
  )

# Subtype sensitivity table (4 subtypes only: exclude missing subtype flags)

sens_subtypes <- tb_cases %>%
  filter(!is.na(tb_clinical_bin), !is.na(tb_moreinf_bin)) %>%
  mutate(
    stratum = paste0(
      if_else(tb_clinical_bin, "Clinical", "Subclinical"),
      " + ",
      if_else(tb_moreinf_bin, "More infectious", "Less infectious")
    )
  ) %>%
  transmute(
    stratum,
    alg_symptoms_only,
    alg_symptoms_or_cxr,
    alg_pearl
  ) %>%
  pivot_longer(
    cols = starts_with("alg_"),
    names_to = "algorithm",
    values_to = "screen_positive"
  ) %>%
  group_by(stratum, algorithm) %>%
  summarise(
    denominator = n(),
    numerator = sum(screen_positive, na.rm = TRUE),
    .groups = "drop"
  )

# Combine + label algorithms + compute sensitivity + exact binomial CI

sens_long <- bind_rows(
  sens_overall,
  sens_clinical,
  sens_infectious,
  sens_subtypes
) %>%
  mutate(
    algorithm = recode(
      algorithm,
      "alg_symptoms_only"   = "Symptoms only",
      "alg_symptoms_or_cxr" = "Symptoms OR CXR (cw TB)",
      "alg_pearl"           = "PEARL algorithm (Symptoms + CXR + Xpert)"
    ),
    algorithm = factor(
      algorithm,
      levels = c(
        "Symptoms only",
        "Symptoms OR CXR (cw TB)",
        "PEARL algorithm (Symptoms + CXR + Xpert)"
      )
    ),
    stratum = factor(
      stratum,
      levels = c(
        "All confirmed TB (Xpert available)",
        "Clinical",
        "Subclinical",
        "More infectious",
        "Less infectious",
        "Subclinical + Less infectious",
        "Subclinical + More infectious",
        "Clinical + Less infectious",
        "Clinical + More infectious"
      )
    )
  ) %>%
  arrange(stratum, algorithm) %>%
  mutate(
    sensitivity = if_else(denominator > 0, numerator / denominator, NA_real_)
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(
      if (is.na(denominator) || denominator == 0) c(NA_real_, NA_real_)
      else stats::binom.test(numerator, denominator)$conf.int
    ),
    ci_low  = ci[[1]],
    ci_high = ci[[2]]
  ) %>%
  ungroup() %>%
  select(algorithm, stratum, numerator, denominator, sensitivity, ci_low, ci_high)


# Write Excel file -----------------------------------

# Ensure folder exists 
output_dir <- here("data-processed", "modelling")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Timestamped file name
timestamp <- format(Sys.Date(), "%Y-%m-%d")
output_path <- file.path(output_dir, paste0("modelling_prevalence_", timestamp, ".xlsx"))

wb <- createWorkbook()

addWorksheet(wb, "Xpert_by_age")
writeData(wb, "Xpert_by_age", tbl_xpert_age)

addWorksheet(wb, "TST_by_age")
writeData(wb, "TST_by_age", tbl_tst_age)

addWorksheet(wb, "TB_clinical_infectious")
writeData(wb, "TB_clinical_infectious", tb_alloc_wide)

addWorksheet(wb, "Algorithm_sensitivity")
writeData(wb, "Algorithm_sensitivity", sens_long)

saveWorkbook(wb, output_path, overwrite = TRUE)

message("Saved modelling prevalence tables to: ", output_path)
