library(tidyverse)
library(openxlsx)
library(here)

# Age × Xpert availability table -----------------------------

# Base: by xpert_group and age_cat_model
tbl_xpert_age_base <- screening_data %>%
  filter(!is.na(age_cat_model)) %>%
  mutate(
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
    )
  ) %>%
  arrange(xpert_group, age_cat_model)

# Age × TST table -------------------------------------

tbl_tst_age <- screening_data %>%
  filter(!is.na(age_cat_model)) %>%
  group_by(age_cat_model) %>%
  summarise(
    n_screened   = n(),
    n_tst_placed = sum(tst_placed_bin, na.rm = TRUE),
    n_tst_read   = sum(tst_read_bin,   na.rm = TRUE),
    n_tst_10mm   = sum(tst_10mm_bin,   na.rm = TRUE),
    .groups = "drop"
  )

# ---- Add All ages row ----
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
    )
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
  tidyr::pivot_wider(
    names_from  = infectious_cat,
    values_from = n,
    values_fill = 0
  )

# Add row totals
tb_alloc_wide <- tb_alloc_wide %>%
  mutate(Row_total = rowSums(dplyr::across(where(is.numeric))))

# Add column totals (including row_total)
tb_alloc_totals <- tb_alloc_wide %>%
  summarise(
    clinical_cat = "All",
    dplyr::across(where(is.numeric), sum)
  )

tb_alloc_wide <- bind_rows(tb_alloc_wide, tb_alloc_totals)

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

saveWorkbook(wb, output_path, overwrite = TRUE)

message("Saved modelling prevalence tables to: ", output_path)
