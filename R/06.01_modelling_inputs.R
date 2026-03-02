# 06.01_modelling_inputs.R
# Analysis and functions to create excel file with modelling inputs from PEARL data
# Author: Jeremy + Gemini
# Created: 2/3/26



# --- MODELLING & SENSITIVITY INPUTS -------------------------------------------

#' Generate and export epidemiological modelling tables to Excel
#' @param data Dataframe. Defaults to screening_data from the environment
#' @param output_path String. Optional path for the Excel export
out_tab_modelling_inputs_xlsx <- function(data = screening_data, output_path = NULL) {
  
  # --- 1. CONFIGURATION -------------------------------------------------------
  age_cat_levels <- c("0-2", "3-9", "10-14", "15-64", "65+", "Missing", "All ages")
  
  if (is.null(output_path)) {
    output_dir <- here::here("data-processed", "modelling")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    output_path <- file.path(output_dir, paste0("modelling_prevalence_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
  }
  
  # --- 2. XPERT AVAILABILITY BY AGE -------------------------------------------
  tbl_xpert_age_base <- data %>%
    mutate(
      age_cat_model = forcats::fct_explicit_na(age_cat_model, na_level = "Missing"),
      xpert_group = if_else(xpert_available, "Xpert available", "Xpert not available")
    ) %>%
    group_by(xpert_group, age_cat_model) %>%
    summarise(
      n_screened        = n(),
      n_screen_positive = sum(tb_decision == "Presumptive TB", na.rm = TRUE),
      n_confirmed_tb    = sum(ntp_diagnosis == "Confirmed",    na.rm = TRUE),
      .groups = "drop"
    )
  
  # Totals for Xpert Group, Age bands, and Grand Total
  tbl_xpert_age <- bind_rows(
    tbl_xpert_age_base,
    tbl_xpert_age_base %>% group_by(xpert_group) %>% 
      summarise(age_cat_model = "All ages", across(starts_with("n_"), sum), .groups = "drop"),
    tbl_xpert_age_base %>% group_by(age_cat_model) %>% 
      summarise(xpert_group = "All", across(starts_with("n_"), sum), .groups = "drop"),
    tbl_xpert_age_base %>% 
      summarise(xpert_group = "All", age_cat_model = "All ages", across(starts_with("n_"), sum), .groups = "drop")
  ) %>%
    mutate(
      n_screen_positive_prop = if_else(n_screened > 0, n_screen_positive / n_screened, NA_real_),
      n_confirmed_tb_prop    = if_else(n_screened > 0, n_confirmed_tb / n_screened, NA_real_),
      xpert_group = factor(xpert_group, levels = c("Xpert available", "Xpert not available", "All")),
      age_cat_model = factor(age_cat_model, levels = age_cat_levels, ordered = TRUE)
    ) %>%
    arrange(xpert_group, age_cat_model)
  
  # --- 3. TST BY AGE ----------------------------------------------------------
  tbl_tst_age <- data %>%
    mutate(age_cat_model = forcats::fct_explicit_na(age_cat_model, na_level = "Missing")) %>%
    group_by(age_cat_model) %>%
    summarise(
      n_screened   = n(),
      n_tst_placed = sum(tst_placed_bin, na.rm = TRUE),
      n_tst_read   = sum(tst_read_bin,   na.rm = TRUE),
      n_tst_10mm   = sum(tst_10mm_bin,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    bind_rows(summarise(., age_cat_model = "All ages", across(where(is.numeric), sum))) %>%
    mutate(
      n_tst_10mm_prop = if_else(n_tst_read > 0, n_tst_10mm / n_tst_read, NA_real_),
      age_cat_model = factor(age_cat_model, levels = age_cat_levels, ordered = TRUE)
    ) %>%
    arrange(age_cat_model)
  
  # --- 4. TB CLINICAL X INFECTIOUS (CONFIRMED ONLY) ---------------------------
  tb_alloc_wide <- data %>%
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
    count(clinical_cat, infectious_cat) %>%
    tidyr::pivot_wider(names_from = infectious_cat, values_from = n, values_fill = 0) %>%
    mutate(Row_total = rowSums(across(where(is.numeric)))) %>%
    bind_rows(summarise(., clinical_cat = "All", across(where(is.numeric), sum))) %>%
    mutate(clinical_cat = factor(clinical_cat, levels = c("Subclinical", "Clinical", "Unknown", "All"))) %>%
    arrange(clinical_cat)
  
  # --- 5. ALGORITHM SENSITIVITY -----------------------------------------------
  tb_cases <- data %>%
    filter(xpert_available == TRUE, ntp_diagnosis == "Confirmed") %>%
    mutate(
      alg_symptoms_only = coalesce(calc_sx == "Sx Positive", FALSE),
      alg_symptoms_or_cxr = coalesce(calc_sx == "Sx Positive", FALSE) | coalesce(calc_xr == "1 cw TB", FALSE),
      alg_pearl = TRUE
    )
  
  # Define sensitivity helper
  calc_sens <- function(df, stratum_label) {
    df %>%
      transmute(stratum = stratum_label, alg_symptoms_only, alg_symptoms_or_cxr, alg_pearl) %>%
      pivot_longer(cols = starts_with("alg_"), names_to = "algorithm", values_to = "pos") %>%
      group_by(stratum, algorithm) %>%
      summarise(denominator = n(), numerator = sum(pos), .groups = "drop")
  }
  
  sens_long <- bind_rows(
    calc_sens(tb_cases, "All confirmed TB (Xpert available)"),
    tb_cases %>% filter(!is.na(tb_clinical_bin)) %>% group_by(tb_clinical_bin) %>% 
      group_modify(~calc_sens(.x, if_else(.y$tb_clinical_bin, "Clinical", "Subclinical"))),
    tb_cases %>% filter(!is.na(tb_moreinf_bin)) %>% group_by(tb_moreinf_bin) %>% 
      group_modify(~calc_sens(.x, if_else(.y$tb_moreinf_bin, "More infectious", "Less infectious"))),
    tb_cases %>% filter(!is.na(tb_clinical_bin), !is.na(tb_moreinf_bin)) %>% 
      mutate(lab = paste0(if_else(tb_clinical_bin, "Clinical", "Subclinical"), " + ", 
                          if_else(tb_moreinf_bin, "More infectious", "Less infectious"))) %>%
      group_by(lab) %>% group_modify(~calc_sens(.x, .y$lab))
  ) %>%
    mutate(
      algorithm = recode(algorithm, "alg_symptoms_only" = "Symptoms only", "alg_symptoms_or_cxr" = "Symptoms OR CXR", "alg_pearl" = "PEARL"),
      sensitivity = numerator / denominator
    ) %>%
    rowwise() %>%
    mutate(
      ci = list(stats::binom.test(numerator, denominator)$conf.int),
      ci_low = ci[[1]], ci_high = ci[[2]]
    ) %>% ungroup()
  
  # --- 6. EXCEL EXPORT --------------------------------------------------------
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Xpert_by_age"); openxlsx::writeData(wb, "Xpert_by_age", tbl_xpert_age)
  openxlsx::addWorksheet(wb, "TST_by_age"); openxlsx::writeData(wb, "TST_by_age", tbl_tst_age)
  openxlsx::addWorksheet(wb, "TB_clinical_infectious"); openxlsx::writeData(wb, "TB_clinical_infectious", tb_alloc_wide)
  openxlsx::addWorksheet(wb, "Algorithm_sensitivity"); openxlsx::writeData(wb, "Algorithm_sensitivity", sens_long)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
  message("Saved modelling inputs to: ", output_path)
}