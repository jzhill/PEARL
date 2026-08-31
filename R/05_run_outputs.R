# Script: 05_run_outputs.R
# Purpose: Implementation script to generate, verify, and export all PEARL outputs
# Author: Jeremy + Gemini
# Date: 2026-03-02

library(tidyverse)
library(here)
library(flextable)
library(officer)
library(webshot2) # Required for saving flextables as PNG
library(qs)

# 1. Infrastructure Setup ------------------------------------------------------

# Source the refactored library
source(here("R", "05_output_functions.R"))

# Unpack latest data (screening_data, treatment_data, weekly_data, etc.)
load_latest_tidy_data()

# Output directory setup
current_date <- format(Sys.Date(), "%Y-%m-%d")
base_dir <- here("figures", paste0("Outputs_", current_date))

# Create logical subfolders
folders <- c(
  "01_Participation",
  "02_Screening_Yield",
  "03_TPT_Pathway",
  "04_Safety_Monitoring",
  "05_Quality_and_Performance",
  "06_Geography",
  "07_Modelling"
)

walk(
  folders,
  ~ dir.create(file.path(base_dir, .x), recursive = TRUE, showWarnings = FALSE)
)


# --- EXPORT HELPERS -----------------------------------------------------------

#' Save a flextable, append date stamp, create dir if missing, and assign to Env
export_table <- function(ft, folder, name, orientation = "portrait") {
  # 1. Assign to environment for quick console viewing
  assign(name, ft, envir = .GlobalEnv)

  # 2. Ensure target directory exists (creates it silently if not)
  folder_path <- file.path(base_dir, folder)
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

  # 3. Build paths with date stamp
  file_base <- paste0(name, "_", current_date)
  path_docx <- file.path(folder_path, paste0(file_base, ".docx"))
  path_png <- file.path(folder_path, paste0(file_base, ".png"))

  # 4. Save as Word
  if (orientation == "landscape") {
    lp_section <- prop_section(
      page_size = page_size(orient = "landscape"),
      type = "continuous"
    )
    flextable::save_as_docx(ft, path = path_docx, pr_section = lp_section)
  } else {
    flextable::save_as_docx(ft, path = path_docx)
  }

  # 5. Save as PNG
  flextable::save_as_image(ft, path = path_png)
}

#' Save a ggplot, append date stamp, create dir if missing, and assign to Env
export_plot <- function(p, folder, name, width = 8, height = 5) {
  # 1. Assign to environment for quick console viewing
  assign(name, p, envir = .GlobalEnv)

  # 2. Ensure target directory exists (creates it silently if not)
  folder_path <- file.path(base_dir, folder)
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

  # 3. Build path and save
  path_png <- file.path(folder_path, paste0(name, "_", current_date, ".png"))
  ggsave(path_png, plot = p, width = width, height = height)
}

# 2. Participation & Coverage --------------------------------------------------
message("Generating Participation and Coverage outputs...")

export_plot(
  out_plot_weekly_activity(),
  "01_Participation",
  "plot_weekly_activity",
  width = 8,
  height = 4
)
export_plot(out_plot_age_pyramid(), "01_Participation", "plot_age_pyramid")
export_plot(
  out_plot_ea_coverage(),
  "01_Participation",
  "plot_ea_coverage",
  width = 10
)
export_plot(
  out_plot_village_cumulative_coverage(),
  "01_Participation",
  "plot_village_cum_coverage"
)
export_plot(
  out_plot_village_cumulative_eligible_coverage(),
  "01_Participation",
  "plot_village_cum_eligible"
)
export_plot(
  out_plot_village_cumulative_screening(),
  "01_Participation",
  "plot_village_cum_screening"
)

export_table(
  out_tab_activity_summary(),
  "01_Participation",
  "tab_activity_summary"
)


# 3. Screening Results & Yield -------------------------------------------------
message("Generating Screening Results outputs...")

export_plot(
  out_plot_tb_yield_demographics(),
  "02_Screening_Yield",
  "plot_tb_yield_demo"
)
export_plot(
  out_plot_lep_yield_demographics(),
  "02_Screening_Yield",
  "plot_lep_yield_demo"
)
# export_plot(out_plot_lep_yield_2025(), "02_Screening_Yield", "plot_lep_yield_25")
export_plot(
  out_plot_tb_outcome_proportions_6m(),
  "02_Screening_Yield",
  "plot_weekly_tb_dec"
)
export_plot(
  out_plot_tst_positivity_by_age(),
  "02_Screening_Yield",
  "plot_tst_pos_age"
)
export_plot(
  out_plot_tst_thresholds_age(),
  "02_Screening_Yield",
  "plot_tst_thresholds"
)
export_plot(
  out_plot_tst_yield_demographics(),
  "02_Screening_Yield",
  "plot_tst_yield_demo"
)

export_table(
  out_tab_treatment_proportions_monthly(),
  "02_Screening_Yield",
  "tab_tx_proportions",
  orientation = "landscape"
)
export_table(
  out_tab_tb_yield_efficiency(),
  "02_Screening_Yield",
  "tab_tb_yield_efficiency",
  orientation = "landscape"
)
export_table(
  out_tab_sputum_cascade(),
  "02_Screening_Yield",
  "tab_sputum_cascade"
)
export_table(
  out_tab_tb_referral_outcomes(),
  "02_Screening_Yield",
  "tab_tb_referral_outcomes"
)
export_table(
  out_tab_lep_referral_outcomes(),
  "02_Screening_Yield",
  "tab_lep_referral_outcomes",
  orientation = "landscape"
)
export_table(
  out_tab_scabies_prevalence_demographics(),
  "02_Screening_Yield",
  "tab_scabies_prevalence"
)

export_table(
  out_tab_tb_yield_demographics_table(),
  "02_Screening_Yield",
  "tab_tb_yield_demographics",
  orientation = "landscape"
)
export_table(
  out_tab_tst_yield_demographics_table(),
  "02_Screening_Yield",
  "tab_tst_yield_demographics",
  orientation = "landscape"
)
export_table(
  out_tab_lep_yield_demographics_table(),
  "02_Screening_Yield",
  "tab_lep_yield_demographics",
  orientation = "landscape"
)


# 4. TPT Pathway & Risk --------------------------------------------------------
message("Generating TPT Pathway outputs...")

export_plot(
  out_plot_tpt_cascade(),
  "03_TPT_Pathway",
  "plot_tpt_cascade",
  width = 9
)
export_plot(
  out_plot_tpt_risk_cascade(),
  "03_TPT_Pathway",
  "plot_tpt_risk_cascade"
)
export_plot(
  out_plot_tpt_ineligibility_reasons(),
  "03_TPT_Pathway",
  "plot_tpt_inelig_pie",
  width = 7,
  height = 6
)
export_plot(
  out_plot_tpt_assessment_gaps(),
  "03_TPT_Pathway",
  "plot_tpt_gaps_pie",
  width = 7,
  height = 6
)

export_table(
  out_tab_tpt_initiation_by_risk(),
  "03_TPT_Pathway",
  "tab_tpt_initiation_risk",
  orientation = "landscape"
)


# 5. Treatment Monitoring & Safety ---------------------------------------------
message("Generating Treatment Monitoring outputs...")

export_plot(
  out_plot_tpt_retention_step(),
  "04_Safety_Monitoring",
  "plot_tpt_retention_step"
)
export_plot(
  out_plot_tpt_outcome_proportions(),
  "04_Safety_Monitoring",
  "plot_tpt_outcome_prop"
)
export_plot(
  out_plot_tpt_age_pyramid(),
  "04_Safety_Monitoring",
  "plot_tpt_age_pyramid"
)
export_plot(
  out_plot_tpt_symptoms_demographics(),
  "04_Safety_Monitoring",
  "plot_tpt_sx_demo"
)

export_table(
  out_tab_tpt_monitoring_summary(),
  "04_Safety_Monitoring",
  "tab_tpt_monitoring_summary"
)
export_table(
  out_tab_tpt_outcomes_monthly(),
  "04_Safety_Monitoring",
  "tab_tpt_outcomes_monthly",
  orientation = "landscape"
)
export_table(
  out_tab_tpt_outcomes_by_symptoms(),
  "04_Safety_Monitoring",
  "tab_tpt_outcomes_symptoms"
)
export_table(
  out_tab_tpt_demographics_count(),
  "04_Safety_Monitoring",
  "tab_tpt_demographics_count"
)
export_table(
  out_tab_tpt_symptoms_count(),
  "04_Safety_Monitoring",
  "tab_tpt_symptoms_count"
)
export_table(
  out_tab_tpt_symptoms_detail(),
  "04_Safety_Monitoring",
  "tab_tpt_symptoms_detail"
)
export_table(
  out_tab_ae_type_summary(),
  "04_Safety_Monitoring",
  "tab_ae_type_summary"
)
export_table(
  out_tab_tpt_discontinued_ae_profile(),
  "04_Safety_Monitoring",
  "tab_discontinued_profile"
)


# 6. Performance & Quality Indicators ------------------------------------------
message("Generating Performance and Quality outputs...")

export_plot(
  out_plot_weekly_quality(),
  "05_Quality_and_Performance",
  "plot_weekly_quality",
  width = 10,
  height = 8
)
export_plot(
  out_plot_monthly_quality_indicators(),
  "05_Quality_and_Performance",
  "plot_monthly_quality"
)
export_plot(
  out_plot_tpt_followup_monthly(),
  "05_Quality_and_Performance",
  "plot_tpt_followup"
)

export_table(
  out_tab_team_weekly_review(period = "current", core_only = TRUE),
  "05_Quality_and_Performance",
  "tab_team_current_core",
  orientation = "landscape"
)
export_table(
  out_tab_team_weekly_review(period = "current", core_only = FALSE),
  "05_Quality_and_Performance",
  "tab_team_current_all",
  orientation = "landscape"
)
export_table(
  out_tab_team_weekly_review(period = "previous", core_only = TRUE),
  "05_Quality_and_Performance",
  "tab_team_previous_core",
  orientation = "landscape"
)
export_table(
  out_tab_team_weekly_review(period = "previous", core_only = FALSE),
  "05_Quality_and_Performance",
  "tab_team_previous_all",
  orientation = "landscape"
)
export_table(
  out_tab_project_weekly_review(core_only = TRUE),
  "05_Quality_and_Performance",
  "tab_project_trend_core",
  orientation = "landscape"
)
export_table(
  out_tab_project_weekly_review(core_only = FALSE),
  "05_Quality_and_Performance",
  "tab_project_trend_all",
  orientation = "landscape"
)


# 1. Define the specific list of indicators for the review
banraeaba_indicators <- c(
  "hh_2020",
  "hh_2023",
  "hh_enum_new",
  "pop_2020",
  "pop_2023",
  "pop_2023_ex02",
  "pop_all_new",
  "pop_elig_new",
  "pop_reg_enum_new",
  "reg",
  "prop_reg_2023_ex02",
  "prop_reg_enum_2023_ex02",
  "reg_complete",
  "cxr_elig",
  "cxr_done",
  "cxr_pct",
  "xpert",
  "xpert_pct",
  "tbdec_prestb",
  "tbdec_ro",
  "tbdec_missing",
  "tbdec_comp_pct",
  "tst_placed",
  "tst_read",
  "tst_read_pct",
  "tst_pos",
  "tst_neg",
  "tst_missing",
  "tpt_should_assess",
  "tpt_assessment_done",
  "tpt_eligible_n",
  "tpt_started_n",
  "tpt_start",
  "tpt_outcome_assigned",
  "tpt_completed"
)

# 2. Identify the EAs belonging to Banraeaba
banraeaba_ea_ids <- ea_data %>%
  dplyr::filter(village == "Banraeaba") %>%
  dplyr::pull(record_id)

# 3. Generate and export the table
# Note: id_col = "record_id" because we are looking at EAs
export_table(
  out_tab_geo_indicators(
    data = ea_data,
    indicators = banraeaba_indicators,
    title = "Geographic Indicators, Banraeaba by EA",
    areas = banraeaba_ea_ids,
    id_col = "record_id",
    show_total = TRUE,
    font_size = 7 # Smaller font to accommodate the long indicator list
  ),
  folder = "02_Screening_Yield",
  name = "tab_ea_performance_banraeaba",
  orientation = "landscape"
)

# 7. Geography -----------------------------------------------------------------
message("Generating Geographic outputs...")

export_plot(
  out_plot_betio_screening_map(),
  "06_Geography",
  "map_betio_screening",
  width = 10,
  height = 8
)
export_plot(
  out_plot_betio_coverage_map(),
  "06_Geography",
  "map_betio_coverage",
  width = 10,
  height = 8
)
export_plot(
  out_plot_betio_household_points(),
  "06_Geography",
  "map_betio_households",
  width = 10,
  height = 8
)


# 8. Modelling Exports ---------------------------------------------------------
message("Generating Modelling outputs...")


# No environment variable mapping needed for xlsx export
out_tab_modelling_inputs_xlsx(
  output_path = file.path(
    base_dir,
    "07_Modelling",
    paste0("modelling_inputs_", current_date, ".xlsx")
  )
)


# 9. Leprosy report outputs ------------------------

export_table(
  out_tab_lep_annual(start_year = 2023, end_year = 2025, table_width = 6),
  "02_Screening_Yield",
  "tab_lep_annual"
)
export_table(
  out_tab_lep_village(
    villages = c(
      "Betio-Temakin",
      "Betio-Temwanoku",
      "Betio-Takaronga",
      "Bairiki",
      "Teaoraereke"
    ),
    start_year = 2025,
    end_year = 2025,
    table_width = 8
  ),
  "02_Screening_Yield",
  "tab_lep_village_2025",
  orientation = "landscape"
)
export_table(
  out_tab_lep_village(
    villages = c(
      "Betio-Temakin",
      "Betio-Temwanoku",
      "Betio-Takaronga",
      "Bairiki",
      "Teaoraereke"
    ),
    start_year = 2023,
    end_year = 2025,
    table_width = 8
  ),
  "02_Screening_Yield",
  "tab_lep_village_2023",
  orientation = "landscape"
)


# Final Summary ----------------------------------------------------------------
message("---")
message("Processing complete for PEARL+ Dashboard library.")
message("All outputs saved to: ", base_dir)
