# Script: 05_run_outputs.R
# Purpose: Implementation script to generate, verify, and export all PEARL outputs.
# One canonical exemplar call per function in 05_output_functions.R, organized
# in the same section order as that file. Report-specific parameterizations
# (e.g. a named village subset, a specific year range, a specific EA list) have
# been removed from this smoke-test script - see the note at the bottom of this
# file for what still needs a home in the relevant Quarto report(s).
# Author: Jeremy + Gemini
# Date: 2026-03-02
# Reorganized: 2026-08 (section order, output index) - see AGENTS.md

library(tidyverse)
library(here)
library(flextable)
library(officer)
library(webshot2) # Required for saving flextables as PNG
library(qs)

# --- DATA INFRASTRUCTURE ------------------------------------------------------

# Source the function library
source(here("R", "05_output_functions.R"))

# Unpack latest data (screening_data, treatment_data, weekly_data, etc.)
load_latest_tidy_data()

# Output directory setup (dated folder; reruns on the same day overwrite in
# place - the whole dated folder is cleared first so stale files from an
# earlier run today, e.g. from a since-removed output, don't linger)
current_date <- format(Sys.Date(), "%Y-%m-%d")
base_dir <- here("figures", paste0("Outputs_", current_date))

if (dir.exists(base_dir)) {
  unlink(base_dir, recursive = TRUE)
}

# Subfolders mirror the section order in 05_output_functions.R
folders <- c(
  "01_Activity_and_Quality",
  "02_Demographics",
  "03_Geographic_Coverage_and_Screening",
  "04_TB_Screening_Outcomes",
  "05_Leprosy_and_Prevention",
  "06_Scabies",
  "07_TPT_Pathway",
  "08_Modelling_and_Sensitivity"
)

walk(
  folders,
  ~ dir.create(file.path(base_dir, .x), recursive = TRUE, showWarnings = FALSE)
)


# --- EXPORT HELPERS -----------------------------------------------------------

#' Extract the roxygen title line above each top-level function in a script
#' Used to auto-populate the output index with a description for each
#' exported table/plot, sourced from 05_output_functions.R itself so the
#' index cannot drift out of sync with the function documentation.
extract_function_descriptions <- function(path) {
  src_lines <- readLines(path, warn = FALSE)
  def_idx <- grep(
    "^[A-Za-z_][A-Za-z0-9_\\.]*\\s*<-\\s*function\\(",
    src_lines
  )
  fn_names <- sub(
    "^([A-Za-z_][A-Za-z0-9_\\.]*)\\s*<-.*$",
    "\\1",
    src_lines[def_idx]
  )

  descriptions <- vapply(
    def_idx,
    function(idx) {
      p <- idx - 1
      title <- NA_character_
      while (p >= 1 && grepl("^#'", src_lines[p])) {
        txt <- sub("^#'\\s*", "", src_lines[p])
        if (nzchar(trimws(txt)) && !grepl("^@", trimws(txt))) {
          title <- trimws(txt)
        }
        p <- p - 1
      }
      if (is.na(title)) "" else title
    },
    character(1)
  )

  setNames(descriptions, fn_names)
}

description_lookup <- extract_function_descriptions(here(
  "R",
  "05_output_functions.R"
))

# Running record of every output actually generated this run, used to write
# the index file at the end. Populated by export_table()/export_plot() (and
# manually for the one xlsx export), so it only ever reflects outputs that
# succeeded.
output_log <- tibble(
  section = character(),
  type = character(),
  name = character(),
  files = character(),
  description = character()
)

#' Record one generated output for the end-of-run index
log_output <- function(section, type, name, fn, files) {
  desc <- if (fn %in% names(description_lookup)) {
    description_lookup[[fn]]
  } else {
    ""
  }
  output_log <<- bind_rows(
    output_log,
    tibble(
      section = section,
      type = type,
      name = name,
      files = paste(basename(files), collapse = ", "),
      description = desc
    )
  )
}

#' Save a flextable, append date stamp, create dir if missing, and assign to Env
#' @param fn Character. Name of the source function in 05_output_functions.R
#'   (e.g. "out_tab_activity_summary"), used to look up its description.
export_table <- function(ft, folder, name, fn, orientation = "portrait") {
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

  # 6. Record for the output index
  log_output(folder, "table", name, fn, c(path_docx, path_png))
}

#' Save a ggplot, append date stamp, create dir if missing, and assign to Env
#' @param fn Character. Name of the source function in 05_output_functions.R
#'   (e.g. "out_plot_weekly_activity"), used to look up its description.
export_plot <- function(p, folder, name, fn, width = 8, height = 5) {
  # 1. Assign to environment for quick console viewing
  assign(name, p, envir = .GlobalEnv)

  # 2. Ensure target directory exists (creates it silently if not)
  folder_path <- file.path(base_dir, folder)
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

  # 3. Build path and save
  path_png <- file.path(folder_path, paste0(name, "_", current_date, ".png"))
  ggsave(path_png, plot = p, width = width, height = height)

  # 4. Record for the output index
  log_output(folder, "plot", name, fn, path_png)
}

#' Write the accumulated output_log out as a plain-text index in base_dir
write_output_index <- function() {
  index_path <- file.path(base_dir, "index.txt")
  lines_out <- c(
    "PEARL+ Output Index",
    paste("Generated:", format(Sys.Date(), "%d %b %Y")),
    strrep("=", 70)
  )

  for (sec in folders) {
    sec_rows <- output_log %>% filter(section == sec)
    if (nrow(sec_rows) == 0) {
      next
    }

    lines_out <- c(lines_out, "", sec, strrep("-", nchar(sec)))
    for (i in seq_len(nrow(sec_rows))) {
      row <- sec_rows[i, ]
      lines_out <- c(
        lines_out,
        paste0("  ", row$name, " [", row$type, "]  ", row$description),
        paste0("    -> ", row$files)
      )
    }
  }

  writeLines(lines_out, index_path)
  message("Output index written to: ", index_path)
}


# --- ACTIVITY AND QUALITY OVER TIME OUTPUTS -----------------------------------
message("Generating Activity and Quality outputs...")

## Tables -------------------------------------
export_table(
  out_tab_activity_summary(),
  "01_Activity_and_Quality",
  "tab_activity_summary",
  "out_tab_activity_summary"
)
export_table(
  out_tab_team_weekly_review(),
  "01_Activity_and_Quality",
  "tab_team_weekly_review",
  "out_tab_team_weekly_review",
  orientation = "landscape"
)
export_table(
  out_tab_project_weekly_review(),
  "01_Activity_and_Quality",
  "tab_project_weekly_review",
  "out_tab_project_weekly_review",
  orientation = "landscape"
)

core_indicators <- get_all_indicators_dict() %>%
  dplyr::filter(is_core == TRUE) %>%
  dplyr::pull(Indicator_Key) %>%
  intersect(names(village_data))

export_table(
  out_tab_geo_indicators(
    data = village_data,
    indicators = core_indicators,
    title = "Geographic Performance Indicators (Village Level)",
    id_col = "village"
  ),
  "01_Activity_and_Quality",
  "tab_geo_indicators",
  "out_tab_geo_indicators",
  orientation = "landscape"
)

## Plots -------------------------------------
export_plot(
  out_plot_weekly_activity(),
  "01_Activity_and_Quality",
  "plot_weekly_activity",
  "out_plot_weekly_activity",
  width = 8,
  height = 4
)
export_plot(
  out_plot_weekly_quality(),
  "01_Activity_and_Quality",
  "plot_weekly_quality",
  "out_plot_weekly_quality",
  width = 10,
  height = 8
)
export_plot(
  out_plot_monthly_quality_indicators(),
  "01_Activity_and_Quality",
  "plot_monthly_quality_indicators",
  "out_plot_monthly_quality_indicators"
)


# --- DEMOGRAPHICS --------------------------------------------------------------
message("Generating Demographics outputs...")

export_plot(
  out_plot_age_pyramid(),
  "02_Demographics",
  "plot_age_pyramid",
  "out_plot_age_pyramid"
)


# --- GEOGRAPHIC COVERAGE AND SCREENING ----------------------------------------
message("Generating Geographic Coverage and Screening outputs...")

export_plot(
  out_plot_ea_coverage(),
  "03_Geographic_Coverage_and_Screening",
  "plot_ea_coverage",
  "out_plot_ea_coverage",
  width = 10
)
export_plot(
  out_plot_village_cumulative_coverage(),
  "03_Geographic_Coverage_and_Screening",
  "plot_village_cumulative_coverage",
  "out_plot_village_cumulative_coverage"
)
export_plot(
  out_plot_village_cumulative_screening(),
  "03_Geographic_Coverage_and_Screening",
  "plot_village_cumulative_screening",
  "out_plot_village_cumulative_screening"
)
export_plot(
  out_plot_betio_screening_map(),
  "03_Geographic_Coverage_and_Screening",
  "map_betio_screening",
  "out_plot_betio_screening_map",
  width = 10,
  height = 8
)
export_plot(
  out_plot_betio_coverage_map(),
  "03_Geographic_Coverage_and_Screening",
  "map_betio_coverage",
  "out_plot_betio_coverage_map",
  width = 10,
  height = 8
)
export_plot(
  out_plot_village_cumulative_eligible_coverage(),
  "03_Geographic_Coverage_and_Screening",
  "plot_village_cumulative_eligible_coverage",
  "out_plot_village_cumulative_eligible_coverage"
)
export_plot(
  out_plot_betio_household_points(),
  "03_Geographic_Coverage_and_Screening",
  "map_betio_household_points",
  "out_plot_betio_household_points",
  width = 10,
  height = 8
)


# --- TB SCREENING OUTCOMES (TB, TST, SPUTUM) ----------------------------------
message("Generating TB Screening Outcomes outputs...")

## Plots -------------------------------------
export_plot(
  out_plot_tb_outcome_proportions_6m(),
  "04_TB_Screening_Outcomes",
  "plot_tb_outcome_proportions_6m",
  "out_plot_tb_outcome_proportions_6m"
)
export_plot(
  out_plot_tst_proportions_6m(),
  "04_TB_Screening_Outcomes",
  "plot_tst_proportions_6m",
  "out_plot_tst_proportions_6m"
)
export_plot(
  out_plot_tb_yield_demographics(),
  "04_TB_Screening_Outcomes",
  "plot_tb_yield_demographics",
  "out_plot_tb_yield_demographics"
)
export_plot(
  out_plot_tst_positivity_by_age(),
  "04_TB_Screening_Outcomes",
  "plot_tst_positivity_by_age",
  "out_plot_tst_positivity_by_age"
)
export_plot(
  out_plot_tst_thresholds_age(),
  "04_TB_Screening_Outcomes",
  "plot_tst_thresholds_age",
  "out_plot_tst_thresholds_age"
)
export_plot(
  out_plot_tst_yield_demographics(),
  "04_TB_Screening_Outcomes",
  "plot_tst_yield_demographics",
  "out_plot_tst_yield_demographics"
)

## Tables -------------------------------------
export_table(
  out_tab_tst_yield_demographics_table(),
  "04_TB_Screening_Outcomes",
  "tab_tst_yield_demographics",
  "out_tab_tst_yield_demographics_table",
  orientation = "landscape"
)
export_table(
  out_tab_sputum_cascade(),
  "04_TB_Screening_Outcomes",
  "tab_sputum_cascade",
  "out_tab_sputum_cascade"
)
export_table(
  out_tab_tb_referral_outcomes(),
  "04_TB_Screening_Outcomes",
  "tab_tb_referral_outcomes",
  "out_tab_tb_referral_outcomes"
)
export_table(
  out_tab_tb_yield_efficiency(),
  "04_TB_Screening_Outcomes",
  "tab_tb_yield_efficiency",
  "out_tab_tb_yield_efficiency",
  orientation = "landscape"
)
export_table(
  out_tab_tb_yield_demographics_table(),
  "04_TB_Screening_Outcomes",
  "tab_tb_yield_demographics",
  "out_tab_tb_yield_demographics_table",
  orientation = "landscape"
)


# --- LEPROSY AND PREVENTION SCREENING OUTCOMES --------------------------------
message("Generating Leprosy and Prevention outputs...")

## Plots -------------------------------------
export_plot(
  out_plot_lep_yield_demographics(),
  "05_Leprosy_and_Prevention",
  "plot_lep_yield_demographics",
  "out_plot_lep_yield_demographics"
)
export_plot(
  out_plot_treatment_proportions_time(),
  "05_Leprosy_and_Prevention",
  "plot_treatment_proportions_monthly",
  "out_plot_treatment_proportions_time"
)

## Tables -------------------------------------
export_table(
  out_tab_lep_yield_demographics_table(),
  "05_Leprosy_and_Prevention",
  "tab_lep_yield_demographics",
  "out_tab_lep_yield_demographics_table",
  orientation = "landscape"
)
export_table(
  out_tab_lep_referral_outcomes(),
  "05_Leprosy_and_Prevention",
  "tab_lep_referral_outcomes",
  "out_tab_lep_referral_outcomes",
  orientation = "landscape"
)
export_table(
  out_tab_lep_ind_time(),
  "05_Leprosy_and_Prevention",
  "tab_lep_annual",
  "out_tab_lep_ind_time"
)
export_table(
  out_tab_lep_village(),
  "05_Leprosy_and_Prevention",
  "tab_lep_village",
  "out_tab_lep_village",
  orientation = "landscape"
)
export_table(
  out_tab_treatment_proportions_time(),
  "05_Leprosy_and_Prevention",
  "tab_treatment_proportions_monthly",
  "out_tab_treatment_proportions_time",
  orientation = "landscape"
)


# --- SCABIES SCREENING OUTCOMES ------------------------------------------------
message("Generating Scabies outputs...")

export_table(
  out_tab_scabies_prevalence_demographics(),
  "06_Scabies",
  "tab_scabies_prevalence_demographics",
  "out_tab_scabies_prevalence_demographics"
)


# --- TPT OUTPUTS ---------------------------------------------------------------
message("Generating TPT Pathway outputs...")

export_plot(
  out_plot_tpt_cascade(),
  "07_TPT_Pathway",
  "plot_tpt_cascade",
  "out_plot_tpt_cascade",
  width = 9
)
export_plot(
  out_plot_tpt_risk_cascade(),
  "07_TPT_Pathway",
  "plot_tpt_risk_cascade",
  "out_plot_tpt_risk_cascade"
)
export_plot(
  out_plot_tpt_ineligibility_reasons(),
  "07_TPT_Pathway",
  "plot_tpt_ineligibility_reasons",
  "out_plot_tpt_ineligibility_reasons",
  width = 7,
  height = 6
)
export_plot(
  out_plot_tpt_assessment_gaps(),
  "07_TPT_Pathway",
  "plot_tpt_assessment_gaps",
  "out_plot_tpt_assessment_gaps",
  width = 7,
  height = 6
)
export_table(
  out_tab_tpt_initiation_by_risk(),
  "07_TPT_Pathway",
  "tab_tpt_initiation_by_risk",
  "out_tab_tpt_initiation_by_risk",
  orientation = "landscape"
)

export_plot(
  out_plot_tpt_age_pyramid(),
  "07_TPT_Pathway",
  "plot_tpt_age_pyramid",
  "out_plot_tpt_age_pyramid"
)
export_table(
  out_tab_tpt_demographics_count(),
  "07_TPT_Pathway",
  "tab_tpt_demographics_count",
  "out_tab_tpt_demographics_count"
)

export_plot(
  out_plot_tpt_outcome_proportions(),
  "07_TPT_Pathway",
  "plot_tpt_outcome_proportions",
  "out_plot_tpt_outcome_proportions"
)
export_table(
  out_tab_tpt_outcomes_monthly(),
  "07_TPT_Pathway",
  "tab_tpt_outcomes_monthly",
  "out_tab_tpt_outcomes_monthly",
  orientation = "landscape"
)
export_plot(
  out_plot_tpt_retention_step(),
  "07_TPT_Pathway",
  "plot_tpt_retention_step",
  "out_plot_tpt_retention_step"
)
export_table(
  out_tab_tpt_monitoring_summary(),
  "07_TPT_Pathway",
  "tab_tpt_monitoring_summary",
  "out_tab_tpt_monitoring_summary"
)

export_plot(
  out_plot_tpt_followup_monthly(),
  "07_TPT_Pathway",
  "plot_tpt_followup_monthly",
  "out_plot_tpt_followup_monthly"
)

export_plot(
  out_plot_tpt_symptoms_demographics(),
  "07_TPT_Pathway",
  "plot_tpt_symptoms_demographics",
  "out_plot_tpt_symptoms_demographics"
)
export_table(
  out_tab_tpt_symptoms_count(),
  "07_TPT_Pathway",
  "tab_tpt_symptoms_count",
  "out_tab_tpt_symptoms_count"
)
export_table(
  out_tab_tpt_symptoms_detail(),
  "07_TPT_Pathway",
  "tab_tpt_symptoms_detail",
  "out_tab_tpt_symptoms_detail"
)
export_table(
  out_tab_tpt_outcomes_by_symptoms(),
  "07_TPT_Pathway",
  "tab_tpt_outcomes_by_symptoms",
  "out_tab_tpt_outcomes_by_symptoms"
)
export_table(
  out_tab_ae_type_summary(),
  "07_TPT_Pathway",
  "tab_ae_type_summary",
  "out_tab_ae_type_summary"
)
export_table(
  out_tab_tpt_discontinued_ae_profile(),
  "07_TPT_Pathway",
  "tab_tpt_discontinued_ae_profile",
  "out_tab_tpt_discontinued_ae_profile"
)


# --- MODELLING & SENSITIVITY INPUTS --------------------------------------------
message("Generating Modelling and Sensitivity outputs...")

modelling_xlsx_path <- file.path(
  base_dir,
  "08_Modelling_and_Sensitivity",
  paste0("modelling_inputs_", current_date, ".xlsx")
)
out_tab_modelling_inputs_xlsx(output_path = modelling_xlsx_path)
log_output(
  "08_Modelling_and_Sensitivity",
  "xlsx",
  "modelling_inputs",
  "out_tab_modelling_inputs_xlsx",
  modelling_xlsx_path
)


# Final Summary ----------------------------------------------------------------
write_output_index()

message("---")
message("Processing complete for PEARL+ Dashboard library.")
message("All outputs saved to: ", base_dir)

# NOTE: Report-specific exports removed from this smoke-test script during the
# 2026-08 reorganization (previously lived here as one-off calls with
# non-default parameters). These need a home in their respective Quarto
# reports rather than in this general exemplar-generation script:
#   - out_tab_geo_indicators() for a named EA subset (was: Banraeaba)
#   - out_tab_team_weekly_review()/out_tab_project_weekly_review() variants for
#     period = "previous" and/or core_only = FALSE
#   - out_tab_lep_village() for specific named villages and year ranges
#   - out_tab_lep_ind_time() for a specific date range/interval
