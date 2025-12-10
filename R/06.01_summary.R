library(dplyr)
library(tibble)
library(flextable)
library(here)
library(officer)

source(here("R/analysis_data.R"))

build_summary_table <- function(analysis_data) {
  weekly_data    <- analysis_data$weekly_data
  household_data <- analysis_data$household_data
  screening_data <- analysis_data$screening_data
  treatment_data <- analysis_data$treatment_data

  max_week <- max(weekly_data$period_start, na.rm = TRUE)
  wd <- weekly_data %>% filter(period_start == max_week)

  this_week <- list(
    "Households Enumerated" = wd$hh_enum_new,
    "Households Screened"   = wd$hh_screened,
    "People Registered"     = wd$reg,
    "TSTs Completed"        = wd$tst_read,
    "Referred to NTP"       = wd$ref_ntp,
    "Referred to NLP"       = wd$ref_nlp,
    "Referred to Hep B"     = wd$ref_hbv,
    "X-Rays Performed"      = wd$cxr_done,
    "X-Rays Resulted"       = wd$cxr_result,
    "Xpert Tests Done"      = wd$xpert,
    "NLP Outcome Recorded"  = wd$nlp_outcome_recorded,
    "NTP Outcome Recorded"  = wd$ntp_outcome_recorded,
    "Started on TPT"        = wd$tpt_start,
    "Completed TPT"         = wd$tpt_completed
  )

  total_counts <- list(
    "Households Enumerated" = household_data %>% filter(hh_reached) %>% nrow(),
    "Households Screened"   = screening_data %>% distinct(dwelling_name) %>% nrow(),
    "People Registered"     = nrow(screening_data),
    "TSTs Completed"        = sum(screening_data$tst_read_bin, na.rm = TRUE),
    "Referred to NTP"       = sum(screening_data$referred_ntp, na.rm = TRUE),
    "Referred to NLP"       = sum(screening_data$referred_nlp, na.rm = TRUE),
    "Referred to Hep B"     = sum(screening_data$prerx_hbv_1 == "Positive", na.rm = TRUE),
    "X-Rays Performed"      = sum(screening_data$cxr_done == "Yes", na.rm = TRUE),
    "X-Rays Resulted"       = sum(screening_data$xr_resulted, na.rm = TRUE),
    "Xpert Tests Done"      = sum(screening_data$spuxpt_labreq_lab, na.rm = TRUE),
    "NLP Outcome Recorded"  = sum(screening_data$nlp_outcome, na.rm = TRUE),
    "NTP Outcome Recorded"  = sum(screening_data$ntp_outcome, na.rm = TRUE),
    "Started on TPT"        = sum(!is.na(treatment_data$tpt_start_date)),
    "Completed TPT"         = sum(treatment_data$tpt_outcome_reason == "Completed", na.rm = TRUE)
  )

  summary_table <- tibble(
    Indicator   = names(this_week),
    This_Week   = unlist(this_week, use.names = FALSE),
    Total_Count = unlist(total_counts[names(this_week)], use.names = FALSE)
  )

  table_06.01 <- summary_table %>%
    flextable() %>%
    set_header_labels(Indicator = "Indicator", This_Week = "This Week", Total_Count = "Total Count") %>%
    add_header_row(
      values = paste("Summary of PEARL activity for week starting", format(max_week, "%Y-%m-%d")),
      colwidths = 3
    ) %>%
    theme_vanilla() %>%
    bg(part = "header", bg = "#F2F2F2") %>%
    bg(part = "body", bg = "white") %>%  # ensure white for dark-mode readability
    bold(part = "header") %>%
    bold(part = "body", j = "Indicator") %>%
    font(part = "header", fontname = "Arial") %>%
    font(part = "body",   fontname = "Arial") %>%
    fontsize(size = 12, part = "header") %>%
    fontsize(size = 10, part = "body") %>%
    line_spacing(space = 1.3, part = "header") %>%
    align(j = c("This_Week", "Total_Count"), align = "center", part = "all") %>%
    border(part = "all", border = fp_border(color = "black", width = 0.5)) %>%
    border_inner(border = fp_border(color = "black", width = 0.5)) %>%
    border_outer(border = fp_border(color = "black", width = 1.2)) %>%
    width(j = "Indicator", width = 3.8) %>%
    width(j = c("This_Week", "Total_Count"), width = 1.6) %>%
    autofit()

  list(table = table_06.01, max_week = max_week)
}

save_summary_table_image <- function(analysis_data = get_analysis_data(), output_dir = NULL) {
  version_date <- if (!is.null(analysis_data$current_date)) analysis_data$current_date else format(Sys.Date(), "%Y-%m-%d")
  if (is.null(output_dir)) {
    output_dir <- file.path(here("figures"), paste0("Outputs_", version_date))
  }

  built <- build_summary_table(analysis_data)
  output_filename <- paste0("table_06.01_", version_date, ".png")

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  save_as_image(built$table, path = file.path(output_dir, output_filename))

  invisible(file.path(output_dir, output_filename))
}

if (interactive()) {
  analysis_data <- get_analysis_data(build_if_missing = TRUE)
  save_summary_table_image(analysis_data)
}
