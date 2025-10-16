# 06.15_tpt_review_summary.R (simplified; uses outputs from 03_)
# - Form status: Expected / Done
# - Side effects (among DONE): Any / Resolved / Not resolved / No side effects
# - Outcome of review (among DONE): Complete / Continue / Suspend

library(tidyverse)
library(flextable)
library(here)

# ---------------- Config ----------------
timepoints <- c("1m","3m","4m")

# Helper to safely pull a column that might not exist (returns NA logical)
pull_or_na <- function(df, col) {
  if (col %in% names(df)) df[[col]] else NA
}

# Labels you consider equivalent to "resolved"
resolved_levels <- c("Yes - all resolved", "All resolved", "Resolved")

# ---------------- Build summaries ----------------
tp_summaries <- map_dfr(timepoints, function(tp) {
  
  # column names by timepoint
  any_col   <- paste0(tp, "_any_true")         # from 03_ side-effect build
  exp_col   <- paste0("tpt_", tp, "_expected") # logical (from 03_)
  done_col  <- paste0("tpt_", tp, "_done")     # logical (from 03_)
  sxres_col <- paste0("tpt_", tp, "_sxres")    # raw text in treatment_data
  out_col   <- paste0("tpt_", tp, "_outcome")  # raw text in treatment_data
  
  df <- treatment_data %>%
    mutate(
      # normalize a couple of fields (if present)
      .any_true = as.logical(pull_or_na(cur_data_all(), any_col)),
      .expected = as.logical(pull_or_na(cur_data_all(), exp_col)),
      .done     = as.logical(pull_or_na(cur_data_all(), done_col)),
      .sxres    = pull_or_na(cur_data_all(), sxres_col) |> as.character() |> stringr::str_squish(),
      .outcome  = pull_or_na(cur_data_all(), out_col)   |> as.character() |> stringr::str_squish()
    )
  
  # ---- Form status (simple totals) ----
  expected_n <- sum(df$.expected %in% TRUE, na.rm = TRUE)
  done_n     <- sum(df$.done     %in% TRUE, na.rm = TRUE)
  
  # ---- Restrict to DONE forms for clinical content ----
  df_done <- df %>% filter(.data[[done_col]] %in% TRUE)
  
  # Totals among DONE
  any_n   <- df_done %>% summarise(n = sum(.data[[any_col]] %in% TRUE,  na.rm = TRUE)) %>% pull(n)
  no_sx_n <- df_done %>% summarise(n = sum(.data[[any_col]] %in% FALSE, na.rm = TRUE)) %>% pull(n)
  
  # Partition "Any side effects" into resolved / not resolved / unknown
  resolved_n <- df_done %>%
    summarise(n = sum((.data[[any_col]] %in% TRUE) & (.data[[sxres_col]] %in% TRUE), na.rm = TRUE)) %>%
    pull(n)
  
  not_resolved_n <- df_done %>%
    summarise(n = sum((.data[[any_col]] %in% TRUE) & (.data[[sxres_col]] %in% FALSE), na.rm = TRUE)) %>%
    pull(n)
  
  unknown_n <- df_done %>%
    summarise(n = sum((.data[[any_col]] %in% TRUE) & is.na(.data[[sxres_col]]))) %>%
    pull(n)
  
  # Outcomes (among DONE forms)
  complete_n <- sum(df_done$.outcome == "Complete TPT",  na.rm = TRUE)
  continue_n <- sum(df_done$.outcome == "Continue TPT",  na.rm = TRUE)
  suspend_n  <- sum(df_done$.outcome == "Suspend TPT",   na.rm = TRUE)
  
  tibble(
    Group = c(rep("Form status", 2),
              rep("Side effects", 5),
              rep("Outcome of review", 3)),
    Row   = c("Expected",
              "Done",
              "Any side effects",
              "Resolved at time of assessment",
              "Not resolved",
              "Resolution unknown",
              "No side effects",
              "Complete TPT",
              "Continue TPT",
              "Suspend TPT"),
    tp    = tp,
    n     = c(expected_n, done_n,
              any_n, resolved_n, not_resolved_n, unknown_n, no_sx_n,
              complete_n, continue_n, suspend_n)
  )
})

# ---------------- Reshape to final wide layout ----------------
mat_tbl <- tp_summaries %>%
  mutate(tp = factor(tp, levels = timepoints)) %>%
  pivot_wider(names_from = tp, values_from = n, values_fill = 0) %>%
  rename(`1 month` = `1m`, `3 month` = `3m`, `4 month` = `4m`) %>%
  mutate(
    Group = factor(Group, levels = c("Form status", "Side effects", "Outcome of review")),
    Row   = factor(Row, levels = c(
      "Expected", "Done",
      "Any side effects", "Resolved at time of assessment", "Not resolved", "Resolution unknown", "No side effects",
      "Complete TPT", "Continue TPT", "Suspend TPT"
    ))
  ) %>%
  arrange(Group, Row)

# ---------------- Flextable ----------------
title_text <- paste0(
  "TPT routine monitoring (1, 3, 4 months) — generated ", format(Sys.Date(), "%Y-%m-%d")
)

table_06.15 <- mat_tbl %>%
  flextable(col_keys = c("Group","Row","1 month","3 month","4 month")) %>%
  add_header_lines(values = title_text) %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%
  bold(part = "header") %>%
  merge_v(j = "Group") %>%
  valign(j = "Group", valign = "top") %>%
  align(j = c("1 month","3 month","4 month"), align = "center", part = "all") %>%
  colformat_int(j = c("1 month","3 month","4 month"), big.mark = ",") %>%
  autofit()

table_06.15

# ---------------- Save ----------------
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here::here("figures"), paste0("Outputs_", current_date))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_filename <- paste0("table_06.15_", current_date, ".png")
save_as_image(table_06.15, path = file.path(output_dir, output_filename))
