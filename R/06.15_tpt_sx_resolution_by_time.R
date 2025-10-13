# 06.15_tpt_review_summary.R
# Routine nursing reviews at 1, 3, 4 months:
#   - Form status: Expected / Done
#   - Side effects: Any / Resolved / Not resolved / No side effects (among DONE forms)
#   - Outcome of review: Complete / Continue / Suspend (among DONE forms)

library(tidyverse)
library(flextable)
library(here)

# ---- Config -----------------------------------------------------------------
timepoints <- c("1m","3m","4m")

# explicit column helpers
col_any     <- function(tp) paste0(tp, "_any_true")
col_sxres   <- function(tp) paste0("tpt_", tp, "_sxres")
col_outcome <- function(tp) paste0("tpt_", tp, "_outcome")
col_exp     <- function(tp) paste0("tpt_", tp, "_expected")
col_done    <- function(tp) paste0("tpt_", tp, "_done")

# ---- Build per-timepoint summaries ------------------------------------------
tp_summaries <- map_dfr(timepoints, function(tp) {
  any_col   <- col_any(tp)
  sxres_col <- col_sxres(tp)
  out_col   <- col_outcome(tp)
  exp_col   <- col_exp(tp)
  done_col  <- col_done(tp)
  
  df <- treatment_data
  
  # ---- Form status (simple totals) ----
  expected_n <- df %>%
    summarise(n = sum(.data[[exp_col]] %in% TRUE, na.rm = TRUE)) %>% pull(n)
  
  done_n <- df %>%
    summarise(n = sum(.data[[done_col]] %in% TRUE, na.rm = TRUE)) %>% pull(n)
  
  # ---- Restrict to DONE forms for clinical content ----
  df_done <- df %>% filter(.data[[done_col]] %in% TRUE)
  
  # Side effects buckets (among DONE forms)
  any_n <- df_done %>% summarise(n = sum(.data[[any_col]] %in% TRUE,  na.rm = TRUE)) %>% pull(n)
  no_sx_n <- df_done %>% summarise(n = sum(.data[[any_col]] %in% FALSE, na.rm = TRUE)) %>% pull(n)
  
  resolved_n <- df_done %>%
    filter(.data[[any_col]] %in% TRUE) %>%
    summarise(n = sum(.data[[sxres_col]] == "Yes - all resolved", na.rm = TRUE)) %>%
    pull(n)
  
  not_resolved_n <- df_done %>%
    filter(.data[[any_col]] %in% TRUE) %>%
    summarise(n = sum(is.na(.data[[sxres_col]]) | .data[[sxres_col]] == "No - ongoing", na.rm = TRUE)) %>%
    pull(n)
  
  # Outcomes (among DONE forms)
  complete_n <- df_done %>%
    summarise(n = sum(.data[[out_col]] == "Complete TPT",  na.rm = TRUE)) %>% pull(n)
  continue_n <- df_done %>%
    summarise(n = sum(.data[[out_col]] == "Continue TPT",  na.rm = TRUE)) %>% pull(n)
  suspend_n  <- df_done %>%
    summarise(n = sum(.data[[out_col]] == "Suspend TPT",   na.rm = TRUE)) %>% pull(n)
  
  tibble(
    Group = c(rep("Form status", 2),
              rep("Side effects", 4),
              rep("Outcome of review", 3)),
    Row   = c("Expected",
              "Done",
              "Any side effects",
              "Resolved at time of assessment",
              "Not resolved",
              "No side effects",
              "Complete TPT",
              "Continue TPT",
              "Suspend TPT"),
    tp    = tp,
    n     = c(expected_n, done_n,
              any_n, resolved_n, not_resolved_n, no_sx_n,
              complete_n, continue_n, suspend_n)
  )
})

# ---- Reshape to final wide layout -------------------------------------------
mat_tbl <- tp_summaries %>%
  mutate(tp = factor(tp, levels = timepoints)) %>%
  pivot_wider(names_from = tp, values_from = n, values_fill = 0) %>%
  rename(`1 month` = `1m`, `3 month` = `3m`, `4 month` = `4m`) %>%
  mutate(
    Group = factor(Group, levels = c("Form status", "Side effects", "Outcome of review")),
    Row   = factor(Row, levels = c(
      "Expected", "Done",
      "Any side effects", "Resolved at time of assessment", "Not resolved", "No side effects",
      "Complete TPT", "Continue TPT", "Suspend TPT"
    ))
  ) %>%
  arrange(Group, Row)

# ---- Flextable ---------------------------------------------------------------
table_06.15 <- mat_tbl %>%
  flextable(col_keys = c("Group","Row","1 month","3 month","4 month")) %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%
  bold(part = "header") %>%
  merge_v(j = "Group") %>%
  valign(j = "Group", valign = "top") %>%
  align(j = c("1 month","3 month","4 month"), align = "center", part = "all") %>%
  colformat_int(j = c("1 month","3 month","4 month"), big.mark = ",") %>%
  autofit()

table_06.15

# ---- Save flextable ----------------------------------------------------------
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_filename <- paste0("table_06.15_", current_date, ".png")
save_as_image(table_06.15, path = file.path(output_dir, output_filename))
