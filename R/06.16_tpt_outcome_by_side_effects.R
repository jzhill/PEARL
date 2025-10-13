# 06.16_tpt_outcome_by_side_effects.R
# Cross-tab of TPT outcomes by side-effect history (sx_any_ever)
# FILTER: only patients with tpt_start_date < (today - 4 months)
# Columns (each with n, %): Reported side effects | No side effects | All

library(tidyverse)
library(lubridate)
library(flextable)
library(officer)
library(here)

# ---- Filter to starts ≥4 months ago -----------------------------------------
cutoff_date <- Sys.Date() %m-% months(4)

note_text <- paste0(
  "Note: Includes only patients who commenced TPT before ",
  format(cutoff_date, "%d %b %Y"),
  " (≥ 4 months before table run date)."
)

df <- treatment_data %>%
  filter(!is.na(tpt_start_date), tpt_start_date < cutoff_date)

# ---- Labels / ordering -------------------------------------------------------
tpt_outcome_levels <- c(
  "Completed",
  "Died",
  "Discontinued",
  "Lost to follow-up",
  "Withdrew consent",
  "Not yet assigned"
)
tpt_outcome_labels <- c(
  "Completed"         = "Completed TPT",
  "Died"              = "Died",
  "Discontinued"      = "Discontinued (Medical Reason)",
  "Lost to follow-up" = "Lost to Follow-up",
  "Withdrew consent"  = "Withdrew Consent",
  "Not yet assigned"  = "Not yet assigned"
)

# ---- Build counts (stay LONG, then add "All" as a third column category) ----
base_long <- df %>%
  mutate(
    outcome_raw = if_else(is.na(tpt_outcome_reason) | tpt_outcome_reason == "",
                          "Not yet assigned", as.character(tpt_outcome_reason)),
    outcome = factor(outcome_raw, levels = tpt_outcome_levels),
    outcome = fct_explicit_na(outcome, na_level = "Not yet assigned"),
    outcome_lab = fct_relabel(outcome, ~ unname(tpt_outcome_labels[.])),
    se_status = if_else(sx_any_ever %in% TRUE, "Reported side effects", "No side effects")
  ) %>%
  count(outcome_lab, se_status, name = "n") %>%
  complete(
    outcome_lab = factor(names(tpt_outcome_labels), levels = names(tpt_outcome_labels)) %>%
      fct_relabel(~ unname(tpt_outcome_labels[.])),
    se_status = c("Reported side effects","No side effects"),
    fill = list(n = 0)
  )

# Add an "All" category (per outcome across both se_status values)
all_rows <- base_long %>%
  group_by(outcome_lab) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(se_status = "All")

counts_long <- bind_rows(base_long, all_rows) %>%
  rename(column = se_status) %>%
  mutate(
    column = factor(column, levels = c("Reported side effects","No side effects","All")),
    outcome_lab = factor(outcome_lab, levels = c(unname(tpt_outcome_labels), "Total"))
  )

# Add a Total row per column
counts_with_total <- counts_long %>%
  bind_rows(
    counts_long %>%
      group_by(column) %>%
      summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
      mutate(outcome_lab = "Total")
  )

# ---- Compute % within each column -------------------------------------------
counts_pct <- counts_with_total %>%
  group_by(column) %>%
  mutate(den = sum(n[outcome_lab != "Total"], na.rm = TRUE),
         pct = if_else(
           outcome_lab == "Total",
           if_else(den > 0, 100, NA_real_),
           if_else(den > 0, round(n / den * 100, 1), NA_real_)
         )) %>%
  ungroup()

counts_pct <- counts_pct %>%
  mutate(outcome_lab = forcats::fct_relevel(outcome_lab, "Total", after = Inf))

# ---- Pivot to wide with subcolumns n / % ------------------------------------
tbl_final <- counts_pct %>%
  arrange(outcome_lab, column) %>%
  pivot_wider(
    id_cols    = outcome_lab,
    names_from = column,
    values_from = c(n, pct),
    names_vary = "slowest"  # block by column: n_*, then pct_*
  ) %>%
  transmute(
    `TPT outcome` = as.character(outcome_lab),
    `Reported side effects_n` = `n_Reported side effects`,
    `Reported side effects_%` = `pct_Reported side effects`,
    `No side effects_n`       = `n_No side effects`,
    `No side effects_%`       = `pct_No side effects`,
    `All_n`                   = `n_All`,
    `All_%`                   = `pct_All`
  )

# ---- Flextable ---------------------------------------------------------------
table_06.16 <- tbl_final %>%
  flextable(col_keys = c(
    "TPT outcome",
    "Reported side effects_n","Reported side effects_%",
    "No side effects_n","No side effects_%",
    "All_n","All_%"
  )) %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%
  add_header_row(values = c("","Reported side effects","No side effects","All"),
                 colwidth = c(1, 2, 2, 2)) %>%
  set_header_labels(
    `TPT outcome`              = "TPT outcome",
    `Reported side effects_n`  = "n",
    `Reported side effects_%`  = "%",
    `No side effects_n`        = "n",
    `No side effects_%`        = "%",
    `All_n`                    = "n",
    `All_%`                    = "%"
  ) %>%
  bold(part = "header") %>%
  align(j = c("Reported side effects_n","Reported side effects_%",
              "No side effects_n","No side effects_%",
              "All_n","All_%"),
        align = "center", part = "all") %>%
  colformat_int(j = c("Reported side effects_n","No side effects_n","All_n"),
                big.mark = ",") %>%
  colformat_num(j = c("Reported side effects_%","No side effects_%","All_%"),
                digits = 1, suffix = "%") %>%
  bold(i = ~ `TPT outcome` == "Total") %>%
  add_footer_lines(values = note_text) %>%
  bg(part = "footer", bg = "white") %>%
  italic(part = "footer") %>%
  hline_top(part = "footer", border = officer::fp_border(color = "grey70")) %>%
  autofit()

table_06.16

# ---- Save flextable ----------------------------------------------------------
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_filename <- paste0("table_06.16_", current_date, ".png")
save_as_image(table_06.16, path = file.path(output_dir, output_filename))
