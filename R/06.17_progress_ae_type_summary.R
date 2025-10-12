# 06.17_progress_ae_type_summary.R
# Rows: Side effects, Illness - new or changed, Medicines - new or changed, Pregnancy, Any
# Cols: n, % of Any

library(tidyverse)
library(flextable)
library(here)

# robust checkbox -> logical
is_checked <- function(x) x %in% c(TRUE, 1, "1", "TRUE", "True", "Checked", "Yes", "yes")

df <- treatment_data %>%
  transmute(
    side_effects = is_checked(tpt_ae_type_ses),
    illness      = is_checked(tpt_ae_type_illness),
    meds         = is_checked(tpt_ae_type_meds),
    pregnancy    = is_checked(tpt_ae_type_preg),
    none         = if ("tpt_ae_type_none" %in% names(.)) is_checked(tpt_ae_type_none) else FALSE
  ) %>%
  mutate(any_flag = side_effects | illness | meds | pregnancy)

denom_any <- sum(df$any_flag, na.rm = TRUE)

row_counts <- tibble(
  Type = c(
    "Side effects",
    "Illness - new or changed",
    "Medicines - new or changed",
    "Pregnancy",
    "Any"
  ),
  n = c(
    sum(df$side_effects & df$any_flag, na.rm = TRUE),
    sum(df$illness      & df$any_flag, na.rm = TRUE),
    sum(df$meds         & df$any_flag, na.rm = TRUE),
    sum(df$pregnancy    & df$any_flag, na.rm = TRUE),
    denom_any
  )
) %>%
  mutate(`% of Any` = if (denom_any > 0) round(n / denom_any * 100, 1) else NA_real_) %>%
  mutate(`% of Any` = if_else(denom_any > 0 & Type == "Any", 100, `% of Any`)) %>%
  select(Type, n, `% of Any`)

# Flextable (white background)
table_06.17 <- row_counts %>%
  flextable() %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%
  bold(part = "header", bold = TRUE) %>%
  bold(i = ~ Type == "Any", bold = TRUE) %>%
  align(j = c("n", "% of Any"), align = "center", part = "all") %>%
  colformat_int(j = "n", big.mark = ",") %>%
  colformat_num(j = "% of Any", digits = 1, suffix = "%") %>%
  autofit()

table_06.17

# Save
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_filename <- paste0("table_06.17_", current_date, ".png")
save_as_image(table_06.17, path = file.path(output_dir, output_filename))
