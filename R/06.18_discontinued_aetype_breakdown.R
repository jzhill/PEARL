# 06.18_discontinued_aetype_breakdown.R
# Discontinued cohort: AE-type + group ever flags + Any reported + None reported + Total

library(tidyverse)
library(flextable)
library(here)

# --- Filter cohort: exactly "Discontinued" -----------------------------------
df_disc <- treatment_data %>%
  filter(tpt_outcome_reason == "Discontinued")

total_n <- nrow(df_disc)

# --- Flags from explicit logical columns (NA -> FALSE) ------------------------
flags <- df_disc %>%
  transmute(
    ses  = coalesce(tpt_ae_type_ses,      FALSE),
    ill  = coalesce(tpt_ae_type_illness,  FALSE),
    meds = coalesce(tpt_ae_type_meds,     FALSE),
    preg = coalesce(tpt_ae_type_preg,     FALSE),
    csx  = coalesce(csx_any_ever,         FALSE),
    dili = coalesce(dili_sx_any_ever,     FALSE),
    rhs  = coalesce(rhs_sx_any_ever,      FALSE)
  ) %>%
  mutate(
    any_reported  = ses | ill | meds | preg | csx | dili | rhs,
    none_reported = !any_reported
  )

rows_order <- c(
  "Side effects",
  "Illness - new or changed",
  "Medicines - new or changed",
  "Pregnancy",
  "Common side effects (ever)",
  "DILI symptoms (ever)",
  "Rifamycin hypersensitivity (ever)",
  "Any reported",
  "None reported",
  "Total"
)

row_counts <- tibble(
  Type = rows_order,
  n = c(
    sum(flags$ses),
    sum(flags$ill),
    sum(flags$meds),
    sum(flags$preg),
    sum(flags$csx),
    sum(flags$dili),
    sum(flags$rhs),
    sum(flags$any_reported),
    sum(flags$none_reported),
    total_n
  )
) %>%
  mutate(`%` = if (total_n > 0) round(100 * n / total_n, 1) else NA_real_,
         `%` = if_else(Type == "Total" & total_n > 0, 100, `%`))

# --- Flextable ---------------------------------------------------------------
table_06.18 <- row_counts %>%
  flextable() %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%
  bold(part = "header") %>%
  bold(i = ~ Type %in% c("Any reported","None reported","Total")) %>%  # highlight the summary trio
  align(j = c("n", "%"), align = "center", part = "all") %>%
  colformat_int(j = "n", big.mark = ",") %>%
  colformat_num(j = "%", digits = 1, suffix = "%") %>%
  autofit()

table_06.18

# --- Save --------------------------------------------------------------------
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_filename <- paste0("table_06.18_", current_date, ".png")
save_as_image(table_06.18, path = file.path(output_dir, output_filename))
