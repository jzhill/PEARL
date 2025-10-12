# 06.15_tpt_sx_resolution_by_time.R
# Resolution of side effects by timepoint among those who reported any side-effect at that timepoint

library(tidyverse)
library(flextable)
library(here)

timepoints <- c("1m","3m","4m","ae")

# ---- Build tidy long from explicit cols -------------------------------------
sxres_long <- treatment_data %>%
  select(record_id, tpt_1m_sxres, tpt_3m_sxres, tpt_4m_sxres, tpt_ae_sxres) %>%
  pivot_longer(
    cols = -record_id,
    names_to = "tp",
    values_to = "sxres",
    names_pattern = "^tpt_(1m|3m|4m|ae)_sxres$"
  )

any_long <- treatment_data %>%
  select(record_id, `1m_any_true`, `3m_any_true`, `4m_any_true`, ae_any_true) %>%
  pivot_longer(
    cols = -record_id,
    names_to = "tp",
    values_to = "any_true",
    names_pattern = "^(1m|3m|4m|ae)_any_true$"
  )

dat_long <- sxres_long %>%
  inner_join(any_long, by = c("record_id", "tp")) %>%
  filter(any_true %in% TRUE)  # only those who reported sx at that tp

# ---- Count resolved / not resolved ------------------------------------------
tp_counts <- dat_long %>%
  mutate(
    Row = case_when(
      sxres == "Yes - all resolved" ~ "Resolved at time of assessment",
      sxres == "No - ongoing"       ~ "Not resolved",
      TRUE                          ~ "Not resolved"   # count NA/other as not resolved so totals add up
    )
  ) %>%
  count(tp, Row, name = "n") %>%
  complete(tp, Row = c("Resolved at time of assessment","Not resolved"), fill = list(n = 0)) %>%
  group_by(tp) %>%
  bind_rows(summarise(., Row = "Total", n = sum(n), .groups = "drop")) %>%
  ungroup()

# ---- Reshape to requested layout --------------------------------------------
mat_tbl <- tp_counts %>%
  mutate(tp = factor(tp, levels = timepoints)) %>%
  pivot_wider(names_from = tp, values_from = n, values_fill = 0) %>%
  rename(
    `1 month` = `1m`,
    `3 month` = `3m`,
    `4 month` = `4m`,
    `AE form` = `ae`
  ) %>%
  rename(`Resolution status` = Row) %>%
  arrange(factor(`Resolution status`, levels = c("Resolved at time of assessment","Not resolved","Total")))

# ---- Flextable ---------------------------------------------------------------
table_06.15 <- mat_tbl %>%
  flextable() %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%
  bold(part = "header") %>%
  bold(i = ~ `Resolution status` == "Total") %>%
  align(j = c("1 month","3 month","4 month","AE form"), align = "center", part = "all") %>%
  colformat_int(j = c("1 month","3 month","4 month","AE form"), big.mark = ",") %>%
  autofit()

table_06.15

# ---- Save flextable ----------------------------------------------------------
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_filename <- paste0("table_06.15_", current_date, ".png")
save_as_image(table_06.15, path = file.path(output_dir, output_filename))
