# 06.12_tpt_age_sex.R
# Summary table: age_cat × tpt_sex for treatment_data (with totals)

library(tidyverse)
library(here)
library(flextable)

# ---- Data -------------------------------------------------------
table_06.12_data <- treatment_data %>%
  filter(tpt_sex %in% c("M", "F"), !is.na(age_cat)) %>%
  mutate(
    tpt_sex = factor(tpt_sex, levels = c("M","F")),
    age_cat = factor(age_cat, levels = levels(age_cat))  # preserve existing order
  ) %>%
  count(age_cat, tpt_sex, name = "n") %>%
  complete(
    age_cat = factor(levels(treatment_data$age_cat), levels = levels(treatment_data$age_cat)),
    tpt_sex = factor(c("M","F"), levels = c("M","F")),
    fill = list(n = 0)
  ) %>%
  arrange(age_cat, tpt_sex) %>%
  pivot_wider(names_from = tpt_sex, values_from = n, values_fill = 0) %>%
  mutate(Total = M + F)

# grand-total row
total_row <- summarise(table_06.12_data,
                       age_cat = "Total",
                       M = sum(M, na.rm = TRUE),
                       F = sum(F, na.rm = TRUE),
                       Total = sum(Total, na.rm = TRUE)
)

table_06.12_data <- bind_rows(table_06.12_data, total_row)

# ---- Flextable --------------------------------------------------
table_06.12 <- table_06.12_data %>%
  rename(`Age group` = age_cat) %>%
  flextable(col_keys = c("Age group","M","F","Total")) %>%
  theme_vanilla() %>%
  bg(bg = "white", part = "all") %>%          # ensure white background for dark-mode viewers
  bold(part = "header", bold = TRUE) %>%
  bold(i = ~ `Age group` == "Total", bold = TRUE) %>%
  align(j = c("M","F","Total"), align = "center", part = "all") %>%
  colformat_int(j = c("M","F","Total"), big.mark = ",") %>%
  width(j = "Age group", width = 1.2) %>%
  autofit()

table_06.12  # displays in the Viewer

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.12_", current_date, ".png")

save_as_image(table_06.12, path = file.path(output_dir, output_filename))
