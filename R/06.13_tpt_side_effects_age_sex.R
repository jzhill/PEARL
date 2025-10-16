# 06.13_tpt_side_effects_age_sex.R
# Age × Sex table for patients on TPT who experienced any side effect
# Uses: treatment_data$age_cat (factor), treatment_data$tpt_sex, treatment_data$sx_any_ever (logical)

library(tidyverse)
library(flextable)
library(here)

# ---- Filter: sx_any_ever == TRUE --------------------------------------------
side_effect_data <- treatment_data %>%
  filter(
    sx_any_ever == TRUE,
    tpt_sex %in% c("M","F"),
    !is.na(age_cat)
  ) %>%
  mutate(
    tpt_sex = factor(tpt_sex, levels = c("M","F")),
    age_cat = factor(age_cat, levels = levels(treatment_data$age_cat))
  )

# ---- Build counts table (Age × Sex) -----------------------------------------
tbl_counts <- side_effect_data %>%
  count(age_cat, tpt_sex, name = "n") %>%
  complete(
    age_cat = factor(levels(treatment_data$age_cat), levels = levels(treatment_data$age_cat)),
    tpt_sex = factor(c("M","F"), levels = c("M","F")),
    fill    = list(n = 0)
  ) %>%
  arrange(age_cat, tpt_sex) %>%
  pivot_wider(names_from = tpt_sex, values_from = n, values_fill = 0) %>%
  mutate(Total = M + F)

# Grand total row
total_row <- summarise(tbl_counts,
                       age_cat = "Total",
                       M       = sum(M, na.rm = TRUE),
                       F       = sum(F, na.rm = TRUE),
                       Total   = sum(Total, na.rm = TRUE)
)

tbl_counts <- bind_rows(tbl_counts, total_row)

# ---- Flextable --------------------------------------------------------------
title_text <- paste0(
  "Patients reporting symptoms during TPT by age and sex — generated ", format(Sys.Date(), "%Y-%m-%d")
)

table_06.13 <- tbl_counts %>%
  rename(`Age group` = age_cat) %>%
  flextable(col_keys = c("Age group","M","F","Total")) %>%
  add_header_lines(values = title_text) %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%              # white background for dark-mode readability
  bold(part = "header", bold = TRUE) %>%
  bold(i = ~ `Age group` == "Total", bold = TRUE) %>%
  align(j = c("M","F","Total"), align = "center", part = "all") %>%
  colformat_int(j = c("M","F","Total"), big.mark = ",") %>%
  autofit()

table_06.13  # display

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.13_", current_date, ".png")

save_as_image(table_06.13, path = file.path(output_dir, output_filename))
