library(dplyr)
library(tidyr)
library(flextable)

# Prepare long summary
tb_summary_long <- screening_data %>%
  filter(!is.na(age_cat), en_sex %in% c("M", "F")) %>%
  mutate(
    presumptive_tb = tb_decision == "Presumptive TB",
    confirmed_tb = ntp_diagnosis == "Confirmed",
    valid_tb_outcome = tb_decision %in% c("Presumptive TB", "Ruled out TB")
  ) %>%
  group_by(age_cat, en_sex) %>%
  summarise(
    presumptive_n = sum(presumptive_tb, na.rm = TRUE),
    confirmed_n = sum(confirmed_tb, na.rm = TRUE),
    valid_outcome_n = sum(valid_tb_outcome, na.rm = TRUE),
    percent_presumptive = round(100 * presumptive_n / valid_outcome_n, 1),
    percent_confirmed = round(100 * confirmed_n / valid_outcome_n, 1),
    .groups = "drop"
  )

# Pivot to wide format
tb_summary_wide <- tb_summary_long %>%
  pivot_wider(
    names_from = en_sex,
    values_from = c(presumptive_n, confirmed_n, valid_outcome_n, percent_presumptive, percent_confirmed),
    names_glue = "{en_sex}_{.value}"
  ) %>%
  arrange(age_cat)

# Define the correct column order and header labels
col_keys <- c("age_cat", 
              "M_presumptive_n", "M_percent_presumptive", 
              "M_confirmed_n", "M_percent_confirmed", 
              "M_valid_outcome_n", 
              "F_presumptive_n", "F_percent_presumptive", 
              "F_confirmed_n", "F_percent_confirmed", 
              "F_valid_outcome_n")

header_labels <- c(
  age_cat = "Age Group",
  M_presumptive_n = "n", M_percent_presumptive = "%",
  M_confirmed_n = "n", M_percent_confirmed = "%",
  M_valid_outcome_n = "Valid n",
  F_presumptive_n = "n", F_percent_presumptive = "%",
  F_confirmed_n = "n", F_percent_confirmed = "%",
  F_valid_outcome_n = "Valid n"
)

# Create flextable
flextable(tb_summary_wide, col_keys = col_keys) %>%
  set_header_labels(values = header_labels) %>%
  add_header_row(
    values = c("Age Group", "Presumptive TB", "%", "Confirmed TB", "%", "Valid n",
               "Presumptive TB", "%", "Confirmed TB", "%", "Valid n"),
    colwidths = rep(1, 11)  # Each header value spans exactly 1 column
  ) %>%
  add_header_row(
    values = c("Age Group", "Male", "Female"),
    colwidths = c(1, 5, 5)
  ) %>%
  merge_v(part = "header") %>%
  theme_booktabs() %>%
  bg(bg = "white", part = "all") %>%
  autofit()