library(dplyr)
library(tidyr)
library(flextable)

# Step 1: Summarise leprosy results by age group and sex
lep_summary_long <- screening_data %>%
  filter(!is.na(age_cat), en_sex %in% c("M", "F"), !is.na(lep_refer)) %>%
  mutate(
    presumptive_lep = lep_refer == TRUE,
    confirmed_lep = nlp_diagnosis == "Confirmed",
    valid_lep = !is.na(lep_refer)
  ) %>%
  group_by(age_cat, en_sex) %>%
  summarise(
    presumptive_n = sum(presumptive_lep, na.rm = TRUE),
    confirmed_n = sum(confirmed_lep, na.rm = TRUE),
    valid_n = sum(valid_lep, na.rm = TRUE),
    percent_presumptive = round(100 * presumptive_n / valid_n, 1),
    percent_confirmed = round(100 * confirmed_n / valid_n, 1),
    .groups = "drop"
  )

# Step 2: Pivot to wide format
lep_summary_wide <- lep_summary_long %>%
  pivot_wider(
    names_from = en_sex,
    values_from = c(presumptive_n, confirmed_n, valid_n, percent_presumptive, percent_confirmed),
    names_glue = "{en_sex}_{.value}"
  ) %>%
  arrange(age_cat)

# Step 3: Define column order and labels
col_keys <- c("age_cat", 
              "M_presumptive_n", "M_percent_presumptive", 
              "M_confirmed_n", "M_percent_confirmed", 
              "M_valid_n",
              "F_presumptive_n", "F_percent_presumptive", 
              "F_confirmed_n", "F_percent_confirmed", 
              "F_valid_n")

header_labels <- c(
  age_cat = "Age Group",
  M_presumptive_n = "n", M_percent_presumptive = "%",
  M_confirmed_n = "n", M_percent_confirmed = "%",
  M_valid_n = "Valid n",
  F_presumptive_n = "n", F_percent_presumptive = "%",
  F_confirmed_n = "n", F_percent_confirmed = "%",
  F_valid_n = "Valid n"
)

# Step 4: Create flextable
flextable(lep_summary_wide, col_keys = col_keys) %>%
  set_header_labels(values = header_labels) %>%
  add_header_row(
    values = c("Age Group", "Presumptive Leprosy", "%", "Confirmed Leprosy", "%", "Valid n",
               "Presumptive Leprosy", "%", "Confirmed Leprosy", "%", "Valid n"),
    colwidths = rep(1, 11)
  ) %>%
  add_header_row(
    values = c("Age Group", "Male", "Female"),
    colwidths = c(1, 5, 5)
  ) %>%
  merge_v(part = "header") %>%
  theme_booktabs() %>%
  bg(bg = "white", part = "all") %>%
  autofit()
