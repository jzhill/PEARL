library(dplyr)
library(tidyr)
library(flextable)

# Step 1: Summarise by age and sex
tst_summary_long <- screening_data %>%
  filter(!is.na(age_cat), en_sex %in% c("M", "F"), !is.na(tst_read_bin)) %>%
  mutate(
    tst_positive = tst_read_positive == "Positive TST",
    valid_tst = tst_read_bin == TRUE
  ) %>%
  group_by(age_cat, en_sex) %>%
  summarise(
    positive_n = sum(tst_positive & valid_tst, na.rm = TRUE),
    valid_n = sum(valid_tst, na.rm = TRUE),
    percent_positive = round(100 * positive_n / valid_n, 1),
    .groups = "drop"
  )

# Step 2: Pivot to wide format
tst_summary_wide <- tst_summary_long %>%
  pivot_wider(
    names_from = en_sex,
    values_from = c(positive_n, percent_positive, valid_n),
    names_glue = "{en_sex}_{.value}"
  ) %>%
  arrange(age_cat)

# Step 3: Define column layout
col_keys <- c("age_cat", 
              "M_positive_n", "M_percent_positive", "M_valid_n",
              "F_positive_n", "F_percent_positive", "F_valid_n")

header_labels <- c(
  age_cat = "Age Group",
  M_positive_n = "n", M_percent_positive = "%",
  M_valid_n = "Valid n",
  F_positive_n = "n", F_percent_positive = "%",
  F_valid_n = "Valid n"
)

# Step 4: Create flextable
flextable(tst_summary_wide, col_keys = col_keys) %>%
  set_header_labels(values = header_labels) %>%
  add_header_row(
    values = c("Age Group", "Positive TST", "%", "Valid n",
               "Positive TST", "%", "Valid n"),
    colwidths = rep(1, 7)
  ) %>%
  add_header_row(
    values = c("Age Group", "Male", "Female"),
    colwidths = c(1, 3, 3)
  ) %>%
  merge_v(part = "header") %>%
  theme_booktabs() %>%
  bg(bg = "white", part = "all") %>%
  autofit()
