library(tidyverse)
library(flextable)

# Step 1: Filter valid (non-missing) lep_scabies results
scabies_data <- screening_data %>%
  filter(!is.na(lep_scabies), en_sex %in% c("M", "F")) %>%
  mutate(age_cat = fct_explicit_na(age_cat, na_level = "Missing"))

# Step 2: Summarise counts and calculate proportions
scabies_summary <- scabies_data %>%
  group_by(age_cat, en_sex) %>%
  summarise(
    n_total = n(),
    n_positive = sum(lep_scabies == TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    proportion = round(n_positive / n_total * 100, 1)
  )

# Step 3: Pivot to wide format with M/F columns
scabies_wide <- scabies_summary %>%
  select(age_cat, en_sex, proportion) %>%
  pivot_wider(names_from = en_sex, values_from = proportion, values_fill = NA)

# Step 4: Add row-wise (age_cat) overall across sexes
scabies_by_age <- scabies_data %>%
  group_by(age_cat) %>%
  summarise(
    n_total = n(),
    n_positive = sum(lep_scabies == TRUE),
    Overall = round(n_positive / n_total * 100, 1)
  )

# Step 5: Merge age_cat-wise overall with M/F columns
scabies_final <- scabies_wide %>%
  left_join(scabies_by_age %>% select(age_cat, Overall), by = "age_cat") %>%
  arrange(age_cat)

# Step 6: Add bottom row with column-wise overall
scabies_by_sex <- scabies_data %>%
  group_by(en_sex) %>%
  summarise(
    n_total = n(),
    n_positive = sum(lep_scabies == TRUE),
    .groups = "drop"
  ) %>%
  mutate(proportion = round(n_positive / n_total * 100, 1)) %>%
  pivot_wider(names_from = en_sex, values_from = proportion)

# Overall across all participants
overall_total <- scabies_data %>%
  summarise(
    n_total = n(),
    n_positive = sum(lep_scabies == TRUE)
  ) %>%
  mutate(Overall = round(n_positive / n_total * 100, 1))

# Create total row
total_row <- tibble(
  age_cat = "Overall",
  M = scabies_by_sex$M,
  F = scabies_by_sex$F,
  Overall = overall_total$Overall
)

# Bind total row
scabies_final_all <- bind_rows(scabies_final, total_row)

# Step 7: Format as styled flextable
table_06.06 <- scabies_final_all %>%
  flextable() %>%
  add_header_row(
    values = "Proportion of Participants with Scabies by Age Group and Sex (Among Those with Non-Missing Result)",
    colwidths = ncol(scabies_final_all)
  ) %>%
  theme_vanilla() %>%
  bg(part = "header", bg = "#F2F2F2") %>%
  bg(part = "body", bg = "white") %>%
  bold(part = "header") %>%
  bold(i = nrow(scabies_final_all), part = "body") %>%
  font(part = "header", fontname = "Arial") %>%
  font(part = "body", fontname = "Arial") %>%
  fontsize(size = 12, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  line_spacing(space = 1.3, part = "header") %>%
  align(align = "center", part = "all") %>%
  width(j = 1, width = 2.5) %>%
  autofit()

table_06.06

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.06_", current_date, ".png")

save_as_image(table_06.06, path = file.path(output_dir, output_filename))

