# Load required libraries
library(tidyverse)
library(lubridate)
library(flextable)

# Define readable labels for calc_any_treatment
treatment_labels <- c(
  "SDR" = "SDR",
  "TPT" = "TPT",
  "TBRx" = "TBRx",
  "MDT" = "MDT",
  "No treatment" = "No treatment"
)

# Step 1: Create a tibble with counts by month
treatment_counts <- screening_data %>%
  mutate(
    month = floor_date(en_date_visit, "month"),  # Convert to month grouping
    calc_any_treatment = ifelse(is.na(calc_any_treatment), "No treatment", as.character(calc_any_treatment))  # Handle NA values
  ) %>%
  mutate(
    calc_any_treatment = factor(calc_any_treatment, 
                                levels = names(treatment_labels),  # Ensure correct order
                                labels = treatment_labels)  # Apply readable names
  ) %>%
  count(month, calc_any_treatment) %>%
  pivot_wider(names_from = calc_any_treatment, values_from = n, values_fill = 0) %>%  # Reshape into wide format
  arrange(month)

# Convert month column to character (formatted as "Jan 2024")
treatment_counts <- treatment_counts %>%
  mutate(month = format(as.Date(month), "%b %Y"))

# Add "Any treatment" column as sum of SDR, TPT, TBRx, and MDT
treatment_counts <- treatment_counts %>%
  mutate(
    `Any treatment` = SDR + TPT + TBRx + MDT,  # Sum of all treatment types
    Total = `Any treatment` + `No treatment`  # Ensure consistency
  ) %>%
  select(month, SDR, TPT, TBRx, MDT, `Any treatment`, `No treatment`, Total)  # Reorder columns

# Compute total for each column (excluding "month" and "Total") and add as a summary row
total_row <- treatment_counts %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(month = "Total")

# Combine table and total row
treatment_counts <- bind_rows(treatment_counts, total_row)

# Step 2: Compute **row-wise proportions**, ensuring each row sums to 100% (excluding "Total" column)
treatment_proportions <- treatment_counts %>%
  mutate(across(-c(month, Total), ~ round(.x / ifelse(Total == 0, 1, Total) * 100, 1)))  # Row-wise percentages

# Step 3: Create a new tibble with "count (percent%)" formatting
treatment_final_table <- treatment_counts %>%
  mutate(across(
    -c(month, Total), 
    ~ paste0(.x, " (", treatment_proportions[[cur_column()]], "%)")
  ))

# Step 4: Convert to flextable with improved styling
table_06.03 <- treatment_final_table %>%
  flextable() %>%
  set_caption("Treatment Proportions by Month (Including Any Treatment)") %>%
  theme_vanilla() %>%
  bg(part = "header", bg = "#F2F2F2") %>%  # Light grey header background
  bg(part = "body", bg = "white") %>%
  bold(part = "header") %>%               # Bold header text
  autofit() %>%  # Auto-adjust column widths
  bold(part = "header") %>%
  bold(i = nrow(treatment_final_table), part = "body") %>%  # Bold the total row
  align(align = "center", part = "all")  # Center-align all columns

# Print flextable
table_06.03

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.03_", current_date, ".png")

save_as_image(table_06.03, path = file.path(output_dir, output_filename))
