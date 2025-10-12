# Load required libraries
library(tidyverse)
library(lubridate)
library(flextable)

# Define readable labels for TPT outcome reasons
tpt_outcome_labels <- c(
  "Completed" = "Completed TPT",
  "Died" = "Died",
  "Discontinued" = "Discontinued (Medical Reason)",
  "Lost to follow-up" = "Lost to Follow-up",
  "Withdrew consent" = "Withdrew Consent",
  "Not yet assigned" = "Not yet assigned"
)

# ---- Exclusion window --------------------------------------------------------
# Define the cutoff as "4 months ago" from today.
cutoff_date <- Sys.Date() %m-% months(4)

# Keep all rows EXCEPT those that:
#   - started on/after cutoff_date (i.e., within last 4 months), AND
tpt_filtered <- treatment_data %>%
  filter(!is.na(tpt_start_date), tpt_start_date < cutoff_date)


# Create a tibble with counts by month
tpt_counts <- tpt_filtered %>%
  mutate(
    month = floor_date(tpt_start_date, "month"),  # Convert to month grouping
    tpt_outcome_reason = ifelse(is.na(tpt_outcome_reason), "Not yet assigned", as.character(tpt_outcome_reason))  # Handle NA values
  ) %>%
  mutate(
    tpt_outcome_reason = factor(tpt_outcome_reason, 
                                levels = names(tpt_outcome_labels),  # Ensure correct order
                                labels = tpt_outcome_labels)  # Apply readable names
  ) %>%
  count(month, tpt_outcome_reason) %>%
  pivot_wider(names_from = tpt_outcome_reason, values_from = n, values_fill = 0) %>%  # Reshape into wide format
  arrange(month)

# Convert month column to character (formatted as "Jan 2024")
tpt_counts <- tpt_counts %>%
  mutate(month = format(as.Date(month), "%b %Y"))

# Add a "Total" column per row
tpt_counts <- tpt_counts %>%
  mutate(Total = rowSums(select(., -month)))

# Compute total for each column (excluding "month" and "Total") and add as a summary row
total_row <- tpt_counts %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(month = "Total")

# Combine table and total row
tpt_counts <- bind_rows(tpt_counts, total_row)

# Step 2: Compute **row-wise proportions**, ensuring each row sums to 100% (excluding "Total" column)
tpt_proportions <- tpt_counts %>%
  mutate(across(-c(month, Total), ~ round(.x / ifelse(Total == 0, 1, Total) * 100, 1)))  # Row-wise percentages

# Step 3: Create a new tibble with "count (percent%)" formatting
tpt_final_table <- tpt_counts %>%
  mutate(across(
    -c(month, Total), 
    ~ paste0(.x, " (", tpt_proportions[[cur_column()]], "%)")
  ))

# Step 4: Convert to flextable with improved styling
table_06.02 <- tpt_final_table %>%
  flextable() %>%
  set_caption("TPT Outcome Counts by Month (With Totals and Row-Wise Percentages)") %>%
  theme_vanilla() %>%
  bg(part = "header", bg = "#F2F2F2") %>%  # Light grey header background
  bg(part = "body", bg = "white") %>%
  bold(part = "header") %>%               # Bold header text
  autofit() %>%  # Auto-adjust column widths
  bold(part = "header") %>%  # Bold the header row
  bold(i = nrow(tpt_final_table), part = "body") %>%  # Bold the total row
  align(align = "center", part = "all")  # Center-align all columns

# Print flextable
table_06.02

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.02_", current_date, ".png")

save_as_image(table_06.02, path = file.path(output_dir, output_filename))
