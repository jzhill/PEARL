# Load required libraries
library(tidyverse)
library(lubridate)

# Define readable labels for calc_any_treatment
treatment_labels <- c(
  "No treatment" = "No treatment",
  "MDT" = "MDT",
  "TBRx" = "TBRx",
  "TPT" = "TPT",
  "SDR" = "SDR"
)

# Process screening data
treatment_plot_data <- screening_data %>%
  mutate(
    month = floor_date(en_date_visit, "month"),  # Group by month
    calc_any_treatment = ifelse(is.na(calc_any_treatment), "No treatment", as.character(calc_any_treatment))  # Handle NA values
  ) %>%
  mutate(
    calc_any_treatment = factor(calc_any_treatment, 
                                levels = names(treatment_labels),  # Reversed order: No treatment on top
                                labels = treatment_labels)  # Apply readable names
  ) %>%
  group_by(month, calc_any_treatment) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(month) %>%
  mutate(proportion = count / sum(count))  # Convert counts to proportions

# Define custom colors with No Treatment as light grey
custom_colors <- c(
  "No treatment" = "lightgrey",
  "MDT" = "#FDE725FF",
  "TBRx" = "#35B779FF",
  "TPT" = "#31688EFF",
  "SDR" = "#440154FF"
)

# Assign ggplot to an object
plot_05.14 <- ggplot(treatment_plot_data, aes(x = month, y = proportion, fill = calc_any_treatment)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked proportional bars
  theme_light() +
  labs(
    title = "Treatment Proportions by Month",
    x = "Month",
    y = "Proportion",
    fill = "Treatment Type"
  ) +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentages
  scale_x_date(
    date_breaks = "1 month",  # Ensure each month is labeled
    date_labels = "%b %Y"  # Format as "Jan 2024"
  ) +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Improve readability
  )

# Print the plot
print(plot_05.14)

# Save plot images
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))

output_filename <- paste0("plot_05.14_", current_date, ".png")
ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.14, width = 8, height = 4, dpi = 300)

