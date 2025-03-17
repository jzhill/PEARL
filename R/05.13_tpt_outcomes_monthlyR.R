# Load required libraries
library(tidyverse)
library(lubridate)

# Define readable labels for TPT outcome reasons
tpt_outcome_labels <- c(
  "Not yet assigned" = "Not yet assigned",
  "Lost to follow-up" = "Lost to Follow-up",
  "Withdrew consent" = "Withdrew Consent",
  "Discontinued" = "Discontinued (Medical Reason)",
  "Died" = "Died",
  "Completed" = "Completed TPT"
)

# Process treatment data
tpt_outcome_plot <- treatment_data %>%
  mutate(
    month = floor_date(tpt_start_date, "month"),  # Group by month
    tpt_outcome_reason = ifelse(is.na(tpt_outcome_reason), "Not yet assigned", as.character(tpt_outcome_reason))  # Handle NA values
  ) %>%
  mutate(
    tpt_outcome_reason = factor(tpt_outcome_reason, 
                                levels = names(tpt_outcome_labels),  # Ensure correct order
                                labels = tpt_outcome_labels)  # Apply readable names
  ) %>%
  group_by(month, tpt_outcome_reason) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(month) %>%
  mutate(proportion = count / sum(count))  # Convert counts to proportions

# Assign ggplot to an object
plot_05.13 <- ggplot(tpt_outcome_plot, aes(x = month, y = proportion, fill = tpt_outcome_reason)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked proportional bars
  theme_light() +
  labs(
    title = "TPT Outcome Proportions by Month",
    x = "Month",
    y = "Proportion",
    fill = "TPT Outcome"
  ) +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentages
  scale_x_date(
    date_breaks = "1 month",  # Ensure each month is labeled
    date_labels = "%b %Y"  # Format as "Jan 2024"
  ) +
  scale_fill_viridis_d(direction = -1) +  # Maintain consistent color styling
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)) # Maintain consistent color styling

# Print the plot
print(plot_05.13)

# Save plot images
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))

output_filename <- paste0("plot_05.13_", current_date, ".png")
ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.13, width = 8, height = 4, dpi = 300)
