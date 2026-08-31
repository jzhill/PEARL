library(tidyverse)
library(lubridate)
library(ggplot2)

# Note: only villages that have been completed will have valid eligible population data
# Ignore any villages in the figure that have NOT been completely enumerated

# Include only villages with >= 100 people registered
village_data_cum_gte_100 <- village_data_cum %>%
  filter(village %in% villages_gte_100) %>%
  select(-first_screen_date) %>%
  left_join(village_data %>% select(village, first_screen_date), by = "village") %>%
  filter(week_reg >= first_screen_date)  # Remove points before village started

# Define min and max date range for x-axis
min_date <- floor_date(min(village_data_cum_gte_100$week_reg, na.rm = TRUE), unit = "month") - months(1)
max_date <- floor_date(max(village_data_cum_gte_100$week_reg, na.rm = TRUE), unit = "month") + months(1)

# Plot cumulative proportion by week on a single panel
plot_05.11 <- ggplot(village_data_cum_gte_100, aes(x = week_reg, y = cum_prop_elig_new, color = village)) + # cum_prop_elig_new on this line
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Registration Week",
       y = "Cumulative Proportion Screened",
       title = "Cumulative Proportion of Village Population Screened Over Time",
       subtitle = "Denominator: eligible population",
       color = "Village") +
  scale_x_date(
    breaks = seq(from = min_date, to = max_date, by = "month")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    minor_breaks = seq(0, 1, by = 0.05)
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_05.11

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.11_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.11, width = 8, height = 5, dpi = 300)
