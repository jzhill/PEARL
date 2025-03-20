library(tidyverse)
library(lubridate)
library(ggplot2)

# Note: denominator is from 2020 census population data - population has grown!

# Include only villages with >= 100 people registered
plot_05.04_data <- village_data_cum %>%
  filter(village %in% villages_gte_100) %>%
  select(-date_started) %>% 
  left_join(village_data %>% select(village, date_started), by = "village") %>%
  filter(week_reg >= date_started)  # Remove points before village started

# Plot cumulative proportion by week on a single panel
plot_05.04 <- ggplot(plot_05.04_data, aes(x = week_reg, y = cum_prop, color = village)) + # cum_prop in this line
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  labs(x = "Registration Week",
       y = "Cumulative Proportion Screened",
       title = "Cumulative Proportion of Village Population Screened Over Time",
       subtitle = "Denominator: 2020 population",
       color = "Village") +
  scale_x_date(
    breaks = seq(from = min_month, to = max_week, by = "month")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    minor_breaks = seq(0, 1, by = 0.05)
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_05.04

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.04_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.04, width = 8, height = 4, dpi = 300)
