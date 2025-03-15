library(tidyverse)
library(lubridate)
library(ggplot2)

# Include only villages with >= 100 people registered
village_data_cum_gte_100 <- village_data_cum %>%
  filter(village %in% villages_gte_100) %>%
  select(-date_started) %>% 
  left_join(village_data %>% select(village, date_started), by = "village") %>%
  filter(week_reg >= date_started)  # Remove points before village started

# Define min and max date range for x-axis
min_date <- floor_date(min(village_data_cum_gte_100$week_reg, na.rm = TRUE), unit = "month") - months(1)
max_date <- floor_date(max(village_data_cum_gte_100$week_reg, na.rm = TRUE), unit = "month") + months(1)

# Plot cumulative proportion by week on a single panel
plot_05.04 <- ggplot(village_data_cum_gte_100, aes(x = week_reg, y = cum_prop, color = village)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Registration Week",
       y = "Cumulative Proportion Screened",
       title = "Cumulative Proportion of Village Population Screened Over Time",
       subtitle = "Denominator: enumerated eligible population",
       color = "Village") +
  scale_x_date(
    breaks = seq(from = min_date, to = max_date, by = "month")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    minor_breaks = seq(0, 1, by = 0.05)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_05.04
