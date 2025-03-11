library(tidyverse)
library(lubridate)
library(ggplot2)

##############################################
######## Enumerated eligible population denominator

## Note that these figures use village cumulative count data based on join of village from household
## Village captured during screening is inaccurate

# Calculate village population denominators from ea_data
village_pop <- ea_data %>%
  group_by(village) %>%
  summarise(total_pop = sum(pop_elig, na.rm = TRUE), .groups = "drop")

# Calculate cumulative screened counts by village and week from screening_data
cumulative_data <- screening_data %>%
  filter(!is.na(week_reg), !is.na(res_village_hh), res_village_hh != "") %>%
  group_by(res_village_hh, week_reg) %>%
  summarise(n_screened = n(), .groups = "drop") %>%
  arrange(res_village_hh, week_reg) %>%
  group_by(res_village_hh) %>%
  mutate(cum_screened = cumsum(n_screened)) %>%
  ungroup()

# Identify villages with a maximum cumulative total > 100
villages_to_include <- cumulative_data %>%
  group_by(res_village_hh) %>%
  summarise(max_screened = max(cum_screened, na.rm = TRUE), .groups = "drop") %>%
  filter(max_screened > 100) %>%
  pull(res_village_hh)

# Join village population and calculate cumulative proportion
cumulative_data <- cumulative_data %>%
  left_join(village_pop, by = c("res_village_hh" = "village")) %>%
  mutate(cum_prop = cum_screened / total_pop) %>%
  filter(res_village_hh %in% villages_to_include)

# Define min and max date range for x-axis
min_date <- floor_date(min(cumulative_data$week_reg, na.rm = TRUE), unit = "month") - months(1)
max_date <- floor_date(max(cumulative_data$week_reg, na.rm = TRUE), unit = "month") + months(1)

# Plot cumulative proportion by week on a single panel
ggplot(cumulative_data, aes(x = week_reg, y = cum_prop, color = res_village_hh)) +
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

##############################################
######## 2020 census denominator

# Calculate village population denominators from ea_data
village_pop <- ea_data %>%
  group_by(village) %>%
  summarise(total_pop = sum(pop_2020, na.rm = TRUE), .groups = "drop")

# Calculate cumulative screened counts by village and week from screening_data
cumulative_data <- screening_data %>%
  filter(!is.na(week_reg), !is.na(res_village_hh), res_village_hh != "") %>%
  group_by(res_village_hh, week_reg) %>%
  summarise(n_screened = n(), .groups = "drop") %>%
  arrange(res_village_hh, week_reg) %>%
  group_by(res_village_hh) %>%
  mutate(cum_screened = cumsum(n_screened)) %>%
  ungroup()

# Identify villages with a maximum cumulative total > 100
villages_to_include <- cumulative_data %>%
  group_by(res_village_hh) %>%
  summarise(max_screened = max(cum_screened, na.rm = TRUE), .groups = "drop") %>%
  filter(max_screened > 100) %>%
  pull(res_village_hh)

# Join village population and calculate cumulative proportion
cumulative_data <- cumulative_data %>%
  left_join(village_pop, by = c("res_village_hh" = "village")) %>%
  mutate(cum_prop = cum_screened / total_pop) %>%
  filter(res_village_hh %in% villages_to_include)

# Define min and max date range for x-axis
min_date <- floor_date(min(cumulative_data$week_reg, na.rm = TRUE), unit = "month") - months(1)
max_date <- floor_date(max(cumulative_data$week_reg, na.rm = TRUE), unit = "month") + months(1)

# Plot cumulative proportion by week on a single panel
ggplot(cumulative_data, aes(x = week_reg, y = cum_prop, color = res_village_hh)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Registration Week",
       y = "Cumulative Proportion Screened",
       title = "Cumulative Proportion of Village Population Screened Over Time",
       subtitle = "Denominator: 2020 census total population",
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

