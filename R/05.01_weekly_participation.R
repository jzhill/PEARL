library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# Determine the maximum week to be the most recent Monday (or start of the week)
max_week <- floor_date(Sys.Date(), unit = "week", week_start = 1)

# 1. Screening Data: using en_date_visit
screening_weekly <- screening_data %>%  
  drop_na(en_date_visit) %>%  
  mutate(week = floor_date(en_date_visit, unit = "week", week_start = 1)) %>%  
  filter(week <= max_week) %>%   # drop any future weeks
  count(week) %>%  
  complete(
    week = seq.Date(
      from = min(week, na.rm = TRUE), 
      to = max_week,
      by = "week"
    ),
    fill = list(n = 0)
  ) %>%  
  mutate(source = "Screening")

# 2. Household Data: using hh_date
household_weekly <- household_data %>%  
  drop_na(hh_date) %>%  
  mutate(week = floor_date(hh_date, unit = "week", week_start = 1)) %>%  
  filter(week <= max_week) %>%   # drop any future weeks
  count(week) %>%  
  complete(
    week = seq.Date(
      from = min(week, na.rm = TRUE), 
      to = max_week,
      by = "week"
    ),
    fill = list(n = 0)
  ) %>%  
  mutate(source = "Household")

# 3. Treatment Data: using tpt_start_date
treatment_weekly <- treatment_data %>%  
  drop_na(tpt_start_date) %>%  
  mutate(week = floor_date(tpt_start_date, unit = "week", week_start = 1)) %>%  
  filter(week <= max_week) %>%   # drop any future weeks
  count(week) %>%  
  complete(
    week = seq.Date(
      from = min(week, na.rm = TRUE), 
      to = max_week,
      by = "week"
    ),
    fill = list(n = 0)
  ) %>%  
  mutate(source = "Treatment")

# Combine the weekly counts from all sources
combined_weekly <- bind_rows(screening_weekly, household_weekly, treatment_weekly)

# Plot the combined counts as a dodged bar chart
plot_05.01 <- ggplot(combined_weekly, aes(x = week, y = n, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Weekly Activity as of", Sys.Date()),
       x = "Week",
       y = "n",
       fill = "Activity Type") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_05.01