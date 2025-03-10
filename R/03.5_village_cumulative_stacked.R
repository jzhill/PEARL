library(tidyverse)
library(lubridate)
library(ggplot2)

# Ensure missing or blank res_village_hh are recoded before processing
screening_data <- screening_data %>%
  mutate(res_village_hh = if_else(is.na(res_village_hh) | res_village_hh == "", "Unknown", res_village_hh))

# Define overall date range based on week_reg in screening_data
overall_range <- seq.Date(
  from = min(screening_data$week_reg, na.rm = TRUE),
  to = max(screening_data$week_reg, na.rm = TRUE),
  by = "week"
)

# Aggregate weekly counts for each village and complete missing weeks
weekly_counts <- screening_data %>%
  filter(!is.na(week_reg)) %>%
  group_by(res_village_hh, week_reg) %>%
  summarise(n_screened = n(), .groups = "drop") %>%
  arrange(res_village_hh, week_reg) %>%
  group_by(res_village_hh) %>%
  complete(week_reg = overall_range, fill = list(n_screened = 0)) %>%
  ungroup()

# Calculate cumulative screening per village for the original groups
weekly_counts <- weekly_counts %>%
  group_by(res_village_hh) %>%
  arrange(week_reg) %>%
  mutate(cum_screened = cumsum(n_screened)) %>%
  ungroup()

# Identify small villages (excluding "Unknown") with maximum cumulative total <= 100
small_villages <- weekly_counts %>%
  group_by(res_village_hh) %>%
  summarise(max_cum = max(cum_screened, na.rm = TRUE), .groups = "drop") %>%
  filter(max_cum <= 100, res_village_hh != "Unknown") %>%
  pull(res_village_hh)

# Recode the res_village_hh column: if a village is small, recode as "Other"
weekly_counts_recoded <- weekly_counts %>%
  mutate(res_village_hh = if_else(res_village_hh %in% small_villages, "Other", res_village_hh))

# Since multiple small villages become "Other", re-aggregate by res_village_hh and week_reg
stacked_data <- weekly_counts_recoded %>%
  group_by(res_village_hh, week_reg) %>%
  summarise(n_screened = sum(n_screened, na.rm = TRUE), .groups = "drop") %>%
  arrange(res_village_hh, week_reg) %>%
  group_by(res_village_hh) %>%
  mutate(cum_screened = cumsum(n_screened)) %>%
  ungroup()

# Define min and max axis ranges
min_date <- floor_date(min(cumulative_data$week_reg, na.rm = TRUE), unit = "month") - months(1)
max_date <- max(screening_data$week_reg, na.rm = TRUE)
max_val <- (ceiling(sum(stacked_data$n_screened, na.rm = TRUE) / 1000) * 1000) + 1

# Create the cumulative stacked area plot (y-axis = cumulative count).
ggplot(stacked_data, aes(x = week_reg, y = cum_screened, fill = res_village_hh)) +
  geom_area() +
  labs(x = "Registration Week",
       y = "Cumulative Number Screened",
       title = "Cumulative Screening by Village",
       fill = "Village") +
  scale_x_date(
    breaks = seq(from = min_date, to = max_date, by = "month")
  ) +
  scale_y_continuous(
    limits = c(0, max_val),
    breaks = (seq(0, max_val, by = 1000))
    ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
