library(tidyverse)
library(lubridate)
library(ggplot2)

# Define min and max axis ranges
min_date <- floor_date(min(village_data_cum$week_reg, na.rm = TRUE), unit = "month") - months(1)
max_date <- max(village_data_cum$week_reg, na.rm = TRUE)
max_val <- (ceiling(sum(village_data_cum$n_screened, na.rm = TRUE) / 1000) * 1000) + 1

# Create the cumulative stacked area plot (y-axis = cumulative count).
plot_05.05 <- ggplot(village_data_cum, aes(x = week_reg, y = cum_screened, fill = village)) +
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

plot_05.05