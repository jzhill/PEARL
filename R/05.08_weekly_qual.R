library(tidyverse)
library(ggplot2)
library(lubridate)

# Prepare data for plotting
weekly_summary <- weekly_data %>%
  pivot_longer(
    cols = c(reg, tst_placed, tst_read, tbdec, sdr, anyrx, tst_read_pct, tbdec_pct, sdr_pct, anyrx_pct),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))  # Remove NAs for cleaner plotting

# Create static time series plot
plot_05.08 <- ggplot(weekly_summary, aes(x = week_reg, y = Value, color = Indicator, group = Indicator)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Aggregated Weekly Data Over Time",
       x = "Time (Weeks)",
       y = "Count or Percentage",
       color = "Indicator")

plot_05.08