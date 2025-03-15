# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the plot
plot_05.01 <- ggplot(weekly_data, aes(x = week_reg)) +
  
  geom_line(aes(y = hh_enum, color = "Households Enumerated"), size = 0.6) +
  geom_point(aes(y = hh_enum, color = "Households Enumerated"), size = 2, na.rm = TRUE) +
  
  geom_line(aes(y = reg, color = "People Registered"), size = 0.6) +
  geom_point(aes(y = reg, color = "People Registered"), size = 2, na.rm = TRUE) +
  
  geom_line(aes(y = tpt_start, color = "People Started on TPT"), size = 0.6) +
  geom_point(aes(y = tpt_start, color = "People Started on TPT"), size = 2, na.rm = TRUE) +
  
  scale_color_viridis_d(option = "F", begin = 0.2, end = 0.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  
  labs(
    title = "Weekly activity: households enumerated, people registered, people commenced on TPT",
    x = "Week",
    y = "Count",
    color = "Indicator"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

plot_05.01
