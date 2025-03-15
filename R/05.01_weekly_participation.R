# Load necessary libraries
library(ggplot2)
library(dplyr)

# Replace zero values with NA for plotting
weekly_data_na <- weekly_data %>%
  mutate(
    hh_enum = ifelse(hh_enum == 0, NA, hh_enum),
    reg = ifelse(reg == 0, NA, reg),
    tpt_start = ifelse(tpt_start == 0, NA, tpt_start)
  )

# Define the plot
plot_05.01 <- ggplot(weekly_data_na, aes(x = week_reg)) +
  
  geom_line(aes(y = hh_enum, color = "Households Enumerated"), size = 0.6) +
  geom_point(aes(y = hh_enum, color = "Households Enumerated"), size = 2, na.rm = TRUE) +
  
  geom_line(aes(y = reg, color = "People Registered"), size = 0.6) +
  geom_point(aes(y = reg, color = "People Registered"), size = 2, na.rm = TRUE) +
  
  geom_line(aes(y = tpt_start, color = "People Started on TPT"), size = 0.6) +
  geom_point(aes(y = tpt_start, color = "People Started on TPT"), size = 2, na.rm = TRUE) +
  
  scale_color_viridis_d(option = "F", begin = 0.2, end = 0.8) +

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
