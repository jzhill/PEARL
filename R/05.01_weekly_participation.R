library(ggplot2)
library(dplyr)
library(here)

plot_05.01 <- ggplot(weekly_data, aes(x = period_start)) +
  geom_line(aes(y = hh_enum_new,   color = "Households Enumerated"), size = 0.8) +
  geom_point(aes(y = hh_enum_new,  color = "Households Enumerated"), size = 1.5, na.rm = TRUE) +
  geom_line(aes(y = reg,       color = "People Registered"),     size = 0.8) +
  geom_point(aes(y = reg,      color = "People Registered"),     size = 1.5, na.rm = TRUE) +
  geom_line(aes(y = tpt_start, color = "People Started on TPT"), size = 0.8) +
  geom_point(aes(y = tpt_start,color = "People Started on TPT"), size = 1.5, na.rm = TRUE) +
  scale_color_viridis_d(option = "F", begin = 0.2, end = 0.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Weekly activity: households enumerated, people registered, people commenced on TPT",
    x = "Week",
    y = "Count",
    color = "Indicator"
  ) +
  theme_light() +
  theme(
    plot.title   = element_text(size = 10),
    axis.title   = element_text(size = 10),
    axis.text    = element_text(size = 9),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.text  = element_text(size = 9),
    legend.position = "bottom"
  )

plot_05.01

current_date  <- format(Sys.Date(), "%Y-%m-%d")
output_dir    <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.01_", current_date, ".png")

# ensure folder exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(filename = file.path(output_dir, output_filename),
       plot = plot_05.01, width = 8, height = 4, dpi = 300)
