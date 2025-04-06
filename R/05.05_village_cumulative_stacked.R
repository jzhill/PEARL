library(tidyverse)
library(lubridate)
library(ggplot2)

# Define min and max axis ranges
plot_05.05_maxy <- 16000

# Create the cumulative stacked area plot (y-axis = cumulative count).
plot_05.05 <- ggplot(village_data_cum, aes(x = week_reg, y = cum_screened, fill = village)) +
  geom_area() +
  labs(x = "Registration Week",
       y = "Cumulative Number Screened",
       title = "Cumulative Screening by Village",
       fill = "Village") +
  scale_x_date(
    breaks = seq(from = min_month, to = max_week, by = "month")
  ) +
  scale_y_continuous(
    limits = c(0, plot_05.05_maxy),
    breaks = (seq(0, plot_05.05_maxy, by = 1000))
    ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 15000) +
  annotate(
    "text", label = "Betio target = 15,000",
    x = min_month + months(6), y = 14500, size = 4
  )
  

plot_05.05

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.05_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.05, width = 8, height = 4, dpi = 300)
