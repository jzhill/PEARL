library(tidyverse)
library(lubridate)
library(ggplot2)
library(here)

# Cumulative coverage by village using 2023 ex-0–2 census denominator
# Only plot villages with at least 100 people registered

plot_05.04_data <- village_data_cum %>%
  # Drop the aggregate "Other or unknown" bucket
  filter(village != "Other or unknown") %>%
  # Keep villages with >= 100 registrations
  filter(!is.na(reg), reg >= 100) %>%
  # Drop any rows before that village actually started
  filter(week_reg >= first_screen_date)

# Plot cumulative proportion (vs 2023 ex-0–2 population)
plot_05.04 <- ggplot(
  plot_05.04_data,
  aes(x = week_reg, y = cum_prop_2023_ex02, color = village)
) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  labs(
    x = "Registration week",
    y = "Cumulative proportion screened",
    title = "Cumulative proportion of village population screened over time",
    subtitle = "Denominator: 2023 census population, excluding 0–2 years",
    color = "Village"
  ) +
  scale_x_date(
    breaks = seq(from = min_month, to = max_week, by = "month")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    minor_breaks = seq(0, 1, by = 0.05)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_05.04

# Save plot image ------------------------------------------------------

current_date    <- format(Sys.Date(), "%Y-%m-%d")
output_dir      <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.04_", current_date, ".png")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

ggsave(
  filename = file.path(output_dir, output_filename),
  plot     = plot_05.04,
  width    = 8,
  height   = 4,
  dpi      = 300
)
