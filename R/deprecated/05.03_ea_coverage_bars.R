library(tidyverse)
library(ggplot2)

plot_05.03_data <- ea_data %>%
  filter(pop_elig_new > 50) %>%  
  mutate(record_id = fct_reorder(record_id, date_enum_new, .desc = FALSE))
  
plot_05.03_maxy <- max(plot_05.03_data$prop_reg_screen_hh, na.rm = TRUE)

if (!is.finite(plot_05.03_maxy)) {
  plot_05.03_maxy <- 1
}

plot_05.03 <- ggplot(
  plot_05.03_data,
  aes(x = record_id, y = prop_reg_screen_hh, fill = village)
) +
  geom_bar(stat = "identity") +
  labs(
    x     = "EA",
    y     = "Proportion registered (screened / eligible)",
    title = "Coverage (registered/eligible) by EA",
    fill  = "Village"
  ) +
  scale_y_continuous(
    limits = c(0, plot_05.03_maxy),
    breaks = seq(0, plot_05.03_maxy, by = 0.1),
    minor_breaks = seq(0, plot_05.03_maxy, by = 0.05),
    sec.axis = dup_axis(name = NULL)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

plot_05.03

current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.03_", current_date, ".png")

ggsave(
  filename = file.path(output_dir, output_filename),
  plot     = plot_05.03,
  width    = 8,
  height   = 4,
  dpi      = 300
)