library(apyramid)

plot_05.02_data <- screening_data %>%
  filter(en_sex %in% c("M", "F"), !is.na(age_cat)) %>% 
  mutate(en_sex = factor(en_sex, levels = c("M", "F")))

plot_05.02 <- age_pyramid(plot_05.02_data, age_group = "age_cat", split_by = "en_sex") +
  scale_fill_viridis_d(option = "F", begin = 0.4, end = 0.6) +
  labs(
    x = "Count",
    y = "Age category"
  ) +
  labs(title = "PEARL participant population pyramid", x = NULL) +
  theme_light() +
  theme(legend.title = element_blank())  # Remove legend title

plot_05.02

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.02_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.02, width = 8, height = 5, dpi = 300)
