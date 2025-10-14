library(dplyr)
library(ggplot2)

# Prep data: calculate proportions in each age category
prop_by_age <- screening_data %>%
  filter(!is.na(tst_read_mm), !is.na(age_cat)) %>%
  group_by(age_cat) %>%
  summarise(
    n = n(),
    prop_5mm = mean(tst_read_mm >= 5),
    prop_10mm = mean(tst_read_mm >= 10)
  )

# Get overall proportions
overall <- screening_data %>%
  filter(!is.na(tst_read_mm)) %>%
  summarise(
    prop_5mm = mean(tst_read_mm >= 5),
    prop_10mm = mean(tst_read_mm >= 10)
  )

# Convert data for ggplot (long format for both thresholds)
library(tidyr)
prop_long <- prop_by_age %>%
  pivot_longer(
    cols = starts_with("prop_"),
    names_to = "threshold",
    values_to = "proportion"
  ) %>%
  mutate(
    threshold = recode(threshold,
                       prop_5mm = "≥5mm",
                       prop_10mm = "≥10mm"),
    threshold = factor(threshold, levels = c("≥5mm", "≥10mm"))
  )

# Set explicit color map
color_map <- c("≥5mm" = "coral4", "≥10mm" = "turquoise4")

plot_05.17 <- ggplot(prop_long, aes(x = age_cat, y = proportion, group = threshold, color = threshold, shape = threshold)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = overall$prop_5mm, linetype = "dashed", color = color_map["≥5mm"], size = 1) +
  geom_hline(yintercept = overall$prop_10mm, linetype = "dashed", color = color_map["≥10mm"], size = 1) +
  scale_color_manual(values = color_map) +
  scale_shape_manual(values = c("≥5mm" = 1, "≥10mm" = 4)) +
  labs(
    x = "Age Category",
    y = "Proportion with Skin Test ≥ Threshold",
    color = "Threshold",
    shape = "Threshold",
    title = "Proportion of Participants with Skin Test ≥ 5mm or ≥ 10mm by Age Category"
  ) +
  theme_light() +
  theme(legend.position = "top")

plot_05.17

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.17_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.17, width = 8, height = 5, dpi = 300)
