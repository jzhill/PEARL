# 05.23_tst_age_sex.R
library(dplyr)
library(ggplot2)
library(scales)
library(here)

# Prepare plot data: % Positive TST among those with a read TST
tst_plot_df <- screening_data %>%
  filter(!is.na(age_cat), en_sex %in% c("M", "F"), tst_read_bin == TRUE) %>%
  group_by(age_cat, en_sex) %>%
  summarise(
    valid_n = n(),
    positive_n = sum(tst_read_positive == "Positive TST", na.rm = TRUE),
    percent_positive = if_else(valid_n > 0, 100 * positive_n / valid_n, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    en_sex = factor(en_sex, levels = c("F","M"), labels = c("Female","Male"))
  )

# Axis headroom (rounded up to the next 5%)
y_max <- suppressWarnings(max(tst_plot_df$percent_positive, na.rm = TRUE))
y_cap <- if (is.finite(y_max)) max(5, ceiling(y_max / 5) * 5) else 5
break_step <- max(1, y_cap / 10)

plot_05.23_tst_age_sex <- ggplot(
  tst_plot_df,
  aes(x = age_cat, y = percent_positive, fill = en_sex)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_y_continuous(
    limits = c(0, y_cap),
    breaks = seq(0, y_cap, by = break_step),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_viridis_d(option = "F", begin = 0.2, end = 0.8, name = "Sex") +
  labs(
    title = "TST positivity by age group and sex",
    x = "Age group",
    y = "Positive TST (%)"
  ) +
  theme_light() +
  theme(
    plot.title   = element_text(size = 11),
    axis.title   = element_text(size = 10),
    axis.text    = element_text(size = 9),
    legend.position = "bottom",
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )

# Display
plot_05.23_tst_age_sex

# Save
current_date    <- format(Sys.Date(), "%Y-%m-%d")
output_dir      <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.23_tst_age_sex_", current_date, ".png")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(output_dir, output_filename),
  plot = plot_05.23_tst_age_sex,
  width = 8, height = 4.2, dpi = 300
)
