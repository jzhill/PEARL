# 05.24_tpt_sx_age_sex.R
# Any symptom during TPT monitoring by age group and sex
library(dplyr)
library(ggplot2)
library(scales)
library(here)

# Build plot data: % with ANY symptom among those who started TPT
tpt_sx_plot_df <- treatment_data %>%
  filter(tpt_sex %in% c("M","F"), !is.na(age_cat), !is.na(tpt_start_date)) %>%
  mutate(
    # match order & labels used in 05.23 (Female first)
    tpt_sex = factor(tpt_sex, levels = c("F","M"), labels = c("Female","Male")),
    # any symptom seen at 1m OR 3m OR 4m OR AE
    any_sx = coalesce(`1m_any_true`, FALSE) |
      coalesce(`3m_any_true`, FALSE) |
      coalesce(`4m_any_true`, FALSE) |
      coalesce(`ae_any_true`, FALSE)
  ) %>%
  group_by(age_cat, tpt_sex) %>%
  summarise(
    started_n = n(),
    sx_n      = sum(any_sx, na.rm = TRUE),
    percent_sx = if_else(started_n > 0, 100 * sx_n / started_n, NA_real_),
    .groups = "drop"
  )

# Axis headroom (rounded up to the next 5%), same approach as 05.23
y_max <- suppressWarnings(max(tpt_sx_plot_df$percent_sx, na.rm = TRUE))
y_cap <- if (is.finite(y_max)) max(5, ceiling(y_max / 5) * 5) else 5
break_step <- max(1, y_cap / 10)

plot_05.24 <- ggplot(
  tpt_sx_plot_df,
  aes(x = age_cat, y = percent_sx, fill = tpt_sex)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_y_continuous(
    limits = c(0, y_cap),
    breaks = seq(0, y_cap, by = break_step),
    labels = function(x) paste0(x, "%")
  ) +
  # match colour styling to 05.23
  scale_fill_viridis_d(option = "F", begin = 0.2, end = 0.8, name = "Sex") +
  labs(
    title = "Any symptom during TPT monitoring by age group and sex",
    x = "Age group",
    y = "Any symptom (%)"
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
plot_05.24

# Save
current_date    <- format(Sys.Date(), "%Y-%m-%d")
output_dir      <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.24_", current_date, ".png")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(output_dir, output_filename),
  plot = plot_05.24,
  width = 4, height = 4, dpi = 300
)
