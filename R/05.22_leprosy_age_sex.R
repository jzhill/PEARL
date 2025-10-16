# --- Leprosy age/sex column chart: Screened Positive (left) vs Confirmed ----

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(here)

lep_plot_df <- screening_data %>%
  filter(!is.na(age_cat), en_sex %in% c("M","F")) %>%
  mutate(
    screened_positive = lep_refer == TRUE,
    confirmed_lep     = nlp_diagnosis == "Confirmed",
    valid_lep_outcome = !is.na(lep_refer)      # decision recorded (TRUE/FALSE)
  ) %>%
  group_by(age_cat, en_sex) %>%
  summarise(
    valid_outcome_n = sum(valid_lep_outcome, na.rm = TRUE),
    presumptive_n   = sum(screened_positive, na.rm = TRUE),
    confirmed_n     = sum(confirmed_lep,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_presumptive = if_else(valid_outcome_n > 0, 100 * presumptive_n / valid_outcome_n, NA_real_),
    percent_confirmed   = if_else(valid_outcome_n > 0, 100 * confirmed_n   / valid_outcome_n, NA_real_)
  ) %>%
  pivot_longer(
    c(percent_presumptive, percent_confirmed),
    names_to = "metric", values_to = "percent"
  ) %>%
  mutate(
    metric = recode(metric,
                    percent_presumptive = "Screened Positive",
                    percent_confirmed   = "Confirmed Leprosy"
    ),
    # keep left panel = "Screened Positive"
    metric = factor(metric, levels = c("Screened Positive", "Confirmed Leprosy")),
    en_sex = factor(en_sex, levels = c("F","M"), labels = c("Female","Male"))
  ) %>%
  arrange(age_cat)

# Shared y-axis cap (rounded to 10s) across BOTH panels
y_max <- suppressWarnings(max(lep_plot_df$percent, na.rm = TRUE))
y_cap <- if (is.finite(y_max)) ceiling(y_max / 5) * 5 else 100
y_cap <- max(y_cap, 5)

plot_05.22 <- ggplot(lep_plot_df, aes(x = age_cat, y = percent, fill = en_sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, na.rm = TRUE) +
  facet_wrap(~ metric, nrow = 1, scales = "fixed") +   # shared y-axis
  scale_y_continuous(
    limits = c(0, y_cap),
    breaks = seq(0, y_cap, by = max(1, y_cap/10)),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_viridis_d(option = "F", begin = 0.2, end = 0.8) +
  labs(
    title  = "Leprosy screening outcomes by age group and sex",
    x      = "Age group",
    y      = "Percent of those with a leprosy decision recorded",
    fill   = "Sex"
  ) +
  theme_light() +
  theme(
    plot.title   = element_text(size = 11),
    axis.title   = element_text(size = 10),
    axis.text    = element_text(size = 9),
    legend.title = element_blank(),
    legend.text  = element_text(size = 9),
    strip.text   = element_text(size = 10),
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )

# Display
plot_05.22

# Save
current_date    <- format(Sys.Date(), "%Y-%m-%d")
output_dir      <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.22_lep_age_sex_", current_date, ".png")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(output_dir, output_filename),
  plot = plot_05.22,
  width = 9.5, height = 4.6, dpi = 300
)
