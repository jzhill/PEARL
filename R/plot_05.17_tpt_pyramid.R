# 05.02_tpt_age_pyramid.R
# Population pyramid for people in treatment_data only

library(tidyverse)
library(here)
library(apyramid)

# ---- Data -------------------------------------------------------
plot_05.17_data <- treatment_data %>%
  filter(tpt_sex %in% c("M","F"), !is.na(age_cat)) %>%
  mutate(tpt_sex = factor(tpt_sex, levels = c("M","F")))

# ---- Plot -------------------------------------------------------
plot_05.17 <- age_pyramid(
  plot_05.17_data,
  age_group = "age_cat",
  split_by  = "tpt_sex"
) +
  scale_fill_viridis_d(option = "F", begin = 0.4, end = 0.7) +
  labs(
    title = "Age–sex distribution of people on TPT",
    subtitle = "Treatment dataset only",
    x = "Count",
    y = "Age category"
  ) +
  theme_light() +
  theme(legend.title = element_blank())

plot_05.17

# ---- Save -------------------------------------------------------
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = file.path(output_dir, paste0("plot_05.17_", current_date, ".png")),
  plot = plot_05.17, width = 8, height = 5, dpi = 300
)
