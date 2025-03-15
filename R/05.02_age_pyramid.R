library(apyramid)

plot_05.02 <- screening_data %>%
  filter(en_sex %in% c("M", "F"), !is.na(age_cat)) %>% 
  mutate(en_sex = factor(en_sex, levels = c("M", "F"))) %>%
  age_pyramid(
    age_group = "age_cat",
    split_by = "en_sex"
  ) +
  scale_fill_viridis_d(option = "F", begin = 0.4, end = 0.6) +  # Apply viridis color scale
  labs(
    x = "Count",
    y = "Age category"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())  # Remove legend title



plot_05.02