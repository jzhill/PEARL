library(apyramid)

plot_05.02 <- screening_data %>%
  filter(en_sex %in% c("M", "F")) %>% 
  mutate(en_sex = droplevels(en_sex)) %>%
  age_pyramid(
    age_group = "age_cat",
    split_by = "en_sex"
  )

plot_05.02