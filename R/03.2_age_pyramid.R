library(apyramid)

screening_data %>%
  filter(en_sex %in% c("M", "F")) %>% 
  mutate(en_sex = droplevels(en_sex)) %>%
  age_pyramid(
    age_group = "age_cat",
    split_by = "en_sex"
  )

print(nrow(screening_data))
print(nrow(screening_data %>% filter(en_sex %in% c("M", "F"))))
