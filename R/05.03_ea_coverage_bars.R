library(tidyverse)
library(ggplot2)

ea_data %>%
  mutate(prop_reg = pop_reg_screen / pop_elig) %>%  # Calculate the proportion
  filter(pop_elig > 50) %>%                           # Only include EAs with >50 eligible individuals
  mutate(record_id = forcats::fct_reorder(record_id, date_enum_new, .desc = FALSE)) %>%
  ggplot(aes(x = record_id, y = prop_reg, fill = village)) +
  geom_bar(stat = "identity") +
  labs(x = "EA", y = "Proportion Registered", 
       title = "Proportion of Population Registered by EA",
       fill = "village") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
