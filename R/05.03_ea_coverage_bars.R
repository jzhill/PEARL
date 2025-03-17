library(tidyverse)
library(ggplot2)

plot_05.03 <- ea_data %>%
  mutate(prop_reg = pop_reg_screen / pop_elig) %>%  # Calculate the proportion
  filter(pop_elig > 50) %>%                           # Only include EAs with >50 eligible individuals
  mutate(record_id = forcats::fct_reorder(record_id, date_enum_new, .desc = FALSE)) %>%
  ggplot(aes(x = record_id, y = prop_reg, fill = village)) +
  geom_bar(stat = "identity") +
  labs(x = "EA", y = "Proportion Registered", 
       title = "Proportion of Population Registered by EA",
       fill = "village") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_05.03

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.03_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.03, width = 8, height = 4, dpi = 300)
