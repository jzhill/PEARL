library(ggplot2)
library(dplyr)
library(tidyr)

# Process data in a single pipeline
plot_05.09_data <- screening_data %>% 
  filter(week_reg >= (max(week_reg[!is.na(week_reg)]) %m-% months(6))) %>%
  mutate(tb_decision = as.character(tb_decision),
         tb_decision = replace(tb_decision, is.na(tb_decision), "Missing"),
         tb_decision = factor(tb_decision, levels = c("Missing","TB status uncertain", "Ruled out TB", "Presumptive TB"))) %>% 
  group_by(week_reg, tb_decision) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(week_reg) %>% 
  mutate(proportion = count / sum(count))

# Plot the stacked bar chart
plot_05.09 <- ggplot(plot_05.09_data, aes(x = week_reg, y = proportion, fill = tb_decision)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Missing" = "lightgrey", "TB status uncertain" = "palegreen3", "Ruled out TB" = "lightblue", "Presumptive TB" = "lightcoral")) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  labs(x = "Week of Registration", y = "Proportion", 
       title = "Weekly proportion of TB outcome (Last 6 Months)",
       fill = "TB decision") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_05.09

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.09_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.09, width = 8, height = 5, dpi = 300)
