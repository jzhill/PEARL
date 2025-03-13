library(ggplot2)
library(dplyr)
library(tidyr)

# Process data in a single pipeline
proportion_data <- screening_data %>% 
  filter(week_reg >= (max(week_reg, na.rm = TRUE) - months(6))) %>% 
  mutate(tb_decision = as.character(tb_decision),  # Ensure it's a character first
         tb_decision = replace(tb_decision, is.na(tb_decision), "Missing"),
         tb_decision = factor(tb_decision, levels = c("Missing","TB status uncertain", "Ruled out TB", "Presumptive TB"))) %>% 
  group_by(week_reg, tb_decision) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(week_reg) %>% 
  mutate(proportion = count / sum(count))

# Plot the stacked bar chart
ggplot(proportion_data, aes(x = week_reg, y = proportion, fill = tb_decision)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Missing" = "lightgrey", "TB status uncertain" = "palegreen3", "Ruled out TB" = "lightblue", "Presumptive TB" = "lightcoral")) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  labs(x = "Week of Registration", y = "Proportion", 
       title = "Weekly proportion of TST result (Last 6 Months)",
       fill = "TST result") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
