library(ggplot2)
library(dplyr)
library(tidyr)

# Process data in a single pipeline
proportion_data <- screening_data %>% 
  filter(week_reg >= (max(week_reg, na.rm = TRUE) - months(6))) %>% 
  mutate(tst_read_positive = as.character(tst_read_positive),  # Ensure it's a character first
         tst_read_positive = replace(tst_read_positive, is.na(tst_read_positive), "Missing"),
         tst_read_positive = factor(tst_read_positive, levels = c("Missing", "Negative TST", "Positive TST"))) %>% 
  group_by(week_reg, tst_read_positive) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(week_reg) %>% 
  mutate(proportion = count / sum(count))

# Plot the stacked bar chart
plot_05.10 <- ggplot(proportion_data, aes(x = week_reg, y = proportion, fill = tst_read_positive)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Missing" = "lightgrey", "Negative TST" = "lightblue", "Positive TST" = "lightcoral")) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  labs(x = "Week of Registration", y = "Proportion", 
       title = "Weekly proportion of TST result (Last 6 Months)",
       fill = "TST result") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_05.10