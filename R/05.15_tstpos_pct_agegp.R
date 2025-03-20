library(ggplot2)
library(dplyr)

# Filter valid data and calculate percentage of Positive TST by age group
tst_summary <- screening_data %>%
  filter(!is.na(tst_read_positive) & !is.na(age_cat)) %>%
  group_by(age_cat) %>%
  summarise(
    pct_positive = mean(tst_read_positive == "Positive TST", na.rm = TRUE) * 100,
    n = n()
  ) 

# Plot percentage of Positive TST by age group
ggplot(tst_summary, aes(x = age_cat, y = pct_positive)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +  # Bar chart
  geom_text(aes(label = sprintf("%.1f%%", pct_positive)), vjust = -0.5, size = 4) +  # Show % labels
  labs(
    x = "Age Category (10-year groups)",
    y = "Percentage with Positive TST",
    title = "Proportion of Positive TST by Age Group",
    subtitle = "Percentage of individuals with TST ≥ positive cutoff per age category"
  ) +
  theme_light()
