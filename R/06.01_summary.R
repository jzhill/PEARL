library(dplyr)
library(tibble)
library(flextable)

# Create summary table
summary_table <- tibble(
  Metric = names(metrics),
  Current_Week = sapply(metrics, function(x) x$week),
  Total_Count = sapply(metrics, function(x) x$total)
)

# Display as flextable with title
table_06.01 <- summary_table %>%
  flextable() %>%
  set_header_labels(Metric = "Indicator", Current_Week = "This Week", Total_Count = "Total Count") %>%
  add_header_lines(values = paste("Summary of PEARL activity for week starting", format(current_week, "%Y-%m-%d"))) %>%
  theme_vanilla() %>%
  autofit()

# Print flextable
table_06.01
