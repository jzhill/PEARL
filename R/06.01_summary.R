library(dplyr)
library(tibble)
library(flextable)
library(here)
library(officer)

# Create summary table
summary_table <- tibble(
  Indicator = names(indicators),
  This_Week = sapply(indicators, function(x) x$week),
  Total_Count = sapply(indicators, function(x) x$total)
)

# Format and enhance flextable
table_06.01 <- summary_table %>%
  flextable() %>%
  
  # Header rows
  set_header_labels(Indicator = "Indicator", This_Week = "This Week", Total_Count = "Total Count") %>%
  add_header_row(
    values = paste("Summary of PEARL activity for week starting", format(max_week, "%Y-%m-%d")),
    colwidths = 3  # Title spans all three columns
  ) %>%
  
  # Styling Enhancements
  theme_vanilla() %>%
  bg(part = "header", bg = "#F2F2F2") %>%  # Light grey header background
  bg(part = "body", bg = "white") %>%
  bold(part = "header") %>%               # Bold header text
  bold(part = "body", j = "Indicator") %>% # Bold first column (Indicator)
  font(part = "header", fontname = "Arial") %>% 
  font(part = "body", fontname = "Arial") %>%
  fontsize(size = 12, part = "header") %>%  # Slightly larger header font
  fontsize(size = 10, part = "body") %>%    # Standard body font
  line_spacing(space = 1.3, part = "header") %>%  # Ensures enough space for title text
  align(j = c("This_Week", "Total_Count"), align = "center", part = "all") %>%  # Center align numbers
  
  # Border Fixes
  border(part = "all", border = fp_border(color = "black", width = 0.5)) %>%
  border_inner(border = fp_border(color = "black", width = 0.5)) %>%  # Inner cell borders
  border_outer(border = fp_border(color = "black", width = 1.2)) %>%  # Outer table border
  
  # Adjust column widths
  width(j = "Indicator", width = 3.5) %>%  
  width(j = c("This_Week", "Total_Count"), width = 1.5) %>%
  autofit()

table_06.01

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.01_", current_date, ".png")

save_as_image(table_06.01, path = file.path(output_dir, output_filename))
