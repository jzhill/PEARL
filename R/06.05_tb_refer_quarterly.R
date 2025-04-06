library(tidyverse)
library(lubridate)
library(flextable)
library(here)
library(officer)

# Step 1: Filter for TB decision presumptive and create "YYYY Qn" format
tb_ref_raw <- screening_data %>%
  filter(tb_decision == "Presumptive TB") %>%
  mutate(
    quarter_start = floor_date(en_date_visit, "quarter"),
    quarter = paste0(year(quarter_start), " Q", quarter(quarter_start)),
    ntp_diagnosis = fct_explicit_na(ntp_diagnosis, na_level = "Missing")
  )

# Step 2: Generate ordered quarter labels up to current
quarter_seq <- seq.Date(
  from = min(tb_ref_raw$quarter_start, na.rm = TRUE),
  to = floor_date(Sys.Date(), "quarter"),
  by = "3 months"
)
quarter_labels <- paste0(year(quarter_seq), " Q", quarter(quarter_seq))

# Step 3: Count outcomes, ensure all quarters are present, and order quarters
tb_ref_counts <- tb_ref_raw %>%
  count(quarter, ntp_diagnosis) %>%
  complete(
    quarter = factor(quarter_labels, levels = quarter_labels, ordered = TRUE),
    ntp_diagnosis,
    fill = list(n = 0)
  ) %>%
  pivot_wider(names_from = ntp_diagnosis, values_from = n, values_fill = 0) %>%
  arrange(quarter)

# Add total column
tb_ref_counts <- tb_ref_counts %>%
  mutate(Total = rowSums(select(., -quarter)))

# Add total row
total_row <- tb_ref_counts %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(quarter = "Total")

tb_ref_counts <- bind_rows(tb_ref_counts, total_row)

# Compute row-wise percentages
tb_ref_props <- tb_ref_counts %>%
  mutate(across(-c(quarter, Total), ~ round(.x / ifelse(Total == 0, 1, Total) * 100, 1)))

# Combine into final formatted table
tb_ref_final <- tb_ref_counts %>%
  mutate(across(
    -c(quarter, Total),
    ~ paste0(.x, " (", tb_ref_props[[cur_column()]], "%)")
  )) %>%
  rename(`Already on DOTS` = `Currently on TB treatment`)

# Optional: Construct a custom title using the latest quarter
latest_quarter <- tb_ref_counts$quarter[tb_ref_counts$quarter != "Total"] %>% 
  as.character() %>% 
  max()

table_06.05 <- tb_ref_final %>%
  flextable() %>%
  
  # Add a title row across all columns
  add_header_row(
    values = paste("TB Referral Outcomes up to", latest_quarter),
    colwidths = ncol(tb_ref_final)
  ) %>%
  
  # General styling
  theme_vanilla() %>%
  bg(part = "header", bg = "#F2F2F2") %>%     # Light grey background for header
  bg(part = "body", bg = "white") %>%        # White background for body
  bold(part = "header") %>%                  # Bold headers
  bold(i = nrow(tb_ref_final), part = "body") %>%  # Bold total row
  font(part = "header", fontname = "Arial") %>%
  font(part = "body", fontname = "Arial") %>%
  fontsize(size = 12, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  line_spacing(space = 1.3, part = "header") %>%
  
  # Borders
  border(part = "all", border = fp_border(color = "black", width = 0.5)) %>%
  border_inner(border = fp_border(color = "black", width = 0.5)) %>%
  border_outer(border = fp_border(color = "black", width = 1.2)) %>%
  
  # Alignment and width
  align(align = "center", part = "all") %>%
  align(j = "quarter", align = "left", part = "all") %>%
  width(width = 1.5)

table_06.05

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.05_", current_date, ".png")

save_as_image(table_06.05, path = file.path(output_dir, output_filename))
