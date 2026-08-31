library(tidyverse)
library(lubridate)
library(flextable)
library(here)
library(officer)

# Step 1: Filter for leprosy referrals and create "YYYY Qn" format
lep_ref_raw <- screening_data %>%
  filter(lep_refer == TRUE) %>%
  mutate(
    quarter_start = floor_date(en_date_visit, "quarter"),
    quarter = paste0(year(quarter_start), " Q", quarter(quarter_start)),
    nlp_diagnosis = fct_explicit_na(nlp_diagnosis, na_level = "Missing")
  )

# Step 2: Generate ordered quarter labels up to current
quarter_seq <- seq.Date(
  from = min(lep_ref_raw$quarter_start, na.rm = TRUE),
  to = floor_date(Sys.Date(), "quarter"),
  by = "3 months"
)
quarter_labels <- paste0(year(quarter_seq), " Q", quarter(quarter_seq))

# Step 3: Count outcomes, ensure all quarters are present, and order quarters
lep_ref_counts <- lep_ref_raw %>%
  count(quarter, nlp_diagnosis) %>%
  complete(
    quarter = factor(quarter_labels, levels = quarter_labels, ordered = TRUE),
    nlp_diagnosis,
    fill = list(n = 0)
  ) %>%
  pivot_wider(names_from = nlp_diagnosis, values_from = n, values_fill = 0) %>%
  arrange(quarter)

# Add total column
lep_ref_counts <- lep_ref_counts %>%
  mutate(Total = rowSums(select(., -quarter)))

# Add total row
total_row <- lep_ref_counts %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(quarter = "Total")

lep_ref_counts <- bind_rows(lep_ref_counts, total_row)

# Compute row-wise percentages
lep_ref_props <- lep_ref_counts %>%
  mutate(across(-c(quarter, Total), ~ round(.x / ifelse(Total == 0, 1, Total) * 100, 1)))

# Combine into final formatted table
lep_ref_final <- lep_ref_counts %>%
  mutate(across(
    -c(quarter, Total),
    ~ paste0(.x, " (", lep_ref_props[[cur_column()]], "%)")
  ))

# Optional: Construct a custom title using the latest quarter
latest_quarter <- lep_ref_counts$quarter[lep_ref_counts$quarter != "Total"] %>% 
  as.character() %>% 
  max()

table_06.04 <- lep_ref_final %>%
  flextable() %>%
  
  # Add a title row across all columns
  add_header_row(
    values = paste("Leprosy Referral Outcomes up to", latest_quarter),
    colwidths = ncol(lep_ref_final)
  ) %>%
  
  # General styling
  theme_vanilla() %>%
  bg(part = "header", bg = "#F2F2F2") %>%     # Light grey background for header
  bg(part = "body", bg = "white") %>%        # White background for body
  bold(part = "header") %>%                  # Bold headers
  bold(i = nrow(lep_ref_final), part = "body") %>%  # Bold total row
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
  width(j = 1, width = 2.2) %>%  # Wider first column ("quarter")
  autofit()

table_06.04

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.04_", current_date, ".png")

save_as_image(table_06.04, path = file.path(output_dir, output_filename))
