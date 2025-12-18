library(dplyr)
library(flextable)
library(officer)
library(here)

# Define positive and valid Xpert results
xpert_any <- c("Not detected", "Trace detected", "Detected")
xpert_positive <- c("Trace detected", "Detected")

# 1. Age-group cascade (main table)
sputum_cascade <- screening_data %>%
  filter(!is.na(age_cat)) %>%
  group_by(age_cat) %>%
  summarise(
    n_registered = n(),
    sputum_collected = sum(spuxpt_taken == "Yes", na.rm = TRUE),
    sputum_tested = sum(spuxpt_labreq_lab, na.rm = TRUE),
    xpert_result = sum(spuxpt_mtb_res_lab %in% xpert_any, na.rm = TRUE),
    xpert_positive = sum(spuxpt_mtb_res_lab %in% xpert_positive, na.rm = TRUE)
  ) %>%
  ungroup()

# 2. Subtotal for age 10+ (excluding "0-9")
sputum_10plus <- screening_data %>%
  filter(!is.na(age_cat), age_cat != "0-9") %>%
  summarise(
    age_cat = "Ages 10+",
    n_registered = n(),
    sputum_collected = sum(spuxpt_taken == "Yes", na.rm = TRUE),
    sputum_tested = sum(spuxpt_labreq_lab, na.rm = TRUE),
    xpert_result = sum(spuxpt_mtb_res_lab %in% xpert_any, na.rm = TRUE),
    xpert_positive = sum(spuxpt_mtb_res_lab %in% xpert_positive, na.rm = TRUE)
  )

# 3. Total for all ages
sputum_total <- screening_data %>%
  filter(!is.na(age_cat)) %>%
  summarise(
    age_cat = "All",
    n_registered = n(),
    sputum_collected = sum(spuxpt_taken == "Yes", na.rm = TRUE),
    sputum_tested = sum(spuxpt_labreq_lab, na.rm = TRUE),
    xpert_result = sum(spuxpt_mtb_res_lab %in% xpert_any, na.rm = TRUE),
    xpert_positive = sum(spuxpt_mtb_res_lab %in% xpert_positive, na.rm = TRUE)
  )

# 4. Combine into a single table
sputum_table <- bind_rows(sputum_cascade, sputum_10plus, sputum_total)

# 5. Make totals row bold in flextable
total_rows <- which(sputum_table$age_cat %in% c("Ages 10+", "All"))

# 6. Create and style flextable
table_06.08 <- sputum_table %>%
  flextable() %>%
  theme_vanilla() %>%
  bg(part = "header", bg = "#F2F2F2") %>%
  bg(part = "body", bg = "white") %>%
  bold(part = "header") %>%
  bold(i = total_rows, part = "body") %>%
  font(part = "header", fontname = "Arial") %>%
  font(part = "body", fontname = "Arial") %>%
  fontsize(size = 12, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  line_spacing(space = 1.3, part = "header") %>%
  border(part = "all", border = fp_border(color = "black", width = 0.5)) %>%
  border_inner(border = fp_border(color = "black", width = 0.5)) %>%
  border_outer(border = fp_border(color = "black", width = 1.2)) %>%
  align(align = "center", part = "all") %>%
  align(j = "age_cat", align = "left", part = "all") %>%
  width(width = 1.5)

table_06.08

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.08_", current_date, ".png")

save_as_image(table_06.08, path = file.path(output_dir, output_filename))
