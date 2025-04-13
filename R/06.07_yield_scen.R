library(dplyr)
library(flextable)
library(here)

# Define XR categories
xr_keep <- c("1 cw TB", "2 CAD 50 plus", "3 Uncertain", "4 Unlikely", "5 CAD under 50")
xr_positive <- c("1 cw TB", "2 CAD 50 plus", "3 Uncertain")

# Create yield table by age group
yield_table <- screening_data %>%
  filter(!is.na(age_cat), tb_decision %in% c("Presumptive TB", "Ruled out TB")) %>%
  group_by(age_cat) %>%
  summarise(
    N = n(),
    sx_n = sum(!is.na(calc_sx)),
    sx_cases = sum(calc_sx == "Sx Positive" & ntp_diagnosis == "Confirmed", na.rm = TRUE),
    xr_n = sum(calc_xr %in% xr_keep, na.rm = TRUE),
    xr_cases = sum(calc_xr %in% xr_positive & ntp_diagnosis == "Confirmed", na.rm = TRUE),
    pearl_cases = sum(ntp_diagnosis == "Confirmed", na.rm = TRUE)
  ) %>%
  mutate(
    Symptom_Yield = round(sx_cases / sx_n * 100, 2),
    Symptom_NNS = round(ifelse(sx_cases > 0, sx_n / sx_cases, Inf), 1),
    Xray_Yield = round(xr_cases / xr_n * 100, 2),
    Xray_NNS = round(ifelse(xr_cases > 0, xr_n / xr_cases, Inf), 1),
    PEARL_Yield = round(pearl_cases / N * 100, 2),
    PEARL_NNS = round(ifelse(pearl_cases > 0, N / pearl_cases, Inf), 1)
  ) %>%
  select(age_cat, N,
         Symptom_Yield, Symptom_NNS,
         Xray_Yield, Xray_NNS,
         PEARL_Yield, PEARL_NNS) %>%
  ungroup()

# Create row 10+
yield_10plus <- screening_data %>%
  filter(!is.na(age_cat), tb_decision %in% c("Presumptive TB", "Ruled out TB"), age_cat != "0-9") %>%
  summarise(
    N = n(),
    sx_n = sum(!is.na(calc_sx)),
    sx_cases = sum(calc_sx == "Sx Positive" & ntp_diagnosis == "Confirmed", na.rm = TRUE),
    xr_n = sum(calc_xr %in% xr_keep, na.rm = TRUE),
    xr_cases = sum(calc_xr %in% xr_positive & ntp_diagnosis == "Confirmed", na.rm = TRUE),
    pearl_cases = sum(ntp_diagnosis == "Confirmed", na.rm = TRUE)
  ) %>%
  mutate(
    Symptom_Yield = round(sx_cases / sx_n * 100, 2),
    Symptom_NNS = round(ifelse(sx_cases > 0, sx_n / sx_cases, Inf), 1),
    Xray_Yield = round(xr_cases / xr_n * 100, 2),
    Xray_NNS = round(ifelse(xr_cases > 0, xr_n / xr_cases, Inf), 1),
    PEARL_Yield = round(pearl_cases / N * 100, 2),
    PEARL_NNS = round(ifelse(pearl_cases > 0, N / pearl_cases, Inf), 1),
    age_cat = "Ages 10+"
  ) %>%
  select(age_cat, N,
         Symptom_Yield, Symptom_NNS,
         Xray_Yield, Xray_NNS,
         PEARL_Yield, PEARL_NNS)

# Bind total row
yield_table <- bind_rows(yield_table, yield_10plus)

# Create totals row
yield_total <- screening_data %>%
  filter(!is.na(age_cat), tb_decision %in% c("Presumptive TB", "Ruled out TB")) %>%
  summarise(
    N = n(),
    sx_n = sum(!is.na(calc_sx)),
    sx_cases = sum(calc_sx == "Sx Positive" & ntp_diagnosis == "Confirmed", na.rm = TRUE),
    xr_n = sum(calc_xr %in% xr_keep, na.rm = TRUE),
    xr_cases = sum(calc_xr %in% xr_positive & ntp_diagnosis == "Confirmed", na.rm = TRUE),
    pearl_cases = sum(ntp_diagnosis == "Confirmed", na.rm = TRUE)
  ) %>%
  mutate(
    Symptom_Yield = round(sx_cases / sx_n * 100, 2),
    Symptom_NNS = round(ifelse(sx_cases > 0, sx_n / sx_cases, Inf), 1),
    Xray_Yield = round(xr_cases / xr_n * 100, 2),
    Xray_NNS = round(ifelse(xr_cases > 0, xr_n / xr_cases, Inf), 1),
    PEARL_Yield = round(pearl_cases / N * 100, 2),
    PEARL_NNS = round(ifelse(pearl_cases > 0, N / pearl_cases, Inf), 1),
    age_cat = "All"
  ) %>%
  select(age_cat, N,
         Symptom_Yield, Symptom_NNS,
         Xray_Yield, Xray_NNS,
         PEARL_Yield, PEARL_NNS)

# Bind total row
yield_table <- bind_rows(yield_table, yield_total)

# Replace Inf with NA
yield_table <- yield_table %>%
  mutate(across(ends_with("NNS"), ~ ifelse(is.infinite(.), NA, .)))

yield_table

table_06.07 <- flextable(yield_table) %>%
  add_header_row(
    values = c("", "", "Symptom Only", "X-ray (10+)", "PEARL"),
    colwidths = c(1, 1, 2, 2, 2)
  ) %>%
  set_header_labels(
    age_cat = "Age Group",
    N = "N",
    Symptom_Yield = "Yield (%)",
    Symptom_NNS = "NNS",
    Xray_Yield = "Yield (%)",
    Xray_NNS = "NNS",
    PEARL_Yield = "Yield (%)",
    PEARL_NNS = "NNS"
  ) %>%
  theme_vanilla() %>%
  align(j = c("N", 
              "Symptom_Yield", "Symptom_NNS",
              "Xray_Yield", "Xray_NNS",
              "PEARL_Yield", "PEARL_NNS"),
        align = "center", part = "all") %>%
  colformat_num(j = c("N",
                      "Symptom_Yield", "Symptom_NNS",
                      "Xray_Yield", "Xray_NNS",
                      "PEARL_Yield", "PEARL_NNS"),
                digits = 2, na_str = "–") %>%
  set_table_properties(layout = "autofit")

table_06.07

# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.07_", current_date, ".png")

save_as_image(table_06.07, path = file.path(output_dir, output_filename))
