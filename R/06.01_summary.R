library(dplyr)
library(lubridate)
library(flextable)

# Define the current week
current_week <- floor_date(Sys.Date(), "week", week_start = 1)

# Generate metrics
enumerated_households <- household_data %>%
  filter(week_enum == current_week) %>%
  nrow()

reached_households <- screening_data %>%
  filter(week_reg == current_week) %>%
  distinct(dwelling_name) %>%
  nrow()

people_registered <- screening_data %>%
  filter(week_reg == current_week) %>%
  nrow()

tst_completed <- screening_data %>%
  filter(week_reg == current_week, tst_read_bin == TRUE) %>%
  nrow()

referred_ntp <- screening_data %>%
  filter(week_reg == current_week, tb_decision == "Presumptive TB") %>%
  nrow()

referred_nlp <- screening_data %>%
  filter(week_reg == current_week, lep_refer == "Yes") %>%
  nrow()

referred_hepb <- screening_data %>%
  filter(week_reg == current_week, prerx_hbv_1 == "Positive") %>%
  nrow()

cxr_performed <- screening_data %>%
  filter(week_reg == current_week, cxr_done == "Yes") %>%
  nrow()

xpert_tests_done <- screening_data %>%
  filter(week_reg == current_week, spuxpt_labreq_lab == "Yes") %>%
  nrow()

started_tpt <- treatment_data %>%
  filter(week_start == current_week) %>%
  nrow()

completed_tpt <- treatment_data %>%
  filter(week_outcome == current_week, tpt_outcome_reason == "Completed") %>%
  nrow()

# Create summary table
summary_table <- tibble(
  Metric = c("Households Enumerated", "Households Reached", "People Registered", 
             "TSTs Completed", "Referred to NTP", "Referred to NLP", 
             "Referred to Hep B", "X-Rays Performed", "Xpert Tests Done", 
             "Started on TPT", "Completed TPT"),
  Count = c(enumerated_households, reached_households, people_registered, 
            tst_completed, referred_ntp, referred_nlp, 
            referred_hepb, cxr_performed, xpert_tests_done, 
            started_tpt, completed_tpt)
)

# Display as flextable with title
table_06.01 <- summary_table %>%
  flextable() %>%
  set_header_labels(Metric = "Indicator", Count = "Total Count") %>%
  add_header_lines(values = paste("Summary of PEARL activity for week starting", format(current_week, "%Y-%m-%d"))) %>%
  theme_vanilla() %>%
  autofit()

table_06.01