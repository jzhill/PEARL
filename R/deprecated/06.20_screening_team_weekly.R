# Friday Reporting Meeting: Team Performance Table
# Aligned with "Screening Team Oral Presentation Format"
# Logic: Targets (from Household assignments) vs. Activity (from Screening data)

library(flextable)
library(officer)
library(dplyr)
library(tidyr)

# 1. Prepare additional flags for completeness ----------------------------
# We define a 'complete' screening as having a TB decision.
screening_data <- screening_data %>%
  mutate(
    calc_screening_complete = !is.na(tb_decision) & tb_decision != ""
  )

# 2. Filter data for the current week --------------------------------------
# We use max_week which was calculated in the 03_ script
current_week_screening <- screening_data %>%
  filter(week_reg == max_week)

current_week_hh <- household_data %>%
  filter(week_enum == max_week)

# 3. Aggregate TARGETS by Assigned Team (from Household Data) --------------
# Using 'hh_team' to see what was assigned to them
team_targets <- current_week_hh %>%
  group_by(hh_team) %>%
  summarise(
    target_hh = n(),
    target_eligible = sum(hh_size_elig, na.rm = TRUE),
    target_non_eligible = sum(hh_size, na.rm = TRUE) - sum(hh_size_elig, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(en_team = hh_team) %>%
  filter(!is.na(en_team))

# 4. Aggregate ACTIVITY by Screening Team (from Screening Data) ------------
team_activity <- current_week_screening %>%
  group_by(en_team) %>%
  summarise(
    actual_registered = n(),
    actual_completed  = sum(calc_screening_complete, na.rm = TRUE),
    
    # TST Indicators
    tst_placed   = sum(tst_success == "Yes", na.rm = TRUE),
    tst_read     = sum(tst_read_bin, na.rm = TRUE),
    tst_positive = sum(tst_read_positive == "Positive TST", na.rm = TRUE),
    
    # TPT Pathway
    tpt_started  = sum(prerx_start == TRUE, na.rm = TRUE),
    tpt_inelig   = sum(prerx_eligible == "No", na.rm = TRUE),
    tpt_assessing = sum(prerx_eligible == "Not yet known" | is.na(prerx_eligible), na.rm = TRUE),
    
    # Outcomes
    tb_prestb    = sum(tb_decision == "Presumptive TB", na.rm = TRUE),
    tb_ruled_out = sum(tb_decision == "Ruled out TB", na.rm = TRUE),
    tb_uncertain = sum(tb_decision == "TB status uncertain", na.rm = TRUE),
    lep_outcome  = sum(referred_nlp, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(!is.na(en_team))

# 5. Join and Calculate "Missed" -------------------------------------------
report_data <- team_targets %>%
  full_join(team_activity, by = "en_team") %>%
  mutate(across(where(is.numeric), ~coalesce(., 0))) %>%
  mutate(
    actual_missed = pmax(0, target_eligible - actual_registered)
  ) %>%
  select(
    en_team, 
    target_hh, target_eligible, target_non_eligible, # Targets
    actual_registered, actual_completed, actual_missed, # Activity
    tst_placed, tst_read, tst_positive, # TST
    tpt_started, tpt_inelig, tpt_assessing, # TPT
    tb_prestb, tb_ruled_out, tb_uncertain, lep_outcome # Results
  )

# 6. Create Flextable for presentation -------------------------------------

# First, calculate the totals to append to the bottom
totals <- report_data %>%
  summarise(
    en_team = "TOTAL",
    across(where(is.numeric), sum)
  )

# Bind the totals to the main data
report_with_totals <- bind_rows(report_data, totals)

ft <- flextable(report_with_totals) %>%
  set_header_labels(
    en_team = "Team",
    target_hh = "HH Target",
    target_eligible = "Eligible",
    target_non_eligible = "Non-Elig",
    actual_registered = "Reg",
    actual_completed = "Complete",
    actual_missed = "Missed",
    tst_placed = "TST Placed",
    tst_read = "Read",
    tst_positive = "Pos",
    tpt_started = "TPT Start",
    tpt_inelig = "TPT Inelig",
    tpt_assessing = "TPT Assess",
    tb_prestb = "Presum. TB",
    tb_ruled_out = "TB R/O",
    tb_uncertain = "TB Uncert",
    lep_outcome = "Leprosy"
  ) %>%
  add_header_row(
    values = c("", "Targets (HH Assigned)", "Activity (Screening)", "TST", "TPT", "Outcomes"),
    colwidths = c(1, 3, 3, 3, 3, 4)
  ) %>%
  theme_vanilla() %>%
  autofit() %>%
  # Formatting the top header row
  bg(part = "header", bg = "#0047AB", i = 1) %>%
  color(part = "header", color = "white", i = 1) %>%
  bold(part = "header", i = 1:2) %>%
  # Formatting the Totals row at the bottom
  bold(i = nrow(report_with_totals)) %>%
  bg(i = nrow(report_with_totals), bg = "#F2F2F2") %>%
  align(align = "center", part = "all")

# Output
print(paste("Friday Reporting Table - Week Starting:", max_week))
ft