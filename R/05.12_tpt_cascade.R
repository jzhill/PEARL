# Load required libraries
library(tidyverse)
library(lubridate)

# Determine the cutoff date (4 months before the most recent date in each dataset)
tpt_outcome_cutoff_date <- Sys.Date() - weeks(16)

# TPT cascade data --------------------------------------

# Process screening_data for initial cascade stages
tpt_cascade_sd <- screening_data %>%
  filter(en_date_visit <= tpt_outcome_cutoff_date) %>% 
  summarise(
    `TST Positive` = sum(tst_read_positive == "Positive TST", na.rm = TRUE),
    `Completed TPT Assessment` = sum(prerx_eligible == "No" | prerx_eligible == "Yes", na.rm = TRUE),
    `Eligible for TPT` = sum(prerx_eligible == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "stage", values_to = "count")

# Process treatment_data for later cascade stages
tpt_cascade_td <- treatment_data %>%
  filter(tpt_start_date <= tpt_outcome_cutoff_date) %>%
  summarise(
    `Started TPT` = sum(!is.na(tpt_start_date), na.rm = TRUE),
    `Treatment Outcome Assigned` = sum(!is.na(tpt_outcome_reason), na.rm = TRUE),
    `Completed TPT` = sum(tpt_outcome_reason == "Completed", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "stage", values_to = "count")

# Combine both cascades
tpt_cascade <- bind_rows(tpt_cascade_sd, tpt_cascade_td)

# Ensure proper ordering of cascade stages
tpt_cascade$stage <- factor(
  tpt_cascade$stage,
  levels = c(
    "TST Positive", 
    "Completed TPT Assessment", 
    "Eligible for TPT", 
    "Started TPT", 
    "Treatment Outcome Assigned", 
    "Completed TPT"
  )
)

# Display table
print(tpt_cascade)

# Reasons ineligible -----------------------------------

tpt_ineligible <- screening_data %>%
  filter(en_date_visit <= tpt_outcome_cutoff_date, prerx_eligible == "No") %>%
  mutate(
    reason = case_when(
      prerx_tptchoice == FALSE ~ "Refused",
      exit_reason_screen == "TPT - withdraw consent and prefer not to continue" ~ "Refused",
      prerx_tb12m == TRUE ~ "Treated <12 months",
      prerx_allergy == TRUE ~ "Reported allergy",
      prerx_preg == TRUE ~ "Pregnant",
      prerx_tbsx == TRUE | (tb_decision == "Presumptive TB" & ntp_diagnosis != "Ruled out") ~ "Presumed TB",
      prerx_riskcat == "High" ~ "High-risk DILI",
      TRUE ~ "Other/Unknown"
    )
  ) %>%
  count(reason)  # Summarize counts for each reason

# Calculate total ineligible count from the dataset
total_ineligible <- sum(tpt_ineligible$n)

# Extract values from tpt_cascade
completed_assessment <- tpt_cascade %>%
  filter(stage == "Completed TPT Assessment") %>%
  pull(count)

eligible_tpt <- tpt_cascade %>%
  filter(stage == "Eligible for TPT") %>%
  pull(count)

# Compute expected ineligible count
expected_ineligible <- completed_assessment - eligible_tpt

print(total_ineligible)
print(expected_ineligible)

# Reasons not completing assessment ------------------------------

# Filter for TST-positive individuals who did not complete TPT assessment
tpt_not_assessed <- screening_data %>%
  filter(
    en_date_visit <= tpt_outcome_cutoff_date,
    tst_read_positive == "Positive TST",
    !(prerx_eligible %in% c("Yes", "No"))  # Exclude those with a completed assessment
  ) %>%
  mutate(
    reason = case_when(
      exit_reason_screen %in% c("TPT - lost to follow-up", "Screening - lost to follow-up") ~ "LTFU",
      exit_reason_screen == "TPT - withdraw consent and prefer not to continue" ~ "Refuse",
      exit_reason_screen == "Died" ~ "Died",
      TRUE ~ calc_tpt  # Default to calc_tpt if not in the predefined categories
    )
  ) %>%
  count(reason, name = "count")  # Summarize counts

# Display the tibble
tpt_not_assessed

# Plots -------------------------------------

ggplot(tpt_cascade, aes(x = stage, y = count, fill = stage)) +
  geom_col(width = 0.6) +  # Make bars wide enough for labels
  geom_text(aes(label = count), vjust = -0.5, size = 5, fontface = "bold") +  # Add data labels above bars
  theme_light() +
  labs(
    title = "TST-Positive Treatment Cascade (Excluding Last 4 Months)",
    x = NULL,  # Remove x-axis label
    y = "Number of Individuals"
  ) +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text labels
    axis.ticks.x = element_blank()  # Remove x-axis ticks
  ) +
  scale_fill_viridis_d()


# Full pie chart with labels inside and a legend
ggplot(tpt_ineligible, aes(x = "", y = n, fill = reason)) +
  geom_bar(stat = "identity", width = 1) +  # Full pie chart
  coord_polar(theta = "y") +  # Convert to pie chart
  theme_void() +  # Remove all unnecessary grid elements
  labs(
    title = "Reasons for TPT Ineligibility (Excluding Last 4 Months)",
    fill = "Reason"
  ) +
  scale_fill_viridis_d() +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +  # Labels inside segments
  annotate("text", x = 0, y = 0, label = paste("Total =", total_ineligible), size = 6, fontface = "bold", color = "black")

# Create a pie chart for reasons not completing TPT assessment
ggplot(tpt_not_assessed, aes(x = "", y = count, fill = reason)) +
  geom_bar(stat = "identity", width = 1) +  # Full pie chart
  coord_polar(theta = "y") +  # Convert to pie chart
  theme_void() +  # Remove unnecessary elements
  labs(
    title = "Reasons for Not Completing TPT Assessment (Excluding Last 4 Months)",
    fill = "Reason"
  ) +
  scale_fill_viridis_d() +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 5, fontface = "bold")
