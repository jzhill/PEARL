# Title and description --------------------------------------------

# Aggregating screening data at household level and uploading to the hh project
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   09 March 2025


# Packages -----------------------------------------

library(tidyverse)
library(REDCapR)
library(here)

# Aggregate screening data: count the number of screened individuals per dwelling_id
screening_counts <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_reg = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

# Aggregate screening data: count the number of individuals with TB decision per dwelling_id
tbdec_counts <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  filter(!is.na(tb_decision) & tb_decision != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_tbdec = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

combined_counts <- full_join(screening_counts, tbdec_counts, by = "record_id")

# Optionally, inspect the aggregated data
print(combined_counts)
REDCapR::validate_for_write(combined_counts)

# Send the aggregated data to the household project
result <- REDCapR::redcap_write(
  combined_counts,
  redcap_uri = uri,
  token = token_hh,
  overwrite_with_blanks = TRUE,
  verbose = TRUE
)

# Print the result of the API call
print(result)