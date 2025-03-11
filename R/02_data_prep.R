# Title and description --------------------------------------------

# Loading and preparing PEARL data from data-raw folders
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   23 Feb 2025


# Packages -----------------------------------------

library(here)
library(stringr)
library(lubridate)
library(tidyverse)
library(epikit)
library(dplyr)
library(purrr)

# Load data from .csv files in folders into dataframes ---------------------------

# Dynamically define report folders by listing subdirectories of "data-raw"
report_folders <- list.dirs(here("data-raw"), recursive = FALSE, full.names = FALSE)

for (folder in report_folders) {
  
  # List all CSV files in the folder
  files <- dir(here("data-raw", folder), pattern = "\\.csv$", full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No files found in folder: ", folder)
    next
  }
  
  # Extract the datetime from each file name, assumes numbers in descending format
  datetime_strings <- str_extract(basename(files), "[0-9].*[0-9]")
  
  # Convert these strings to Datetime objects using 
  datetimes <- lubridate::ymd_hm(datetime_strings)
  
  # Identify the file with the most recent date
  latest_index <- which.max(datetimes)
  latest_file <- files[latest_index]
  
  # Read the .csv file into a data frame
  df <- read_csv(latest_file)
  
  # Create variable names and assign dataframe
  var_name <- paste0(folder, "_data")
  assign(var_name, df, envir = .GlobalEnv)
  
  message("Loaded ", var_name, " from file: ", latest_file)
}

## Load data dictionary CSV files into the environment ----------------------

# Data dictionary column rename map

rename_map <- c(
  "Variable / Field Name" = "field_name",
  "Form Name" = "form_name",
  "Section Header" = "section_header",
  "Field Type" = "field_type",
  "Field Label" = "field_label",
  "Choices, Calculations, OR Slider Labels" = "select_choices_or_calculations",
  "Field Note" = "field_note",
  "Text Validation Type OR Show Slider Number" = "text_validation_type_or_show_slider_number",
  "Text Validation Min" = "text_validation_min",
  "Text Validation Max" = "text_validation_max",
  "Identifier?" = "identifier",
  "Branching Logic (Show field only if...)" = "branching_logic",
  "Required Field?" = "required_field",
  "Custom Alignment" = "custom_alignment",
  "Question Number (surveys only)" = "question_number",
  "Matrix Group Name" = "matrix_group_name",
  "Matrix Ranking?" = "matrix_ranking",
  "Field Annotation" = "field_annotation"
)

# Data dictionary column rename helper function

rename_if_needed <- function(dd) {
  dd_renamed <- dd %>%
    rename_with(
      .fn = ~ rename_map[.x],
      .cols = intersect(names(dd), names(rename_map))
    )
  return(dd_renamed)
}

# Define a helper function to normalise and strip everything from field_label

normalise_field_label <- function(x) {
  x %>%
    gsub("<[^>]+>", "", .) %>%  # Remove HTML tags
    str_squish() %>%  # Remove extra whitespace
    str_to_lower() %>%  # Convert to lowercase
    str_replace_all("[[:punct:]]", "") %>%  # Remove punctuation
    str_replace_all("\\s+", "")  # Remove remaining spaces
}

# Load data dictionary for each folder, applying the column rename function for each

for (folder in report_folders) {
  
  dd_file_path <- here("data-raw", paste0(folder, "_dd.csv"))
  
  if (file.exists(dd_file_path)) {
    dd_data <- read_csv(dd_file_path) %>%
      rename_if_needed() %>%
      mutate(field_label_norm = normalise_field_label(field_label))
    dd_var_name <- paste0(folder, "_dd")
    assign(dd_var_name, dd_data, envir = .GlobalEnv)
    message("Loaded data dictionary for ", folder, " from file: ", dd_file_path)
  } else {
    warning("Data dictionary file not found for folder: ", folder)
  }
}

## Convert column names based on the data dictionary -------------------------

# Define a helper function to convert column names

rename_columns_using_dd <- function(df, dd) {
  rename_map <- setNames(dd$field_name, dd$field_label_norm)
  current_names <- names(df)
  normalised_current <- normalise_field_label(current_names)
  new_names <- sapply(seq_along(current_names), function(i) {
    norm <- normalised_current[i]
    if (norm %in% names(rename_map)) {
      rename_map[[norm]]
    } else {
      current_names[i]
    }
  }, USE.NAMES = FALSE)
  names(df) <- new_names
  return(df)
}

# Apply helper function to each dataset

screening_data <- rename_columns_using_dd(screening_data, screening_dd)
household_data <- rename_columns_using_dd(household_data, household_dd)
treatment_data <- rename_columns_using_dd(treatment_data, treatment_dd)
ea_data <- rename_columns_using_dd(ea_data, ea_dd)

## Convert column types based on the data dictionary -----------------------

# Define a helper function to convert field types

convert_field_types <- function(data, dd) {
  
  # DATE
  date_fields1 <- dd %>%
    filter(text_validation_type_or_show_slider_number == "date_dmy") %>%
    pull(field_name)
  date_fields2 <- c("calc_alt_last_date", "calc_1mrv")
  date_fields <- union(date_fields1, date_fields2)
  
  # DATETIME
  datetime_fields <- dd %>%
    filter(text_validation_type_or_show_slider_number == "datetime_dmy") %>%
    pull(field_name)
  
  # NUMERIC
  num_fields <- dd %>%
    filter(text_validation_type_or_show_slider_number == "number") %>%
    pull(field_name)
  
  # INTEGER
  int_fields1 <- dd %>%
    filter(text_validation_type_or_show_slider_number == "integer") %>%
    pull(field_name)
  int_fields2 <- dd %>%
    filter(field_type == "calc") %>%
    pull(field_name)
  int_fields <- union(int_fields1, int_fields2)
  
  # FACTOR
  factor_fields1 <- dd %>%
    filter(field_type %in% c("radio", "dropdown", "yesno")) %>%
    pull(field_name)
  factor_fields2 <- dd %>%
    filter(!is.na(field_annotation) & str_starts(field_annotation, "@CALCTEXT")) %>%
    pull(field_name)
  factor_fields <- union(factor_fields1, factor_fields2)
  factor_fields <- setdiff(factor_fields, date_fields2)  # Remove @CALCTEXT where that should be a date
  
  # CHARACTER
  text_fields <- dd %>%
    filter(field_type %in% c("text", "notes")) %>%
    pull(field_name)
  
  # Remove any fields that have been allocated to another conversion group
  other_fields <- union(factor_fields, union(date_fields, union(datetime_fields, union(num_fields, int_fields))))
  text_fields <- setdiff(text_fields, other_fields)
  
  # Only convert columns that are present in the dataset using any_of()
  data <- data %>%
    mutate(across(any_of(factor_fields), as.factor)) %>%
    mutate(across(any_of(text_fields), as.character)) %>% 
    mutate(across(any_of(date_fields), as.Date)) %>% 
    mutate(across(any_of(datetime_fields), as.Date)) %>% 
    mutate(across(any_of(num_fields), as.numeric)) %>% 
    mutate(across(any_of(int_fields), as.integer)) %>% 
    mutate(record_id = as.character(record_id))
  
  return(data)
}

# Apply the conversion function to each dataset using its respective data dictionary

screening_data <- convert_field_types(screening_data, screening_dd)
household_data <- convert_field_types(household_data, household_dd)
treatment_data <- convert_field_types(treatment_data, treatment_dd)
ea_data <- convert_field_types(ea_data, ea_dd)

## Data preparation steps -------------------------

# Age category

screening_data <- screening_data %>%
  mutate(age_cat = epikit::age_categories(en_cal_age, by = 10, upper = 80))

treatment_data <- treatment_data %>%
  mutate(age_cat = epikit::age_categories(tpt_age, by = 10, upper = 80))

# Epi weeks

max_week <- floor_date(Sys.Date(), unit = "week", week_start = 1)

screening_data <- screening_data %>%
  mutate(week_reg = floor_date(en_date_visit, unit = "week", week_start = 1)) %>%
  mutate(week_reg = if_else(week_reg > max_week, NA_Date_, week_reg))

treatment_data <- treatment_data %>%
  mutate(week_reg = floor_date(tpt_reg_date, unit = "week", week_start = 1)) %>%  
  mutate(week_reg = if_else(week_reg > max_week, NA_Date_, week_reg)) %>% 
  mutate(week_start = floor_date(tpt_start_date, unit = "week", week_start = 1)) %>%  
  mutate(week_start = if_else(week_start > max_week, NA_Date_, week_start))

household_data <- household_data %>%
  mutate(week_enum = floor_date(hh_date, unit = "week", week_start = 1)) %>%  
  mutate(week_enum = if_else(week_enum > max_week, NA_Date_, week_enum))

# EA numbers removing any text

screening_data <- screening_data %>% 
  mutate(ea_id = str_sub(ea_number, 1, 8))

household_data <- household_data %>% 
  mutate(hh_ea_id = str_sub(hh_ea, 1, 8))

treatment_data <- treatment_data %>% 
  mutate(tpt_ea_id = str_sub(tpt_ea, 1, 8))

# Join village from EA database to household 

household_data <- household_data %>%
  left_join(
    ea_data %>% select(record_id, village),
    by = c("hh_ea_id" = "record_id")
  ) %>%
  rename(hh_village_ea = village)

# Join village and EA from household database for comparison with captured data

screening_data <- screening_data %>%
  left_join(
    household_data %>% select(record_id, hh_ea_id, hh_village_ea),
    by = c("dwelling_id" = "record_id")
  ) %>%
  rename(
    ea_number_hh = hh_ea_id,
    res_village_hh = hh_village_ea
  )

## Pivot with count or sum for each field per household into the household dataset -----------------------------

# Aggregate screening data: count the number of screened individuals per dwelling_id
hh_pivot_reg <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_reg_new = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

# Aggregate screening data: count the number of individuals with TB decision per dwelling_id
hh_pivot_tbdec <- screening_data %>%
  filter(!is.na(dwelling_id) & dwelling_id != "") %>%
  filter(!is.na(tb_decision) & tb_decision != "") %>%
  group_by(dwelling_id) %>%
  summarise(hh_tbdec_new = n(), .groups = "drop") %>%
  rename(record_id = dwelling_id)

hh_pivot <- full_join(hh_pivot_reg, hh_pivot_tbdec, by = "record_id")

household_data <- household_data %>%
  select(-any_of(c("hh_reg_new", "hh_tbdec_new"))) %>% 
  left_join(hh_pivot, by = "record_id")


## Pivot with count or sum for each field per EA into the EA dataset -----------------------------

# Aggregate screening data
ea_pivot_screen <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "") %>%
  group_by(ea_id) %>%
  summarise(
    pop_reg_screen_new = n(),
    date_screen_new = min(en_date_visit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(record_id = ea_id)

ea_pivot_screen_mf <- screening_data %>%
  filter(!is.na(ea_id) & ea_id != "" & en_sex %in% c("M", "F")) %>%
  mutate(en_sex = tolower(en_sex)) %>%
  group_by(ea_id, en_sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = en_sex, values_from = count, names_prefix = "pop_reg_screen_") %>%
  mutate(
    pop_reg_screen_m = replace_na(pop_reg_screen_m, 0),
    pop_reg_screen_f = replace_na(pop_reg_screen_f, 0)
  ) %>%
  rename(
    record_id = ea_id,
    pop_reg_screen_m_new = pop_reg_screen_m,
    pop_reg_screen_f_new = pop_reg_screen_f
  )

# Aggregate household data
ea_pivot_hh <- household_data %>%
  filter(!is.na(hh_ea_id) & hh_ea_id != "") %>%
  group_by(hh_ea_id) %>%
  summarise(
    pop_all_new = sum(hh_all, na.rm = TRUE),
    pop_current_new = sum(hh_size, na.rm = TRUE),
    pop_elig_new = sum(hh_size_elig, na.rm = TRUE),
    pop_reg_enum_new = sum(hh_reg, na.rm = TRUE),
    hh_enum_new = n(),
    date_enum_new = min(hh_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(record_id = hh_ea_id)

ea_pivot <- reduce(c(list(ea_pivot_screen, ea_pivot_screen_mf, ea_pivot_hh)), full_join, by = "record_id")

ea_data <- ea_data %>%
  select(-any_of(c("pop_reg_screen_new",
                   "pop_reg_screen_m_new",
                   "pop_reg_screen_f_new",
                   "pop_all_new",
                   "pop_current_new",
                   "pop_elig_new",
                   "pop_reg_enum_new",
                   "hh_enum_new",
                   "date_enum_new",
                   "date_screen_new"))) %>%
  left_join(ea_pivot, by = "record_id")

