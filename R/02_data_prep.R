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

# Load data from .rds files in folders into dataframes ---------------------------

# Define report folders
report_folders <- c("screening", "household", "treatment")

for (folder in report_folders) {
  # List all .rds files in the folder with a 6-digit datestamp at the end
  files <- dir(here("data-raw", folder), pattern = "\\d{6}\\.rds$", full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No files found in folder: ", folder)
    next
  }
  
  # Extract the datestamp from each file name, assumes the file name ends with _yymmdd.rds
  date_strings <- str_extract(basename(files), "(?<=_)[0-9]{6}(?=\\.rds$)")
  
  # Convert these strings to Date objects using the format "%y%m%d"
  dates <- as.Date(date_strings, format = "%y%m%d")
  
  # Identify the file with the most recent date
  latest_index <- which.max(dates)
  latest_file <- files[latest_index]
  
  # Read the .rds file into a data frame
  df <- readRDS(latest_file)
  
  # Create variable names and assign dataframe
  var_name <- paste0(folder, "_data")
  assign(var_name, df, envir = .GlobalEnv)
  
  message("Loaded ", var_name, " from file: ", latest_file)
}

## Mutate record_id in each df to character ----------------------

# Vector of the dataframe names as character strings
df_names <- c("screening_data", "household_data", "treatment_data")

# Loop to mutate record_id then reassign to variable

for (df_name in df_names) {
  df <- get(df_name)
  df <- df %>% mutate(record_id = as.character(record_id))
  assign(df_name, df, envir = .GlobalEnv)
}

## Load data dictionary CSV files into the environment ----------------------
for (folder in report_folders) {
  
  # Build the filename for the data dictionary CSV file (e.g., "screening_dd.csv")
  dd_file_path <- here("data-raw", paste0(folder, "_dd.csv"))
  
  if (file.exists(dd_file_path)) {
    dd_data <- read_csv(dd_file_path)
    # Create a variable name for the data dictionary (e.g., screening_dd)
    dd_var_name <- paste0(folder, "_dd")
    assign(dd_var_name, dd_data, envir = .GlobalEnv)
    message("Loaded data dictionary for ", folder, " from file: ", dd_file_path)
  } else {
    warning("Data dictionary file not found for folder: ", folder)
  }
}

## Define a function that converts column types based on the data dictionary -----------------------

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
