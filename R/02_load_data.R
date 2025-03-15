# Title and Description --------------------------------------------

# Loading and preparing PEARL data from data-raw folders
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   23 Feb 2025
# Last modified:    15 Mar 2025

# Packages -----------------------------------------

library(here)
library(stringr)
library(lubridate)
library(tidyverse)
library(sf)

# Define required data folders explicitly -------------------------

required_folders <- c(
  here("data-raw/screening"),
  here("data-raw/household"),
  here("data-raw/treatment"),
  here("data-raw/ea"),
  here("data-raw/dds") # Data dictionaries folder
)

# Ensure all required directories exist; create them if missing
for (dir in required_folders) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message("Created missing directory: ", dir)
  }
}

# Load the most recent CSV file from each required folder -----------------

load_latest_csv <- function(folder) {
  files <- dir(folder, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No CSV files found in folder: ", folder)
    return(NULL)
  }
  
  # Extract datetime from filenames (assumes YYYY-MM-DD_HHMM format)
  datetime_strings <- str_extract(basename(files), "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}")
  datetimes <- lubridate::ymd_hm(datetime_strings)
  
  # Identify and load the most recent file
  latest_file <- files[which.max(datetimes)]
  df <- read_csv(latest_file, show_col_types = FALSE)
  
  message("Loaded data from: ", latest_file)
  return(df)
}

# Load datasets into environment ----------------------

screening_data <- load_latest_csv(here("data-raw/screening"))
household_data <- load_latest_csv(here("data-raw/household"))
treatment_data <- load_latest_csv(here("data-raw/treatment"))
ea_data <- load_latest_csv(here("data-raw/ea"))

# Load data dictionaries ----------------------

# Column rename map
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

# Helper function: Rename columns if needed
rename_if_needed <- function(dd) {
  dd %>%
    rename_with(~ rename_map[.x], .cols = intersect(names(dd), names(rename_map)))
}

# Normalize field labels for matching
normalize_field_label <- function(x) {
  x %>%
    gsub("<[^>]+>", "", .) %>%  # Remove HTML tags
    str_squish() %>%  # Remove extra whitespace
    str_to_lower() %>%  # Convert to lowercase
    str_replace_all("[[:punct:]]", "") %>%  # Remove punctuation
    str_replace_all("\\s+", "")  # Remove remaining spaces
}

# Load data dictionaries from data-raw/dds ----------------------

load_dd <- function(filename) {
  filepath <- here("data-raw/dds", filename)
  if (!file.exists(filepath)) {
    warning("Data dictionary file not found: ", filename)
    return(NULL)
  }
  
  dd <- read_csv(filepath, show_col_types = FALSE) %>%
    rename_if_needed() %>%
    mutate(field_label_norm = normalize_field_label(field_label))
  
  message("Loaded data dictionary: ", filename)
  return(dd)
}

screening_dd <- load_dd("screening_dd.csv")
household_dd <- load_dd("household_dd.csv")
treatment_dd <- load_dd("treatment_dd.csv")
ea_dd <- load_dd("ea_dd.csv")

# Rename columns using the data dictionary ----------------------

rename_columns_using_dd <- function(df, dd) {
  if (is.null(df) || is.null(dd)) return(df)
  
  rename_map <- setNames(dd$field_name, dd$field_label_norm)
  current_names <- names(df)
  normalized_current <- normalize_field_label(current_names)
  
  new_names <- sapply(seq_along(current_names), function(i) {
    norm <- normalized_current[i]
    if (norm %in% names(rename_map)) {
      rename_map[[norm]]
    } else {
      current_names[i]
    }
  }, USE.NAMES = FALSE)
  
  names(df) <- new_names
  return(df)
}

# Apply renaming to datasets
screening_data <- rename_columns_using_dd(screening_data, screening_dd)
household_data <- rename_columns_using_dd(household_data, household_dd)
treatment_data <- rename_columns_using_dd(treatment_data, treatment_dd)
ea_data <- rename_columns_using_dd(ea_data, ea_dd)

# Convert column types using data dictionaries ----------------------

convert_field_types <- function(data, dd) {
  if (is.null(data) || is.null(dd)) return(data)
  
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
  
  # Convert column types
  data <- data %>%
    mutate(across(any_of(factor_fields), as.factor)) %>%
    mutate(across(any_of(text_fields), as.character)) %>%
    mutate(across(any_of(date_fields), as.Date)) %>%
    mutate(across(any_of(datetime_fields), as.POSIXct)) %>%
    mutate(across(any_of(num_fields), as.numeric)) %>%
    mutate(across(any_of(int_fields), as.integer)) %>%
    mutate(record_id = as.character(record_id))
  
  return(data)
}

# Apply conversions
screening_data <- convert_field_types(screening_data, screening_dd)
household_data <- convert_field_types(household_data, household_dd)
treatment_data <- convert_field_types(treatment_data, treatment_dd)
ea_data <- convert_field_types(ea_data, ea_dd)

# Load GIS data if present ------------------------------

ki_ea_gis_path <- here("data-raw/gis/KIR_EA_Census2020FINAL.geojson")

if (file.exists(ki_ea_gis_path)) {
    message("Loading GIS data from: ", ki_ea_gis_path)
  layer_ki_ea <- st_read(ki_ea_gis_path, quiet = TRUE)
  
  if (is.na(st_crs(layer_ki_ea))) {
    warning("The GIS data has no defined CRS. It may not align properly with other spatial data.")
  } else if (st_crs(layer_ki_ea)$epsg != 4326) {
    warning("The CRS is not EPSG:4326. Converting to EPSG:4326 for consistency...")
    layer_ki_ea <- st_transform(layer_ki_ea, crs = 4326)
  }
  
  # Convert to EPSG:3832 (Kiribati UTM Zone 57S) for better plotting
  layer_ki_ea_3832 <- st_transform(layer_ki_ea, crs = 3832)
  
  message("GIS data successfully loaded and standardized to EPSG:4326 and EPSG:3832.")
  
} else {
  warning("GIS data file not found: ", geojson_path)
}
