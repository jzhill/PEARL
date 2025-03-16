# Title and Description --------------------------------------------

# Loading and preparing PEARL data from data-raw folders
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   23 Feb 2025
# Last modified:    16 Mar 2025

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
  here("data-raw/dds")
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
  
  # Check for files which include valid datetimes
  valid_indices <- which(!is.na(datetimes))
  if (length(valid_indices) == 0) {
    warning("No valid timestamp found in filenames in folder: ", folder)
    return(NULL)
  }
  
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

# Load and clean data dictionaries ----------------------

## Rename map and helper function ---------------------

# Data dictionary column rename map
# DD downloaded from REDCap has labelled columns, DD retrieved by API has coded columns
# This way, a DD from either method can be loaded

rename_map_dd <- c(
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

rename_dd_if_needed <- function(dd) {
  dd %>%
    rename_with(~ rename_map_dd[.x], .cols = intersect(names(dd), names(rename_map_dd)))
}

# Labelled data column headers exported from REDCap are just the text
# In the DDs the field_label column includes html from the label
# First need to normalise the data in the field_name column
# Then need to normalise the column headers in labelled data
# Then we can rename the column headers with field names from the DD

normalise_field_label <- function(x) {
  x %>%
    gsub("<[^>]+>", "", .) %>%  # Remove HTML tags
    str_squish() %>%  # Remove extra whitespace
    str_to_lower() %>%  # Convert to lowercase
    str_replace_all("[[:punct:]]", "") %>%  # Remove punctuation
    str_replace_all("\\s+", "")  # Remove remaining spaces
}

load_dd <- function(filename) {
  filepath <- here("data-raw/dds", filename)
  if (!file.exists(filepath)) {
    warning("Data dictionary file not found: ", filename)
    return(NULL)
  }
  
  dd <- read_csv(filepath, show_col_types = FALSE) %>%
    rename_dd_if_needed() %>%
    mutate(field_label_norm = normalise_field_label(field_label))
  
  message("Loaded data dictionary: ", filename)
  return(dd)
}

## Load data dictionaries from data-raw/dds ----------------------

screening_dd <- load_dd("screening_dd.csv")
household_dd <- load_dd("household_dd.csv")
treatment_dd <- load_dd("treatment_dd.csv")
ea_dd <- load_dd("ea_dd.csv")

# Rename data columns using the data dictionary ----------------------

rename_columns_using_dd <- function(df, dd) {
  if (is.null(df) || is.null(dd)) return(df)
  
  rename_map <- setNames(dd$field_name, dd$field_label_norm)
  current_names <- names(df)
  current_names_norm <- normalise_field_label(current_names)
  
  new_names <- sapply(seq_along(current_names), function(i) {
    norm <- current_names_norm[i]
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

# Identify and clean valid yes/no columns --------------------

# Helper function
clean_yesno_values <- function(df) {
  yesno_cols <- names(df)[sapply(df, function(col) {
    unique_vals <- na.omit(unique(col))  # Remove NAs
    cleaned_vals <- tolower(str_trim(unique_vals))  # Normalize case & trim spaces
    all(cleaned_vals %in% c("yes", "no"))  # Ensure only Yes/No remain
  })]
  
  if (length(yesno_cols) > 0) {
    df <- df %>%
      mutate(across(all_of(yesno_cols), ~ case_when(
        tolower(str_trim(.)) == "yes" ~ "Yes",
        tolower(str_trim(.)) == "no" ~ "No",
        TRUE ~ as.character(NA)  # Preserve missing values
      )))
  }
  
  return(df)
}

# Apply yes/no function
screening_data <- clean_yesno_values(screening_data)
household_data <- clean_yesno_values(household_data)
treatment_data <- clean_yesno_values(treatment_data)
ea_data <- clean_yesno_values(ea_data)

# Convert column types in imported data ----------------------

convert_field_types <- function(data, dd) {
  if (is.null(data) || is.null(dd)) return(data)
  
  # Classify columns based on the data dictionary
  field_groups <- list(
    date = dd %>%
      filter(field_type == "text" & text_validation_type_or_show_slider_number == "date_dmy") %>%
      pull(field_name),
    
    datetime = dd %>%
      filter(field_type == "text" & text_validation_type_or_show_slider_number == "datetime_dmy") %>%
      pull(field_name),
    
    integer = dd %>%
      filter(field_type == "text" & text_validation_type_or_show_slider_number == "integer") %>%
      pull(field_name),
    
    numeric = dd %>%
      filter((field_type == "text" & text_validation_type_or_show_slider_number == "number") | 
               (field_type == "calc")) %>%
      pull(field_name),
    
    factor = dd %>%
      filter(field_type %in% c("radio", "dropdown", "checkbox", "yesno") | 
               (field_type == "dropdown" & text_validation_type_or_show_slider_number == "autocomplete")) %>%
      pull(field_name),
    
    calctext = dd %>%
      filter(!is.na(field_annotation) & str_starts(field_annotation, "@CALCTEXT")) %>%
      pull(field_name)
  )
  
  # Merge @CALCTEXT fields into factor fields
  field_groups$factor <- unique(c(field_groups$factor, field_groups$calctext))
  
  # Identify text fields last, ensuring they are not already converted
  converted_fields <- unlist(field_groups[c("integer", "numeric", "date", "datetime", "factor")])
  
  field_groups$text <- dd %>%
    filter(field_type %in% c("text", "notes", "textarea", "descriptive")) %>%
    pull(field_name) %>%
    setdiff(converted_fields)
  
  # Apply dictionary-based conversions
  data <- data %>%
    mutate(across(any_of(field_groups$date), as.Date)) %>%
    mutate(across(any_of(field_groups$datetime), as.POSIXct)) %>%
    mutate(across(any_of(field_groups$integer), as.integer)) %>%
    mutate(across(any_of(field_groups$numeric), as.numeric)) %>%
    mutate(across(any_of(field_groups$factor), as.factor)) %>%
    mutate(across(any_of(field_groups$text), as.character)) %>%
    mutate(record_id = as.character(record_id))  # Ensure record_id stays character
  
  # Detect and Convert yes/no Columns to Logical ----------------------------
  
  yesno_cols <- names(data)[sapply(data, function(col) {
    unique_vals <- na.omit(unique(col)) 
    cleaned_vals <- tolower(str_trim(unique_vals))
    all(cleaned_vals %in% c("yes", "no"))  # Ensure only "yes" and "no"
  })]
  
  # Convert detected yes/no columns to logical
  if (length(yesno_cols) > 0) {  
    data <- data %>%
      mutate(across(all_of(yesno_cols), ~ case_when(
        tolower(.) == "yes" ~ TRUE,
        tolower(.) == "no" ~ FALSE,
        TRUE ~ NA  # Preserve missing values
      )))
  }
  
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
  
  layer_ki_ea <- tryCatch(
    {
      data <- st_read(ki_ea_gis_path, quiet = TRUE)
      if (is.null(data) || nrow(data) == 0) stop("GIS file is empty.")
      data
    },
    error = function(e) {
      warning("Failed to load GIS data: ", e$message)
      return(NULL)
    }
  )
  
  if (!is.null(layer_ki_ea)) {
    message("Successfully loaded GIS data from: ", ki_ea_gis_path)
    
    if (st_crs(layer_ki_ea)$epsg != 4326) {
      layer_ki_ea <- st_transform(layer_ki_ea, crs = 4326)
    }
    layer_ki_ea_3832 <- st_transform(layer_ki_ea, crs = 3832)
  }
} else {
  warning("GIS data file not found: ", ki_ea_gis_path)
}
