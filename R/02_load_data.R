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
  latest_file <- files[valid_indices][ which.max(datetimes[valid_indices]) ]
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
  cols_to_rename <- intersect(names(dd), names(rename_map_dd))
  dd %>% rename_with(~ unname(rename_map_dd[.x]), .cols = all_of(cols_to_rename))
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

check_unmapped <- function(df, dd) {
  if (is.null(df) || is.null(dd)) return(character())
  # Base map: field_label_norm -> field_name
  rename_map_base <- setNames(dd$field_name, dd$field_label_norm)
  # Checkbox map
  rename_map_cb <- build_checkbox_label_rename_map(dd)
  known_norms <- union(names(rename_map_base), names(rename_map_cb))
  cur_norms <- normalise_field_label(names(df))
  setdiff(cur_norms, known_norms)
}

## Load data dictionaries from data-raw/dds ----------------------

screening_dd <- load_dd("screening_dd.csv")
household_dd <- load_dd("household_dd.csv")
treatment_dd <- load_dd("treatment_dd.csv")
ea_dd <- load_dd("ea_dd.csv")

## Checkbox helper functions -------------------------------------------------

# Since the data dictionary only contains one row for each checkbox field, but the data contains one column for each option
# we need to create dictionary entries for each cb option that can be inserted to rename data columns consistently.
# We also need to recode the data values from checked and unchecked into T/F

# Parse data dictionary "select_choices_or_calculations" for checkbox options:
# e.g. "1, Option A | 2, Option B | 98, Other (specify)"
# Returns a tibble containing the code and label for each choice

parse_cb_choices <- function(x) {
  
  # Create an empty tibble in case of missing data
  if (is.na(x) || !nzchar(x)) return(tibble(code = character(), label = character()))
  
  #Split and extract text from choices into a tibble
  tibble(raw = str_split(x, "\\s*\\|\\s*", simplify = FALSE)[[1]]) %>%
    mutate(
      # split on the first comma only (labels can also contain commas)
      code  = str_trim(str_replace(raw, "^\\s*([^,]+),.*$", "\\1")),
      label = str_trim(str_replace(raw, "^\\s*[^,]+,\\s*", ""))
    ) %>%
    select(code, label) %>%
    filter(nzchar(code), nzchar(label))
}

# Build a rename map for checkbox columns that were exported with LABELLED headers.
# We reconstruct REDCap's labelled-export column header pattern: "[Field Label] (choice=<label>)"
# Then run normalise_field_label() on it so it matches df header normalisation.

build_checkbox_label_rename_map <- function(dd) {
  if (is.null(dd)) return(character())
  
  # df of only checkbox fields
  dd_cb <- dd %>% filter(field_type == "checkbox")
  if (nrow(dd_cb) == 0) return(character())
  
  # extract vector of cb choices, based on the tibble of cb choices from the previous function
  cb_list <- list()
  for (i in seq_len(nrow(dd_cb))) {
    base_field <- dd_cb$field_name[i]
    base_label <- dd_cb$field_label[i]
    choices_str <- dd_cb$select_choices_or_calculations[i]
    choices <- parse_cb_choices(choices_str)
    if (nrow(choices) == 0) next
    
    # For each choice, build a temporary string with normalised choice label and choice fieldname appended after :::, then add to the vector
    ch_str <- purrr::map2_chr(
      choices$label, choices$code,
      ~ {
        labelled_header <- paste0(base_label, " (choice=", .x, ")")
        norm_header <- normalise_field_label(labelled_header)
        # target REDCap-style checkbox column name
        target <- paste0(base_field, "___", .y)
        # store as "name"=target mapping after we return the vector
        paste(norm_header, target, sep = ":::")
      }
    )
    cb_list[[length(cb_list) + 1]] <- ch_str
  }
  
  if (length(cb_list) == 0) return(character())
  cb_list_chr <- unlist(cb_list, use.names = FALSE)
  
  # Convert the normalised string "norm_label:::fieldname" into a named character vector
  ch_names <- str_replace(cb_list_chr, "^.*:::", "")
  names(ch_names) <- str_replace(cb_list_chr, ":::.*$", "")
  ch_names
}

# Rename data columns using the data dictionary ----------------------

rename_columns_using_dd <- function(df, dd) {
  if (is.null(df) || is.null(dd)) return(df)
  
  # Base map: field_label_norm -> field_name
  rename_map_base <- setNames(dd$field_name, dd$field_label_norm)
  
  # Checkbox-specific map: "[field label] (choice=...)" -> "field_name___code"
  rename_map_cb <- build_checkbox_label_rename_map(dd)
  
  # Combine, with checkbox rules taking precedence if there’s any overlap
  # (named vector where names are the *normalised* current headers)
  rename_map <- c(rename_map_base, rename_map_cb)
  
  current_names <- names(df)
  current_names_norm <- normalise_field_label(current_names)
  
  # Renaming loop, leaving any unmapped names unchanged
  new_names <- vapply(seq_along(current_names), function(i) {
    norm <- current_names_norm[i]
    if (!is.na(norm) && norm %in% names(rename_map)) {
      rename_map[[norm]]
    } else {
      current_names[i]
    }
  }, character(1), USE.NAMES = FALSE)
  
  names(df) <- new_names
  df
}

# Apply renaming to datasets
screening_data <- rename_columns_using_dd(screening_data, screening_dd)
household_data <- rename_columns_using_dd(household_data, household_dd)
treatment_data <- rename_columns_using_dd(treatment_data, treatment_dd)
ea_data <- rename_columns_using_dd(ea_data, ea_dd)

# Print any unmapped headers to help debugging
unmapped_screening <- check_unmapped(screening_data, screening_dd)
unmapped_household <- check_unmapped(household_data, household_dd)
unmapped_treatment <- check_unmapped(treatment_data, treatment_dd)
unmapped_ea        <- check_unmapped(ea_data, ea_dd)

if (length(unmapped_screening)) message("Unmapped screening headers: ", paste(unmapped_screening, collapse = ", "))
if (length(unmapped_household)) message("Unmapped household headers: ", paste(unmapped_household, collapse = ", "))
if (length(unmapped_treatment)) message("Unmapped treatment headers: ", paste(unmapped_treatment, collapse = ", "))
if (length(unmapped_ea))        message("Unmapped EA headers: ", paste(unmapped_ea, collapse = ", "))

# Identify and clean valid yes/no columns --------------------

# Helper function for yes/no fields
clean_yesno_values <- function(df) {
  yesno_cols <- names(df)[sapply(df, function(col) {
    # Don't try to detect on logicals; treat only non-logical columns
    if (is.logical(col)) return(FALSE)
    vals <- na.omit(unique(col))
    vals_chr <- tolower(stringr::str_trim(as.character(vals)))
    length(vals_chr) > 0 && all(vals_chr %in% c("yes", "no"))
  })]
  
  if (length(yesno_cols) > 0) {
    df <- df %>%
      dplyr::mutate(across(all_of(yesno_cols), ~ dplyr::case_when(
        tolower(stringr::str_trim(as.character(.))) == "yes" ~ "Yes",
        tolower(stringr::str_trim(as.character(.))) == "no"  ~ "No",
        TRUE ~ as.character(NA)
      )))
  }
  
  df
}

# Apply yes/no function
screening_data <- clean_yesno_values(screening_data)
household_data <- clean_yesno_values(household_data)
treatment_data <- clean_yesno_values(treatment_data)
ea_data <- clean_yesno_values(ea_data)

# Helper function for checkbox columns

coerce_checkbox_logical <- function(data, dd) {
  if (is.null(data) || is.null(dd)) return(data)
  cb_fields <- dd %>% filter(field_type == "checkbox") %>% pull(field_name)
  if (length(cb_fields) == 0) return(data)
  
  # find columns for each checkbox field by prefix "fieldname___"
  cb_cols <- names(data)[Reduce(`|`, lapply(cb_fields, function(f) str_starts(names(data), paste0(f, "___"))), init = rep(FALSE, length(names(data))))]
  if (length(cb_cols) == 0) return(data)
  
  data %>%
    mutate(across(all_of(cb_cols), ~ {
      # Handle logicals, 0/1, "0"/"1", "Checked"/"Unchecked", "Yes"/"No"
      if (is.logical(.)) return(.)
      x <- .
      x <- dplyr::case_when(
        is.numeric(x) ~ x != 0,
        is.character(x) ~ {
          lx <- tolower(str_trim(x))
          dplyr::case_when(
            lx %in% c("1", "checked", "true", "yes") ~ TRUE,
            lx %in% c("0", "unchecked", "false", "no") ~ FALSE,
            TRUE ~ NA
          )
        },
        TRUE ~ as.logical(.)
      )
      as.logical(x)
    }))
}

# Apply checkbox function
screening_data <- coerce_checkbox_logical(screening_data, screening_dd)
household_data <- coerce_checkbox_logical(household_data, household_dd)
treatment_data <- coerce_checkbox_logical(treatment_data, treatment_dd)
ea_data <- coerce_checkbox_logical(ea_data, ea_dd)

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
      filter(field_type %in% c("radio", "dropdown", "yesno") | 
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
    mutate(across(any_of("record_id"), as.character))  # Ensure record_id stays character
  
  # Detect and Convert yes/no Columns to Logical ----------------------------
  yesno_cols <- names(data)[sapply(data, function(col) {
    if (is.logical(col)) return(FALSE)
    vals <- na.omit(unique(col))
    vals_chr <- tolower(stringr::str_trim(as.character(vals)))
    length(vals_chr) > 0 && all(vals_chr %in% c("yes", "no"))
  })]
  
  # Convert detected yes/no columns to logical
  if (length(yesno_cols) > 0) {
    data <- data %>%
      dplyr::mutate(across(all_of(yesno_cols), ~ dplyr::case_when(
        tolower(as.character(.)) == "yes" ~ TRUE,
        tolower(as.character(.)) == "no"  ~ FALSE,
        TRUE ~ NA
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
    
    crs_epsg <- tryCatch(st_crs(layer_ki_ea)$epsg, error = function(e) NA_integer_)
    
    # Only transform if CRS is known and different
    if (!is.na(crs_epsg) && crs_epsg != 4326) {
      layer_ki_ea <- st_transform(layer_ki_ea, crs = 4326)
    }
    
    # Safely attempt 3832; wrap in tryCatch so a missing/unknown CRS won't crash
    layer_ki_ea_3832 <- tryCatch(
      st_transform(layer_ki_ea, crs = 3832),
      error = function(e) {
        warning("Could not transform layer_ki_ea to EPSG:3832 (", e$message, ")")
        NULL
      }
    )
  }
} else {
  warning("GIS data file not found: ", ki_ea_gis_path)
}
