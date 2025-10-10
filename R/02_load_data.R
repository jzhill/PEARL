# Title and Description --------------------------------------------

# Loading and preparing PEARL data from data-raw folders
# Clean, functions-first version
# Data owned by University of Sydney and Kiribati MHMS
# Author: Jeremy Hill
# Started: 23 Feb 2025
# Last modified: 16 Mar 2025 (reorg)


# ---- Packages ------------------------------------------------

library(here)
library(stringr)
library(lubridate)
library(tidyverse)
library(sf)

# ---- Paths / Folders ----------------------------------------

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

# ---- Functions ----------------------------


## ---- File load ----------------------------------------------

# load_latest_csv(): Load the most recent timestamped CSV in a folder.
# - Assumes filenames contain YYYY-MM-DD_HHMM.
# - Returns a tibble or NULL (with warning) if none found.

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
  valid <- which(!is.na(datetimes))
  if (!length(valid)) {
    warning("No valid timestamp found in filenames in folder: ", folder)
    return(NULL)
  }
  
  # Identify and load the most recent file
  latest_file <- files[valid][which.max(datetimes[valid])]
  df <- readr::read_csv(latest_file, show_col_types = FALSE)
  
  message("Loaded data from: ", latest_file)
  df
}

## ---- Column name guards -------------------------------------

# check_unique_data_cols(): Ensure raw data columns are unique and not auto-repaired.
# - Stops with a helpful message if duplicates or readr '...2' style names are present.

check_unique_data_cols <- function(df, df_label = "dataframe") {
  if (is.null(df)) return(invisible(TRUE))
  nms <- names(df)
  dups <- unique(nms[duplicated(nms)])
  repaired <- unique(nms[grepl("\\.\\.\\.[0-9]+$", nms)])
  if (length(dups) > 0 || length(repaired) > 0) {
    msg <- c(
      sprintf("Header issues detected in %s. Fix in REDCap and re-export.", df_label),
      if (length(dups) > 0) sprintf("- Duplicate column names: %s", paste(dups, collapse = ", ")),
      if (length(repaired) > 0) sprintf("- Auto-repaired names (e.g. '...2'): %s", paste(repaired, collapse = ", "))
    )
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

# check_unique_renamed_cols(): Ensure post-rename columns are valid (no NA/empty/dups).
# - Stops with a clear message if invalid names or duplicates exist.

check_unique_renamed_cols <- function(df, df_label) {
  if (is.null(df)) return(invisible(TRUE))
  nms <- names(df)
  if (anyNA(nms) || any(!nzchar(nms))) {
    bad <- which(is.na(nms) | !nzchar(nms))
    stop(sprintf("Invalid column names (NA/empty) after renaming in %s: positions %s",
                 df_label, paste(bad, collapse = ", ")), call. = FALSE)
  }
  dups <- unique(nms[duplicated(nms)])
  if (length(dups) > 0) {
    stop(sprintf("Duplicate column names after renaming in %s: %s",
                 df_label, paste(dups, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

## ---- Data dictionary loading / normalisation ----------------

# rename_map_dd: Mapping to standardize DD column names (handles label-vs-API forms).
# - API downloads data dictionary with fieldnames in the first row
# - Exported dd has field lables in the first row

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

# rename_dd_if_needed(): Apply rename_map_dd if needed for exported dd files

rename_dd_if_needed <- function(dd) {
  cols_to_rename <- intersect(names(dd), names(rename_map_dd))
  dd %>% rename_with(~ unname(rename_map_dd[.x]), .cols = all_of(cols_to_rename))
}

# normalise_field_label(): Normalise labels to a comparable key (strip HTML, lower, no punct/space)
# - this is needed to match the labelled field column headings in data retrieved by the API
#   (text from REDCap field labels without html styling) to the field labels in the 
#   data dictionaries (text with html styling)

normalise_field_label <- function(x) {
  x %>%
    gsub("<[^>]+>", "", .) %>%         # strip HTML
    str_squish() %>%                   # whitespace
    str_to_lower() %>%                 # lowercase
    str_replace_all("[[:punct:]]", "") %>% # remove punctuation
    str_replace_all("\\s+", "")        # remove spaces
}

# load_dd(): Read and standardize a DD file, adding normalised label key column
# - Returns the DD tibble or NULL if not found.

load_dd <- function(filename) {
  filepath <- here("data-raw/dds", filename)
  if (!file.exists(filepath)) {
    warning("Data dictionary file not found: ", filename)
    return(NULL)
  }
  dd <- readr::read_csv(filepath, show_col_types = FALSE) %>%
    rename_dd_if_needed() %>%
    mutate(field_label_norm = normalise_field_label(field_label))
  message("Loaded data dictionary: ", filename)
  dd
}

# report_dd_issues(): Emit counts of rows with missing field_name / field_label (for awareness)
# - none should be reported since all dd rows have field_name if intact

report_dd_issues <- function(dd, dd_label) {
  if (is.null(dd)) return(invisible())
  bad_name <- dd %>% filter(is.na(field_name) | !nzchar(field_name))
  bad_lab  <- dd %>% filter(is.na(field_label) | !nzchar(field_label))
  if (nrow(bad_name)) message(dd_label, ": ", nrow(bad_name), " rows missing/empty field_name (ignored).")
  if (nrow(bad_lab))  message(dd_label, ": ", nrow(bad_lab),  " rows missing/empty field_label (ignored).")
}

## ---- Checkbox helpers ---------------------------------------

# parse_cb_choices(): Parse REDCap choices string into code/label rows.
# checkbox fields in the dd have the choices listed in the select_choices_or_calculations column
# Each choice becomes a column in the data
# Need to extract choices from dd to make columns in the data
# - Example: "1, Yes | 0, No" -> tibble(code="1","0", label="Yes","No")

parse_cb_choices <- function(x) {
  if (is.na(x) || !nzchar(x)) return(tibble(code = character(), label = character()))
  tibble(raw = str_split(x, "\\s*\\|\\s*", simplify = FALSE)[[1]]) %>%
    mutate(
      code  = str_trim(str_replace(raw, "^\\s*([^,]+),.*$", "\\1")),
      label = str_trim(str_replace(raw, "^\\s*[^,]+,\\s*", ""))
    ) %>%
    select(code, label) %>%
    filter(nzchar(code), nzchar(label))
}

# option_key_from_label(): Extract trailing (key) from label; fallback to code; slugify.
# - In REDCap I have added a short code for each checkbox in brackets (xxx)
# - Need to extract the code and make sure it is compliant as a column name
# - Ensures lowercase a-z0-9 with underscores.

option_key_from_label <- function(label, fallback_code) {
  key <- stringr::str_match(label, "\\(([^)]+)\\)\\s*$")[,2]
  key <- ifelse(is.na(key) | key == "", fallback_code, key)
  key %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

# build_label_to_field_map(): Map normalised field_label -> field_name (non-checkbox base fields).

build_label_to_field_map <- function(dd) {
  if (is.null(dd)) return(character())
  dd_base <- dd %>%
    filter(!is.na(field_name), nzchar(field_name),
           !is.na(field_label), nzchar(field_label)) %>%
    mutate(field_label_norm = normalise_field_label(field_label)) %>%
    filter(!is.na(field_label_norm), nzchar(field_label_norm))
  stats::setNames(dd_base$field_name, dd_base$field_label_norm)
}

# build_label_to_checkbox_map(): Map normalised checkbox label headers -> target names.
# - target="slug": field_<slug> ; target="opt": field_opt#
build_label_to_checkbox_map <- function(dd, target = c("slug","opt")) {
  target <- match.arg(target)
  if (is.null(dd)) return(character())
  dd_cb <- dd %>%
    filter(field_type == "checkbox",
           !is.na(field_name), nzchar(field_name),
           !is.na(field_label), nzchar(field_label))
  if (!nrow(dd_cb)) return(character())
  
  out <- list()
  for (i in seq_len(nrow(dd_cb))) {
    base_field  <- dd_cb$field_name[i]
    base_label  <- dd_cb$field_label[i]
    choices     <- parse_cb_choices(dd_cb$select_choices_or_calculations[i])
    if (!nrow(choices)) next
    
    ch <- purrr::map2_chr(choices$label, choices$code, ~{
      labelled_header <- paste0(base_label, " (choice=", .x, ")")
      norm_header <- normalise_field_label(labelled_header)
      if (target == "opt") {
        tgt <- paste0(base_field, "_opt", .y)
      } else {
        slug <- option_key_from_label(.x, .y)
        tgt  <- paste0(base_field, "_", slug)
      }
      paste(norm_header, tgt, sep = ":::")
    })
    out[[length(out)+1]] <- ch
  }
  if (!length(out)) return(character())
  vec  <- unlist(out, use.names = FALSE)
  vals <- str_replace(vec, "^.*:::", "")
  keys <- str_replace(vec, ":::.*$",  "")
  keep <- !is.na(keys) & nzchar(keys) & !is.na(vals) & nzchar(vals)
  stats::setNames(vals[keep], keys[keep])
}

# rename_from_labels(): Rename dataframe columns by matching normalised label text via DD.
# - Checkbox map takes precedence over base map; never assigns NA/empty; keeps original if unknown.
rename_from_labels <- function(df, dd, checkbox_target = c("slug","opt")) {
  if (is.null(df) || is.null(dd)) return(df)
  checkbox_target <- match.arg(checkbox_target)
  base_map <- build_label_to_field_map(dd)
  cb_map   <- build_label_to_checkbox_map(dd, target = checkbox_target)
  rename_map <- c(cb_map, base_map) # checkbox first = precedence
  
  cur  <- names(df)
  norm <- normalise_field_label(cur)
  
  new <- vapply(seq_along(cur), function(i) {
    key <- norm[i]
    if (!is.na(key) && key %in% names(rename_map)) {
      proposed <- rename_map[[key]]
      if (length(proposed) == 1 && !is.na(proposed) && nzchar(proposed)) proposed else cur[i]
    } else cur[i]
  }, character(1), USE.NAMES = FALSE)
  
  names(df) <- new
  df
}

# build_cb_code_to_target_map(): Map code-style checkbox names (___/#/__/_opt#) -> final target.
# - target="slug": field_<slug> ; target="opt": field_opt#
build_cb_code_to_target_map <- function(dd, target = c("slug","opt")) {
  target <- match.arg(target)
  if (is.null(dd)) return(character())
  dd_cb <- dd %>% filter(field_type == "checkbox")
  if (!nrow(dd_cb)) return(character())
  
  out <- list()
  for (i in seq_len(nrow(dd_cb))) {
    base_field  <- dd_cb$field_name[i]
    choices     <- parse_cb_choices(dd_cb$select_choices_or_calculations[i])
    if (!nrow(choices)) next
    
    if (target == "opt") {
      to <- paste0(base_field, "_opt", choices$code)
    } else {
      slug <- option_key_from_label(choices$label, choices$code)
      to   <- paste0(base_field, "_", slug)
    }
    from1 <- paste0(base_field, "___", choices$code)  # REDCap triple
    from2 <- paste0(base_field, "__",  choices$code)  # REDCap double
    from3 <- paste0(base_field, "_opt", choices$code) # already explicit opt
    
    out[[length(out)+1]] <- c(stats::setNames(to, from1),
                              stats::setNames(to, from2),
                              stats::setNames(to, from3))
  }
  unlist(out, use.names = TRUE)
}

# rename_cb_codes(): Apply the code-to-target map to a dataframe's column names.
# - Harmonizes any leftover code-style checkbox columns to the chosen scheme.
rename_cb_codes <- function(df, dd, target = c("slug","opt")) {
  map <- build_cb_code_to_target_map(dd, target = match.arg(target))
  if (!length(map) || is.null(df)) return(df)
  hits <- intersect(names(df), names(map))
  if (!length(hits)) return(df)
  idx <- match(hits, names(df))
  names(df)[idx] <- unname(map[hits])
  df
}

## ---- Light cleaning -----------------------------------------

# clean_yesno_values(): Standardize free-text yes/no columns to "Yes"/"No" (strings).
clean_yesno_values <- function(df) {
  if (is.null(df)) return(df)
  yesno_cols <- names(df)[sapply(df, function(col) {
    if (is.logical(col)) return(FALSE)
    vals <- na.omit(unique(col))
    vals_chr <- tolower(str_trim(as.character(vals)))
    length(vals_chr) > 0 && all(vals_chr %in% c("yes", "no"))
  })]
  if (!length(yesno_cols)) return(df)
  df %>%
    mutate(across(all_of(yesno_cols), ~ dplyr::case_when(
      tolower(str_trim(as.character(.))) == "yes" ~ "Yes",
      tolower(str_trim(as.character(.))) == "no"  ~ "No",
      TRUE ~ as.character(NA)
    )))
}

# coerce_checkbox_logical(): Convert checkbox columns (field_<anything>) to logical TRUE/FALSE/NA.
coerce_checkbox_logical <- function(data, dd) {
  if (is.null(data) || is.null(dd)) return(data)
  cb_fields <- dd %>% filter(field_type == "checkbox") %>% pull(field_name)
  if (!length(cb_fields)) return(data)
  cb_cols <- names(data)[Reduce(`|`, lapply(cb_fields, function(f) {
    stringr::str_starts(names(data), paste0("^", f, "_"))
  }), init = rep(FALSE, length(names(data))))]
  if (!length(cb_cols)) return(data)
  
  data %>%
    mutate(across(all_of(cb_cols), ~ {
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
        is.factor(x) ~ {
          lx <- tolower(str_trim(as.character(x)))
          dplyr::case_when(
            lx %in% c("1", "checked", "true", "yes") ~ TRUE,
            lx %in% c("0", "unchecked", "false", "no") ~ FALSE,
            TRUE ~ NA
          )
        },
        TRUE ~ as.logical(x)
      )
      as.logical(x)
    }))
}

## ---- Type conversions (dictionary-driven) --------------------

# convert_field_types(): Convert columns to date/datetime/integer/numeric/factor/character using DD.
# - Also converts any remaining yes/no string columns to logical TRUE/FALSE/NA.
convert_field_types <- function(data, dd) {
  if (is.null(data) || is.null(dd)) return(data)
  
  field_groups <- list(
    date = dd %>% filter(field_type == "text", text_validation_type_or_show_slider_number == "date_dmy") %>% pull(field_name),
    datetime = dd %>% filter(field_type == "text", text_validation_type_or_show_slider_number == "datetime_dmy") %>% pull(field_name),
    integer = dd %>% filter(field_type == "text", text_validation_type_or_show_slider_number == "integer") %>% pull(field_name),
    numeric = dd %>% filter((field_type == "text" & text_validation_type_or_show_slider_number == "number") | (field_type == "calc")) %>% pull(field_name),
    factor  = dd %>% filter(field_type %in% c("radio", "dropdown", "yesno") | (field_type == "dropdown" & text_validation_type_or_show_slider_number == "autocomplete")) %>% pull(field_name),
    calctext = dd %>% filter(!is.na(field_annotation) & str_starts(field_annotation, "@CALCTEXT")) %>% pull(field_name)
  )
  field_groups$factor <- unique(c(field_groups$factor, field_groups$calctext))
  converted <- unlist(field_groups[c("integer","numeric","date","datetime","factor")])
  
  field_groups$text <- dd %>%
    filter(field_type %in% c("text","notes","textarea","descriptive")) %>%
    pull(field_name) %>%
    setdiff(converted)
  
  data <- data %>%
    mutate(across(any_of(field_groups$date),     ~ if (is.character(.)) lubridate::dmy(.) else .)) %>%
    mutate(across(any_of(field_groups$datetime), ~ if (is.character(.)) lubridate::parse_date_time(., orders = c("dmy HM","dmy HMS","ymd HM","ymd HMS")) else .)) %>%
    mutate(across(any_of(field_groups$integer),  as.integer)) %>%
    mutate(across(any_of(field_groups$numeric),  ~ if (is.numeric(.)) . else readr::parse_number(as.character(.)))) %>%
    mutate(across(any_of(field_groups$factor),   as.factor)) %>%
    mutate(across(any_of(field_groups$text),     as.character)) %>%
    mutate(across(any_of("record_id"),          as.character))
  
  # Optional: detect & convert residual yes/no to logical
  yesno_cols <- names(data)[sapply(data, function(col) {
    if (is.logical(col)) return(FALSE)
    vals <- na.omit(unique(col))
    vals_chr <- tolower(stringr::str_trim(as.character(vals)))
    length(vals_chr) > 0 && all(vals_chr %in% c("yes","no"))
  })]
  if (length(yesno_cols)) {
    data <- data %>%
      mutate(across(all_of(yesno_cols), ~ dplyr::case_when(
        tolower(as.character(.)) == "yes" ~ TRUE,
        tolower(as.character(.)) == "no"  ~ FALSE,
        TRUE ~ NA
      )))
  }
  data
}

## ---- GIS loader ----------------------------------------------

# load_gis_if_present(): Load a GeoJSON (if present), return WGS84 layer and EPSG:3832 variant.
# - Returns list(layer_4326=..., layer_3832=...) or NULL if missing/empty.
load_gis_if_present <- function(path_geojson) {
  if (!file.exists(path_geojson)) {
    warning("GIS data file not found: ", path_geojson)
    return(NULL)
  }
  message("Loading GIS data from: ", path_geojson)
  layer <- tryCatch(st_read(path_geojson, quiet = TRUE), error = function(e) { warning("Failed to load GIS: ", e$message); NULL })
  if (is.null(layer) || !nrow(layer)) return(NULL)
  
  crs_epsg <- tryCatch(st_crs(layer)$epsg, error = function(e) NA_integer_)
  if (!is.na(crs_epsg) && crs_epsg != 4326) {
    layer <- st_transform(layer, 4326)
  }
  layer_3832 <- tryCatch(st_transform(layer, 3832), error = function(e) { warning("Could not transform to EPSG:3832 (", e$message, ")"); NULL })
  list(layer_4326 = layer, layer_3832 = layer_3832)
}

# ---- Operations (end-to-end flow) -------------------------

## ---- Load raw datasets --------------------------------------

screening_data  <- load_latest_csv(here("data-raw/screening"))
household_data  <- load_latest_csv(here("data-raw/household"))
treatment_data  <- load_latest_csv(here("data-raw/treatment"))
ea_data         <- load_latest_csv(here("data-raw/ea"))

check_unique_data_cols(screening_data, "screening_data (raw)")
check_unique_data_cols(household_data, "household_data (raw)")
check_unique_data_cols(treatment_data, "treatment_data (raw)")
check_unique_data_cols(ea_data,        "ea_data (raw)")

## ---- Load DDs ------------------------------------------------

screening_dd <- load_dd("screening_dd.csv")
household_dd <- load_dd("household_dd.csv")
treatment_dd <- load_dd("treatment_dd.csv")
ea_dd        <- load_dd("ea_dd.csv")

report_dd_issues(screening_dd, "screening_dd")
report_dd_issues(household_dd, "household_dd")
report_dd_issues(treatment_dd, "treatment_dd")
report_dd_issues(ea_dd,        "ea_dd")

## ---- Optional diagnostics: unmapped label headers ------------

# check_unmapped(): Show which current headers don't match any known normalised DD labels.
check_unmapped <- function(df, dd) {
  if (is.null(df) || is.null(dd)) return(character())
  base_map <- build_label_to_field_map(dd)
  cb_map   <- build_label_to_checkbox_map(dd, target = "slug") # compare against final target
  known    <- union(names(base_map), names(cb_map))
  cur_norm <- normalise_field_label(names(df))
  setdiff(cur_norm, known)
}
if (length(u <- check_unmapped(screening_data, screening_dd))) message("Unmapped screening headers: ", paste(u, collapse = ", "))
if (length(u <- check_unmapped(household_data, household_dd))) message("Unmapped household headers: ", paste(u, collapse = ", "))
if (length(u <- check_unmapped(treatment_data, treatment_dd))) message("Unmapped treatment headers: ", paste(u, collapse = ", "))
if (length(u <- check_unmapped(ea_data,        ea_dd)))        message("Unmapped EA headers: ", paste(u, collapse = ", "))

## ---- Rename phase A: labels -> names (fields and checkbox options) ------------------

# Use final target "slug" so we end up with single-underscore names everywhere.
screening_data <- rename_from_labels(screening_data, screening_dd, checkbox_target = "slug")
household_data <- rename_from_labels(household_data, household_dd, checkbox_target = "slug")
treatment_data <- rename_from_labels(treatment_data, treatment_dd, checkbox_target = "slug")
ea_data        <- rename_from_labels(ea_data,        ea_dd,        checkbox_target = "slug")

## ---- Rename phase B: convert any code-style checkbox names to the same slug scheme -------------

screening_data <- rename_cb_codes(screening_data, screening_dd, target = "slug")
household_data <- rename_cb_codes(household_data, household_dd, target = "slug")
treatment_data <- rename_cb_codes(treatment_data, treatment_dd, target = "slug")
ea_data        <- rename_cb_codes(ea_data,        ea_dd,        target = "slug")

## ---- Post-rename safety check --------------------------------

check_unique_renamed_cols(screening_data, "screening_data (post-rename)")
check_unique_renamed_cols(household_data, "household_data (post-rename)")
check_unique_renamed_cols(treatment_data, "treatment_data (post-rename)")
check_unique_renamed_cols(ea_data,        "ea_data (post-rename)")

## ---- Light cleaning & typing ---------------------------------

screening_data <- screening_data %>% clean_yesno_values() %>% coerce_checkbox_logical(screening_dd) %>% convert_field_types(screening_dd)
household_data <- household_data %>% clean_yesno_values() %>% coerce_checkbox_logical(household_dd) %>% convert_field_types(household_dd)
treatment_data <- treatment_data %>% clean_yesno_values() %>% coerce_checkbox_logical(treatment_dd) %>% convert_field_types(treatment_dd)
ea_data        <- ea_data        %>% clean_yesno_values() %>% coerce_checkbox_logical(ea_dd)        %>% convert_field_types(ea_dd)

# ---- GIS (optional) ------------------------------------------

gis_paths <- here("data-raw/gis/KIR_EA_Census2020FINAL.geojson")
gis_layers <- load_gis_if_present(gis_paths)
layer_ki_ea       <- if (!is.null(gis_layers)) gis_layers$layer_4326 else NULL
layer_ki_ea_3832  <- if (!is.null(gis_layers)) gis_layers$layer_3832 else NULL
