# Analysis data helpers ------------------------------------------------------

# Purpose: load the tidy data bundle produced by 03_tidy_data.R
# Usage: source("R/analysis_data.R"); get_analysis_data()

library(here)
library(qs)

## Path helpers ---------------------------------------------------------------

tidy_data_dir <- file.path(here("data-processed", "tidy-data-bundles"))
tidy_data_latest_path <- file.path(tidy_data_dir, "tidy_data_latest.qs")


## Function to load latest tidy data bundle ------------------------------------------------------

# Load a tidy data bundle saved by 03_tidy_data.R. This does not build the data;
# run 02_* and 03_* first to create tidy_data_YYYY-MM-DD.qs and tidy_data_latest.qs.

get_tidy_data <- function(path = tidy_data_latest_path,
                          dir = tidy_data_dir,
                          quiet = FALSE) {
  
  if (!dir.exists(dir)) {
    stop("No data-processed directory found. Please run 02_* and 03_* to create the tidy data bundle.")
  }
  
  if (!file.exists(path)) {
    stop("Tidy data bundle not found. Please run 02_* and 03_* to create the tidy data bundle.")
  }
  
  if (!quiet) {
    message("Loading tidy data bundle from ", path)
  }
  
  qread(path)
}

## Function to attach latest tidy data bundle into calling environment --------------------------------

attach_tidy_data <- function(path = tidy_data_latest_path,
                             dir = tidy_data_dir,
                             quiet = FALSE,
                             env = parent.frame()) {
  
  td <- get_tidy_data(path = path, dir = dir, quiet = quiet)
  list2env(td, envir = env)
  invisible(td)
}