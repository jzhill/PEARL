# Analysis data helpers ------------------------------------------------------

# Purpose: load the tidy data bundle produced by 03_tidy_data.R
# Usage: source("R/analysis_data.R"); get_analysis_data()

library(here)
library(glue)
library(qs)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

## Path helpers ---------------------------------------------------------------

# Directory holding the versioned tidy data bundles created by 03_tidy_data.R
# (e.g., data-processed/tidy_data_YYYY-MM-DD.qs and tidy_data_latest.qs)
tidy_data_dir <- function() {
  here("data-processed")
}

# Construct the expected bundle path given a version label
# - version = "latest" → tidy_data_latest.qs
# - version = "YYYY-MM-DD" → tidy_data_YYYY-MM-DD.qs
tidy_data_path <- function(version = "latest", dir = tidy_data_dir()) {
  if (is.null(version) || identical(version, "latest")) {
    return(file.path(dir, "tidy_data_latest.qs"))
  }

  file.path(dir, glue("tidy_data_{version}.qs"))
}

# Locate the most recent dated tidy data bundle (excluding the "latest" pointer)
latest_dated_tidy_bundle <- function(dir = tidy_data_dir()) {
  if (!dir.exists(dir)) {
    return(NULL)
  }

  tibble(path = Sys.glob(file.path(dir, "tidy_data_*.qs"))) %>%
    filter(!str_detect(path, "tidy_data_latest\\.qs$")) %>%
    mutate(mtime = file.info(path)$mtime) %>%
    arrange(desc(mtime)) %>%
    slice_head(n = 1) %>%
    pull(path) %>%
    pluck(1)
}

## Load tidy data bundle ------------------------------------------------------

# Load a tidy data bundle saved by 03_tidy_data.R. This does not build the data;
# run 02_* and 03_* first to create tidy_data_YYYY-MM-DD.qs and tidy_data_latest.qs.
get_analysis_data <- function(version = "latest", quiet = FALSE, dir = tidy_data_dir()) {
  if (!dir.exists(dir)) {
    stop("No data-processed directory found. Please run 02_* and 03_* to create the tidy data bundle.")
  }

  bundle_path <- tidy_data_path(version, dir)

  if (identical(version, "latest") && !file.exists(bundle_path)) {
    # Fall back to the most recent dated bundle if the pointer is missing
    bundle_path <- latest_dated_tidy_bundle(dir)
  }

  if (is.null(bundle_path) || !file.exists(bundle_path)) {
    stop("Tidy data bundle not found. Please run 02_* and 03_* to create tidy_data_YYYY-MM-DD.qs.")
  }

  if (!quiet) {
    message("Loading tidy data bundle from ", bundle_path)
  }

  qs::qread(bundle_path)
}
