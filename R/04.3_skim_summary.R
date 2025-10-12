# Title and description --------------------------------------------
# Making skim outputs for LLM assistance
# Data are owned by University of Sydney and Kiribati MHMS
# Author:           Jeremy Hill
# Date commenced:   11 Mar 2025

# Packages ----------------------------------------------------------
library(tidyverse)
library(skimr)
library(here)

# 1) Function to generate and save skim summaries as CSVs ---------------------

generate_skim_summary <- function(dataset, dataset_name) {
  output_dir <- here("data-processed", "skim_summary")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  skim_summary <- skim(dataset)
  csv_path <- file.path(output_dir, paste0(dataset_name, "_skim.csv"))
  readr::write_csv(skim_summary, csv_path)
  message("Skim summary saved for: ", dataset_name, " --> ", csv_path)
  return(csv_path)
}

## Datasets to skim --------------------------------------------------
datasets <- list(
  screening_data = screening_data,
  household_data = household_data,
  ea_data        = ea_data,
  treatment_data = treatment_data
)

## Run skims ---------------------------------------------------------

skim_files <- purrr::map2(datasets, names(datasets), ~ generate_skim_summary(.x, .y))

# 2) Concatenate project R scripts from ./R into data-processed/R.txt ---------------

concat_R_txt <- function(output_file = here::here("data-processed", "R.txt"),
                         source_dir  = here::here("R"),
                         recursive   = FALSE,
                         echo_files  = TRUE) {
  src <- normalizePath(source_dir, winslash = "/", mustWork = FALSE)
  
  if (!dir.exists(src)) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    cat("# Source directory not found: ", src, "\n", file = output_file, sep = "")
    message("Directory not found: ", src)
    return(invisible(tibble::tibble(file = character(), n_lines = integer())))
  }
  
  files <- list.files(src, pattern = "\\.R$", full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  con <- file(output_file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  
  sep_line <- paste(rep("-", 80), collapse = "")
  time_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  
  # Header
  cat("# Project code bundle (R/)\n",
      "# Generated: ", time_str, "\n",
      "# Source dir: ", src, "\n",
      "# Files included: ", length(files), "\n\n",
      sep = "", file = con)
  
  if (length(files) == 0) {
    message("Code bundle written: ", output_file, " (0 files)")
    return(invisible(tibble::tibble(file = character(), n_lines = integer())))
  }
  
  stats <- purrr::map_dfr(files, function(f) {
    rel <- basename(f)
    txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    header <- c(sep_line,
                paste0("# FILE: ", rel),
                paste0("# LAST MODIFIED: ",
                       format(file.info(f)$mtime, "%Y-%m-%d %H:%M:%S %Z")),
                sep_line)
    cat(paste(c(header, txt, "", ""), collapse = "\n"), file = con)
    tibble::tibble(file = rel, n_lines = length(txt))
  })
  
  if (isTRUE(echo_files)) {
    cat("Included files:\n", paste0(" - ", basename(files)), sep = "\n")
  }
  
  message("Code bundle written: ", output_file,
          " (", nrow(stats), " files; ",
          format(sum(stats$n_lines), big.mark = ","), " lines)")
  invisible(stats)
}

## Call it (non-recursive; set recursive = TRUE if you add subfolders under R/) ---------------

concat_R_txt(output_file = here("data-processed", "R.txt"),
             source_dir  = here("R"),
             recursive   = FALSE)
