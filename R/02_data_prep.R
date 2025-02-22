# Title and description --------------------------------------------

# Loading and preparing PEARL data from data-raw folders
# Data are owned by University of Sydney and Kiribati MHMS

# Author:           Jeremy Hill
# Date commenced:   23 Feb 2025


# Packages -----------------------------------------

library(here)
library(stringr)
library(lubridate)

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

