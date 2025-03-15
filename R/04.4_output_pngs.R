library(ggplot2)
library(lubridate)
library(here)
library(fs)
library(flextable)
library(webshot2)

# Set today's date
report_date <- Sys.Date()
report_dir <- here("reports", paste0("PEARL_Sitrep_", report_date))

# Create directory if it doesn't exist
if (!dir_exists(report_dir)) {
  dir_create(report_dir)
}

# List of scripts to run (located in ~/R with prefixes 05 or 06)
scripts_to_run <- list.files(here("R"), pattern = "^(05|06)_.*\\.R$", full.names = TRUE)

# Source all the scripts
for (script in scripts_to_run) {
  source(script)
}

# Function to save a plot with timestamp
save_plot_with_date <- function(plot, filename) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filepath <- file.path(report_dir, paste0(filename, "_", timestamp, ".png"))
  ggsave(filepath, plot, width = 8, height = 6, dpi = 300)
  message("Saved: ", filepath)
}

# Function to save flextable as an image without keeping the HTML file
save_table_as_image <- function(table, filename) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  html_path <- tempfile(fileext = ".html")  # Temporary HTML file
  image_path <- file.path(report_dir, paste0(filename, "_", timestamp, ".png"))
  save_as_html(table, path = html_path)
  webshot2::webshot(html_path, image_path)
  unlink(html_path)  # Delete the temporary HTML file
  message("Saved: ", image_path)
}

# Identify all objects in the environment that match 'plot_', 'map_', or 'table_'
all_objects <- ls()
plot_objects <- grep("^plot_", all_objects, value = TRUE)
map_objects <- grep("^map_", all_objects, value = TRUE)
table_objects <- grep("^table_", all_objects, value = TRUE)

# Save all identified plot and map objects
for (plot_name in c(plot_objects, map_objects)) {
  plot_obj <- get(plot_name)
  save_plot_with_date(plot_obj, plot_name)
}

# Save tables as images
for (table_name in table_objects) {
  table_obj <- get(table_name)
  save_table_as_image(table_obj, table_name)
}

message("All reports generated and saved in: ", report_dir)
