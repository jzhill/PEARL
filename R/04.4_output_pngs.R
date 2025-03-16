library(ggplot2)
library(lubridate)
library(here)
library(fs)
library(flextable)

# Clear existing objects from previous runs
rm(list = ls(pattern = "^(plot_|table_|map_)", envir = .GlobalEnv))

# Set today's date
report_date <- Sys.Date()
report_dir <- here("figures", paste0("Outputs_", report_date))

# Create directory if it doesn't exist
if (!dir_exists(report_dir)) {
  dir_create(report_dir)
}

# Run all relevant scripts from ~/R with prefixes 05 or 06
scripts_to_run <- list.files(here("R"), pattern = "^(05|06).*\\.R$", full.names = TRUE)
lapply(scripts_to_run, source)

# Function to save plots
save_plot <- function(plot, filename) {
  ggsave(file.path(report_dir, paste0(filename, "_", format(Sys.Date(), "%Y%m%d"), ".png")), 
         plot, width = 8, height = 6, dpi = 300)
  message("Saved: ", filename)
}

# Function to save flextable as an image
save_table <- function(table, filename) {
  if (inherits(table, "flextable")) {
    flextable::save_as_image(table, 
                             path = file.path(report_dir, paste0(filename, "_", format(Sys.Date(), "%Y%m%d"), ".png")))
    message("Saved: ", filename)
  }
}

# Identify objects matching 'plot_', 'map_', or 'table_'
all_objects <- ls()
plot_objects <- grep("^plot_", all_objects, value = TRUE)
map_objects <- grep("^map_", all_objects, value = TRUE)
table_objects <- grep("^table_", all_objects, value = TRUE)

# Save only identified plot, map, and table objects
for (obj in c(plot_objects, map_objects)) {
  obj_value <- get(obj)
  if (inherits(obj_value, "ggplot")) {
    save_plot(obj_value, obj)
  }
}

for (obj in table_objects) {
  obj_value <- get(obj)
  if (inherits(obj_value, "flextable")) {
    save_table(obj_value, obj)
  }
}

message("All reports generated and saved in: ", report_dir)
