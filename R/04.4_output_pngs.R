library(here)
library(fs)

# Define common parameters used in plot scripts
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))

# Create directory if it doesn't exist
if (!dir_exists(output_dir)) {
  dir_create(output_dir)
}

# Run all relevant scripts from ~/R with prefixes 05 or 06
scripts_to_run <- list.files(here("R"), pattern = "^(05|06).*\\.R$", full.names = TRUE)
message("Running scripts: ", paste(basename(scripts_to_run), collapse = ", "))
lapply(scripts_to_run, source)

message("All plots and tables have been generated and saved in: ", output_dir)
