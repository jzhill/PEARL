library(here)
library(fs)

# Define common parameters used in plot scripts
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))

# Create directory if it doesn't exist
if (!dir_exists(output_dir)) {
  dir_create(output_dir)
}

# Detect and list all relevant scripts
scripts_to_run <- list.files(here("R"), pattern = "^(05|06).*\\.R$", full.names = TRUE)
message("Detected scripts: ", paste(basename(scripts_to_run), collapse = ", "))

# Run each script and show progress
for (script in scripts_to_run) {
  script_name <- basename(script)
  message("▶ Running: ", script_name)
  
  tryCatch(
    {
      source(script)
      message("✔ Completed: ", script_name)
    },
    error = function(e) {
      message("❌ Error in ", script_name, ": ", e$message)
    }
  )
}

message("All scripts processed. Outputs saved in: ", output_dir)
