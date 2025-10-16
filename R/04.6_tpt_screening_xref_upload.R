# 04.6_tpt_xref_upload.R
# Minimal helper to set screening$prerx_start_conf = 1 for
# every record_id present in treatment_data (already in memory).
#
# Output: creates a small upload frame `tpt_xref_ul` and (optionally) writes it to REDCap.

# Packages -----------------------------------------

library(dplyr)
library(REDCapR)


# Parameters ------------------------------

uri <- "https://redcap.sydney.edu.au/api/"
token_screen <- Sys.getenv("RCAPI_PEARL_screen")

# Build the upload frame ---------------------------------------------------

stopifnot(exists("treatment_data"))

# Keep record_id and mark confirmation flag
tpt_xref_ul <- treatment_data %>%
  select(record_id) %>%
  mutate(
    record_id = as.character(record_id),
    prerx_start_conf = 1L
  )

message("Prepared tpt_xref_ul with ", nrow(tpt_xref_ul), " rows.")

# Validate and (optionally) write to REDCap --------------------------------

REDCapR::validate_for_write(tpt_xref_ul)

if (interactive()) {
  choice <- menu(choices = c("Yes", "No"),
                 title = "Write prerx_start_conf=1 to Screening now?")
  
  if (choice == 1) {
    message("Writing data to REDCap...")
    
    result <- REDCapR::redcap_write(
      ds = tpt_xref_ul,
      redcap_uri = uri,
      token = token_screen,
      overwrite_with_blanks = TRUE,
      verbose = TRUE
    )
    message("Data written to REDCap. Response: ")
    print(result)
  } else {
    message("Data was not written to REDCap.")
  }
} else {
  message("Non-interactive mode detected; data was not written to REDCap.")
}
