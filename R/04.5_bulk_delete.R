library(REDCapR)
library(dplyr)

uri <- "https://redcap.sydney.edu.au/api/"
token <- Sys.getenv("RCAPI_PEARL_screen")
report_id <- 39575

# Retrieve empty records from the report
empty_records <- REDCapR::redcap_report(
  redcap_uri = uri,
  token = token,
  report_id = report_id
)$data

# Filter records with record_id according to conditions (default set to 100000 to ensure explicit)
empty_records <- empty_records %>%
  mutate(record_id = as.integer(record_id)) %>%
  filter(record_id >= 100000)

cat("🔍 Found", nrow(empty_records), "empty records.\n")
print(head(empty_records$record_id, 10))

# Delete records and print result

STOP_DELETION <- TRUE  # Change this to FALSE to enable deletion

if (STOP_DELETION) {
  stop("Deletion is disabled by default. Set STOP_DELETION <- FALSE to proceed.")
}

delete_result <- REDCapR::redcap_delete(
  redcap_uri = uri,
  token = token,
  records_to_delete = records_to_delete,
  verbose = TRUE
)

print(delete_result)