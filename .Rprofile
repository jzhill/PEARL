# Create directory structure (base R only - nothing loaded yet)
dirs <- c(
  "data-processed",
  "data-raw",
  "data-raw/dds",
  "data-raw/ea",
  "data-raw/gis",
  "data-raw/household",
  "data-raw/screening",
  "data-raw/treatment",
  "figures",
  "ea_maps",
  "Quarto",
  "R",
  "reports"
)

lapply(dirs, function(d) {
  dir.create(file.path(getwd(), d), showWarnings = FALSE, recursive = TRUE)
})

# Bootstrap and load renv
if (!file.exists("renv/activate.R")) {
  renv::init()
} else {
  source("renv/activate.R")
}

cat("\nWelcome to", basename(getwd()), "\n")
