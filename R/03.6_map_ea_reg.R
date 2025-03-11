library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# Define file paths
geojson_path <- "data-raw/gis/KIR_EA_Census2020FINAL.geojson"

# Check if the geojson file exists
if (!file.exists(geojson_path)) {
  stop("GeoJSON file not found: ", geojson_path)
}

# Read the geojson file
kiribati_ea <- st_read(geojson_path, quiet = TRUE)

# Check CRS (should be EPSG:4326)
if (st_crs(kiribati_ea)$epsg != 4326) {
  warning("The CRS is not EPSG:4326. Please check the data.")
}

# Ensure both columns have the same type for joining
kiribati_ea$ea_2020 <- as.character(kiribati_ea$ea_2020)
ea_data$record_id <- as.character(ea_data$record_id)

# Merge with GIS data
kiribati_ea <- kiribati_ea %>%
  left_join(ea_data, by = c("ea_2020" = "record_id"))

# Replace NA counts with 0 (for EAs without screening data)
kiribati_ea$pop_reg_screen_new[is.na(kiribati_ea$pop_reg_screen_new)] <- 0

# Filter to only include EAs with vid of 716
kiribati_ea <- kiribati_ea %>%
  filter(vid == 716)

# Create a new column for fill color classification
kiribati_ea <- kiribati_ea %>%
  mutate(fill_category = ifelse(pop_reg_screen_new < 100, "Below 100", "Above 100"))

# Plot map
ggplot(kiribati_ea) +
  geom_sf(aes(fill = pop_reg_screen_new), color = "black", size = 0.2) +
  geom_sf_text(aes(label = pop_reg_screen_new), size = 3, color = "white") +
  scale_fill_gradient(low = "lightgrey", high = "darkred", na.value = "lightgrey") +
  labs(title = "Screening Counts by Enumeration Area (VID 716)",
       fill = "Count") +
  theme_minimal()
