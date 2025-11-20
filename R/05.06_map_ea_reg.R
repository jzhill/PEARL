library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# Create a new column for fill color classification
layer_betio_ea_3832 <- layer_betio_ea_3832 %>%
  mutate(fill_category = ifelse(joined_reg < 100, "Below 100", "Above 100"))

# Plot map
map_05.06 <- ggplot(layer_betio_ea_3832) +
  geom_sf(aes(fill = joined_reg), color = "black", size = 0.2) +
  scale_fill_gradient(low = "lightgrey", high = "darkred", na.value = "lightgrey") +
  labs(title = "Screening Counts by Enumeration Area (VID 716)",
       fill = "Count") +
  theme_light()

map_05.06

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("map_05.06_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = map_05.06, width = 8, height = 4, dpi = 300)
