library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# Create a new column for fill color classification
layer_betio_ea_3832 <- layer_betio_ea_3832 %>%
  mutate(fill_category = ifelse(joined_pop_reg_screen_new < 100, "Below 100", "Above 100"))

# Plot map
map_05.06 <- ggplot(layer_betio_ea_3832) +
  geom_sf(aes(fill = joined_pop_reg_screen_new), color = "black", size = 0.2) +
  geom_sf_text(aes(label = joined_pop_reg_screen_new), size = 3, color = "white") +
  scale_fill_gradient(low = "lightgrey", high = "darkred", na.value = "lightgrey") +
  labs(title = "Screening Counts by Enumeration Area (VID 716)",
       fill = "Count") +
  theme_minimal()

map_05.06