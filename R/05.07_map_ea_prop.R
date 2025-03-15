library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# Create a new column for fill color classification
layer_betio_ea_3832 <- layer_betio_ea_3832 %>%
  mutate(fill_category_prop_reg = ifelse(joined_prop_reg < 0.2, "Below 20pc", "Above 20pc"))

# Create labels with EA ID and proportion
layer_betio_ea_3832 <- layer_betio_ea_3832 %>%
  mutate(label_text = paste0(ea_2020, "\n", round(joined_prop_reg * 100, 1), "%"))

# Plot map
map_05.07 <- ggplot(layer_betio_ea_3832) +
  geom_sf(aes(fill = joined_prop_reg), color = "black", size = 0.2) +
  scale_fill_gradient(low = "lightgrey", high = "darkgreen", na.value = "lightgrey") +
  labs(title = "Screening Proportion by Enumeration Area (VID 716)",
       fill = "Proportion") +
  theme_minimal()

map_05.07