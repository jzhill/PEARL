library(ggplot2)
library(sf)

map_hh <- ggplot() +
  geom_sf(data = layer_betio_ea_3832, fill = "white", color = "black") +
  geom_sf(data = layer_households, aes(color = hh_status), size = 1, alpha = 0.8) +
  theme_light()

map_hh