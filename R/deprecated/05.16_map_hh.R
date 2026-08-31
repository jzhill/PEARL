library(ggplot2)
library(sf)

map_05.16 <- ggplot() +
  geom_sf(data = layer_betio_ea_3832, fill = "white", color = "black") +
  geom_sf(data = layer_hh_betio_3832, aes(color = hh_status), size = 1, alpha = 0.8) +
  theme_light()

map_05.16

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("map_05.16_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = map_05.16, width = 8, height = 4, dpi = 300)
