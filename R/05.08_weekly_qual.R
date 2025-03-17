library(ggplot2)
library(cowplot)
library(dplyr)
library(scales)

# Define important events (adjust dates and labels as needed)
events_df <- data.frame(
  week_reg = as.Date(c("2023-03-27",
                       "2023-08-07",
                       "2023-10-30",
                       "2024-04-08",
                       "2024-10-14",
                       "2025-01-13")),
  event_label = c("Commence in Temakin,\nstockout of TPT meds", 
                  "Lab tech start",
                  "Stockout of TB meds,\nreplenishment of TPT", 
                  "Recommence screening\nin Temakin",
                  "Xpert stockout",
                  "New staff")
)

# Define proper names for the indicators
indicator_labels <- c(
  "anyrx_pct" = "Any Treatment",
  "tbdec_pct" = "TB Outcome Assigned",
  "tst_read_pct" = "TST Read",
  "tst_place_pct" = "TST Placed",
  "cxr_pct" = "XR Done if Eligible",
  "xpert_pct" = "Xpert Test Done",
  "reg" = "People Registered",
  "tpt_start" = "Started Treatment"
)

weekly_long_p1 <- weekly_long %>% 
  filter(!(Indicator %in% c("reg", "tpt_start"))) %>% 
  mutate(Indicator = recode(Indicator, !!!indicator_labels))

weekly_long_p2 <- weekly_long %>%
  filter(Indicator %in% c("reg", "tpt_start")) %>%
  mutate(Indicator = recode(Indicator, !!!indicator_labels))

max_y_p2 <- max(weekly_long_p2$Value, na.rm = TRUE) * 1.25  # Place text slightly above max value


p2 <- ggplot(weekly_long_p2, aes(x = week_reg, y = Value, color = Indicator, group = Indicator)) +
  geom_line(size = 0.5) +
  scale_y_continuous(name = "Activity count \nReg and TPT start", limits = c(0, max_y_p2)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  scale_color_manual(values = c("People Registered" = "grey70", "Started Treatment" = "grey40")) +
  labs(color = "Indicator") +
  theme_light() +
  theme(
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  
  # Add vertical lines for events
  geom_vline(data = events_df, aes(xintercept = as.numeric(week_reg)), linetype = "dashed", color = "grey") +
  
  # Add event labels above the plot
  geom_text(
    data = events_df, 
    aes(x = week_reg + 3, y = max_y_p2 * 0.8, label = event_label), 
    angle = 0,
    hjust = 0, 
    vjust = 0, 
    size = 2.5, 
    color = "black", 
    inherit.aes = FALSE)

p1 <- ggplot(weekly_long_p1, aes(x = week_reg, y = Value, color = Indicator, group = Indicator)) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  scale_y_continuous(labels = percent_format(scale = 1), name = "Percentage (%)") +  # Left y-axis
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_viridis_d(option = "F", begin = 0.2, end = 0.8) +
  labs(x = "Time (Weeks)", color = "Indicator") +
  theme_light() +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

aligned_plots <- align_plots(p2, p1, align = "v", axis = "lr")

plot_05.08 <- ggdraw() +
  draw_plot(aligned_plots[[1]], 0, 0.7, 1, 0.3) + 
  draw_plot(aligned_plots[[2]], 0, 0, 1, 0.7) 

# Display the final plot
plot_05.08

# Save plot image
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.08_", current_date, ".png")

ggsave(filename = file.path(output_dir, output_filename), plot = plot_05.08, width = 8, height = 6, dpi = 300)
