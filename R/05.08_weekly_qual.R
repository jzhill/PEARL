library(ggplot2)
library(cowplot)
library(dplyr)
library(scales)
library(here)

# Important events (now keyed to `period_start`)
events_df <- tibble::tibble(
  period_start = as.Date(c(
    "2023-03-27","2023-08-07","2023-10-30",
    "2024-04-08","2024-10-14","2025-01-13"
  )),
  event_label = c(
    "Commence in Temakin,\nstockout of TPT meds",
    "Lab tech start",
    "Stockout of TB meds,\nreplenishment of TPT",
    "Recommence screening\nin Temakin",
    "Xpert stockout",
    "New staff"
  )
)

# Pretty names for indicators
indicator_labels <- c(
  "anyrx_pct"   = "Any Treatment",
  "tbdec_pct"   = "TB Outcome Assigned",
  "tst_read_pct"= "TST Read",
  "tst_place_pct"="TST Placed",
  "cxr_pct"     = "XR Done if Eligible",
  "cxr_res_pct" = "XR Result Available",
  "xpert_pct"   = "Xpert Test Done",
  "reg"         = "People Registered",
  "tpt_start"   = "Started Treatment"
)

weekly_long_p1 <- weekly_long %>% 
  filter(Indicator %in% c("anyrx_pct","tbdec_pct","tst_read_pct","tst_place_pct","cxr_pct","cxr_res_pct","xpert_pct")) %>% 
  mutate(Indicator = recode(Indicator, !!!indicator_labels))

weekly_long_p2 <- weekly_long %>%
  filter(Indicator %in% c("reg","tpt_start")) %>%
  mutate(Indicator = recode(Indicator, !!!indicator_labels))

max_y_p2 <- max(weekly_long_p2$Value, na.rm = TRUE) * 1.25

plot_05.08_p2 <- ggplot(weekly_long_p2, aes(x = period_start, y = Value, color = Indicator, group = Indicator)) +
  geom_line(size = 0.7) +
  scale_y_continuous(name = "Activity count \nReg and TPT start", limits = c(0, max_y_p2)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  scale_color_manual(values = c("People Registered" = "grey70", "Started Treatment" = "grey40")) +
  labs(color = "Indicator") +
  theme_light() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.y  = element_text(size = 9),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  # Events
  geom_vline(data = events_df, aes(xintercept = as.numeric(period_start)),
             linetype = "dashed", color = "grey") +
  geom_text(
    data = events_df,
    aes(x = period_start + 3, y = max_y_p2 * 0.8, label = event_label),
    angle = 0, hjust = 0, vjust = 0, size = 3.5, color = "black", inherit.aes = FALSE
  )

plot_05.08_p1 <- ggplot(weekly_long_p1, aes(x = period_start, y = Value, color = Indicator, group = Indicator)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    minor_breaks = seq(0, 100, by = 5)
  ) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_viridis_d(option = "F", begin = 0.2, end = 0.8) +
  labs(x = "Time (Weeks)", y = "Percentage (%)", color = "Indicator") +
  theme_light() +
  theme(
    plot.title   = element_text(size = 11),
    axis.title   = element_text(size = 10),
    axis.text    = element_text(size = 9),
    legend.title = element_blank(),
    legend.text  = element_text(size = 9),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

aligned_plots <- align_plots(plot_05.08_p2, plot_05.08_p1, align = "v", axis = "lr")

plot_05.08 <- ggdraw() +
  draw_plot(aligned_plots[[1]], 0, 0.7, 1, 0.3) +
  draw_plot(aligned_plots[[2]], 0, 0,   1, 0.7)

# Display
plot_05.08

# Save
current_date   <- format(Sys.Date(), "%Y-%m-%d")
output_dir     <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename<- paste0("plot_05.08_", current_date, ".png")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(filename = file.path(output_dir, output_filename),
       plot = plot_05.08, width = 8, height = 6, dpi = 300)
