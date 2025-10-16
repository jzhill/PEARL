library(ggplot2)
library(cowplot)
library(dplyr)
library(scales)
library(here)

# Events (keyed to period_start)
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

# Pretty names for the five follow-up quality indicators (monthly)
indicator_labels_followup <- c(
  "ntp_out_pct"                 = "NTP Outcome Recorded",
  "nlp_out_pct"                 = "NLP Outcome Recorded",
  "tpt_assessed_of_should_pct"  = "TPT Assessed / Should Assess",
  "tpt_started_of_eligible_pct" = "TPT Started / Eligible",
  "tpt_eligible_of_started_pct" = "Eligible among Started"
)

monthly_followup <- monthly_long %>%
  filter(Indicator %in% names(indicator_labels_followup)) %>%
  mutate(Indicator = recode(Indicator, !!!indicator_labels_followup))

# Headroom for >100% if it happens (don’t clamp to 100)
y_max <- suppressWarnings(max(monthly_followup$Value, na.rm = TRUE))
y_cap <- if (is.finite(y_max)) ceiling(y_max / 10) * 10 else 100

plot_05.18 <- ggplot(
  monthly_followup,
  aes(x = period_start, y = Value, color = Indicator, group = Indicator)
) +
  geom_line(size = 0.9) +
  geom_point(size = 1.8, na.rm = TRUE) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, y_cap),
    breaks = seq(0, y_cap, by = 10),
    sec.axis = sec_axis(
      ~ .,
      labels = function(x) paste0(x, "%"),
      breaks = seq(0, y_cap, by = 10),
      name = NULL
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_viridis_d(option = "F", begin = 0.2, end = 0.8) +
  labs(
    title = "Monthly follow-up quality indicators",
    x = "Month",
    y = "Percent",
    color = "Indicator"
  ) +
  theme_light() +
  theme(
    plot.title   = element_text(size = 11),
    axis.title   = element_text(size = 10),
    axis.text    = element_text(size = 9),
    legend.title = element_blank(),
    legend.text  = element_text(size = 9),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    axis.ticks.y.right = element_line()
  ) +
  # Events
  geom_vline(
    data = events_df,
    aes(xintercept = as.numeric(period_start)),
    linetype = "dashed", color = "grey"
  ) +
  geom_text(
    data = events_df,
    aes(x = period_start + 3, y = y_cap * 0.95, label = event_label),
    angle = 0, hjust = 0, vjust = 0, size = 3.3, color = "black",
    inherit.aes = FALSE
  )

# Display
plot_05.18

# Save
current_date    <- format(Sys.Date(), "%Y-%m-%d")
output_dir      <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.18_", current_date, ".png")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(output_dir, output_filename),
  plot = plot_05.18,
  width = 8, height = 4.8, dpi = 300
)
