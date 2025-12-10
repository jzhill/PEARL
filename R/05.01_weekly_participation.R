library(ggplot2)
library(dplyr)
library(here)

source(here("R/analysis_data.R"))

plot_weekly_participation <- function(analysis_data) {
  weekly_data <- analysis_data$weekly_data

  ggplot(weekly_data, aes(x = period_start)) +
    geom_line(aes(y = hh_enum_new,   color = "Households Enumerated"), size = 0.8) +
    geom_point(aes(y = hh_enum_new,  color = "Households Enumerated"), size = 1.5, na.rm = TRUE) +
    geom_line(aes(y = reg,       color = "People Registered"),     size = 0.8) +
    geom_point(aes(y = reg,      color = "People Registered"),     size = 1.5, na.rm = TRUE) +
    geom_line(aes(y = tpt_start, color = "People Started on TPT"), size = 0.8) +
    geom_point(aes(y = tpt_start,color = "People Started on TPT"), size = 1.5, na.rm = TRUE) +
    scale_color_viridis_d(option = "F", begin = 0.2, end = 0.8) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      title = "Weekly activity: households enumerated, people registered, people commenced on TPT",
      x = "Week",
      y = "Count",
      color = "Indicator"
    ) +
    theme_light() +
    theme(
      plot.title   = element_text(size = 10),
      axis.title   = element_text(size = 10),
      axis.text    = element_text(size = 9),
      axis.text.x  = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      legend.text  = element_text(size = 9),
      legend.position = "bottom"
    )
}

save_weekly_participation <- function(analysis_data = get_analysis_data(), output_dir = NULL) {
  version_date <- if (!is.null(analysis_data$current_date)) analysis_data$current_date else format(Sys.Date(), "%Y-%m-%d")
  if (is.null(output_dir)) {
    output_dir <- file.path(here("figures"), paste0("Outputs_", version_date))
  }
  output_filename <- paste0("plot_05.01_", version_date, ".png")

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  plot_obj <- plot_weekly_participation(analysis_data)
  ggsave(filename = file.path(output_dir, output_filename),
         plot = plot_obj, width = 8, height = 4, dpi = 300)

  invisible(file.path(output_dir, output_filename))
}

if (interactive()) {
  analysis_data <- get_analysis_data(build_if_missing = TRUE)
  save_weekly_participation(analysis_data)
}
