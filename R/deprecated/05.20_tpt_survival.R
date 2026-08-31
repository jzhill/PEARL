# Retention curve among those who have an outcome ---------------------------------

library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(scales)
library(here)

# Parameters
max_day <- 168  # plot up to 24 weeks

# Build the pooled cohort: include anyone with a valid start date
td_cohort <- treatment_data %>%
  filter(!is.na(tpt_start_date) & !is.na(tpt_dur_real)) %>%
  transmute(record_id,
            dur = as.integer(tpt_dur_real)) %>%
  # guardrails: any negative/NA duration -> treat as 0 at most (still counts at day 0)
  mutate(dur = ifelse(is.na(dur) | dur < 0, 0L, dur))

cohort_n <- nrow(td_cohort)

# If there are no valid starts, make a blank plot-friendly tibble
retention <- if (cohort_n == 0) {
  tibble(day = 0:max_day, on_n = 0L, pct_on = NA_real_)
} else {
  tibble(day = 0:max_day) %>%
    mutate(
      on_n  = map_int(day, ~ sum(td_cohort$dur >= .x)),
      pct_on = 100 * on_n / cohort_n
    )
}

# Plot: step function, starts at 100% on day 0, drops as participants finish
plot_05.20 <- ggplot(retention, aes(x = day, y = pct_on)) +
  geom_step(linewidth = 1) +
  geom_point(data = subset(retention, day %% 7 == 0), size = 1.6) +  # weekly dots (optional)
  scale_y_continuous(
    limits = c(0, 100),
    breaks  = seq(0, 100, 10),
    labels  = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    limits = c(0, max_day),
    breaks = seq(0, max_day, 14),    # every 2 weeks
    minor_breaks = seq(0, max_day, 7)
  ) +
  labs(
    title = "Treatment retention over time (all starts pooled)",
    subtitle = paste0("Cohort size N = ", cohort_n, "; day 0 = start of TPT"),
    x = "Days since TPT start",
    y = "On treatment (%)"
  ) +
  theme_light() +
  theme(
    plot.title   = element_text(size = 11),
    axis.title   = element_text(size = 10),
    axis.text    = element_text(size = 9)
  )

# Display
plot_05.20

# Save
current_date    <- format(Sys.Date(), "%Y-%m-%d")
output_dir      <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("plot_05.20_retention_", current_date, ".png")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(output_dir, output_filename),
  plot = plot_05.20,
  width = 8, height = 4.2, dpi = 300
)
