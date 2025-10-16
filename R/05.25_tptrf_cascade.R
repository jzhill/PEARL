# 05.25_tptrf_cascade.R
# Cascade of TPT risk assessment steps using indicators in screening_data
# Steps: TST+ TB ruled out → RFs assessed → ALT needed → ALT requested → ALT resulted → Risk group assigned → Risk matches expected

library(tidyverse)
library(here)

# ---- Counts ---------------------------------------------------------------
tptrf_cascade <- screening_data %>%
  summarise(
    `TST Positive, TB ruled out` = sum(
      (tst_read_positive == "Positive TST") &
        (tb_decision == "Ruled out TB" | ntp_diagnosis == "Ruled out"),
      na.rm = TRUE
    ),
    `Risk factors assessed`    = sum(tptrf_assessed,        na.rm = TRUE),
    `Baseline ALT needed`      = sum(tptrf_alt_needed,      na.rm = TRUE),
    `Baseline ALT requested`   = sum(tptrf_alt_requested,   na.rm = TRUE),
    `Baseline ALT resulted`    = sum(tptrf_alt_result,    na.rm = TRUE),
    `Risk group assigned`      = sum(tptrf_risk_assigned,   na.rm = TRUE),
    `Risk matches expected`    = sum(tptrf_risk_as_expected, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "stage", values_to = "count") %>%
  mutate(
    stage = factor(stage, levels = c(
      "TST Positive, TB ruled out",
      "Risk factors assessed",
      "Baseline ALT needed",
      "Baseline ALT requested",
      "Baseline ALT resulted",
      "Risk group assigned",
      "Risk matches expected"
    ))
  )

# Short multi-line labels for x-axis
cascade_labels <- c(
  "TST Positive, TB ruled out" = "TST+\nTB ruled out",
  "Risk factors assessed"      = "Risk factors\nassessed",
  "Baseline ALT needed"        = "Baseline ALT\nneeded",
  "Baseline ALT requested"     = "Baseline ALT\nrequested",
  "Baseline ALT resulted"      = "Baseline ALT\nresulted",
  "Risk group assigned"        = "Risk group\nassigned",
  "Risk matches expected"      = "Risk matches\nexpected"
)

# ---- Plot ---------------------------------------------------------------
plot_05.25 <- ggplot(tptrf_cascade, aes(x = stage, y = count, fill = stage)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::comma(count)), vjust = -0.35, size = 5, fontface = "bold") +
  scale_x_discrete(labels = cascade_labels, drop = FALSE) +
  scale_y_continuous(name = "Number of individuals",
                     expand = expansion(mult = c(0.02, 0.10))) +
  scale_fill_viridis_d() +
  labs(
    title = "TPT Risk Assessment Cascade (All patients)",
    x = NULL
  ) +
  theme_light() +
  theme(
    axis.text.x  = element_text(size = 9, lineheight = 0.95),
    axis.ticks.x = element_line(),
    legend.position = "none"
  )

# Display
plot_05.25

# ---- Save ---------------------------------------------------------------
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(output_dir, paste0("plot_05.25_", current_date, ".png")),
  plot = plot_05.25,
  width = 8, height = 4, dpi = 300
)
