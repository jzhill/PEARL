# TPT cascade (all patients) with an extra bar: "4+ months since starting"
library(tidyverse)
library(lubridate)
library(here)

four_months_ago <- Sys.Date() - weeks(16)  # ≈ 4 months

# Screening-derived stages (include all)
tpt_cascade_sd <- screening_data %>%
  summarise(
    `TST Positive` = sum(tst_read_positive == "Positive TST", na.rm = TRUE),
    `TST Positive, TB ruled out` = sum(
      (tst_read_positive == "Positive TST") &
        (tb_decision == "Ruled out TB" | ntp_diagnosis == "Ruled out"),
      na.rm = TRUE
    ),
    `Completed TPT Assessment` = sum(
      (prerx_eligible %in% c("Yes","No")) |
        (exit_reason_screen == "TPT - withdraw consent and prefer not to continue"),
      na.rm = TRUE
    ),
    `Eligible for TPT` = sum(prerx_eligible == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "stage", values_to = "count")

# Treatment-derived stages (all starts + 4+ months since start)
tpt_cascade_td <- treatment_data %>%
  summarise(
    `Started TPT` = sum(!is.na(tpt_start_date), na.rm = TRUE),
    `4+ months since starting` = sum(!is.na(tpt_start_date) & tpt_start_date <= four_months_ago, na.rm = TRUE),
    `Treatment Outcome Assigned` = sum(!is.na(tpt_outcome_reason), na.rm = TRUE),
    `Completed TPT` = sum(tpt_outcome_reason == "Completed", na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "stage", values_to = "count")

# Combine & order
tpt_cascade <- bind_rows(tpt_cascade_sd, tpt_cascade_td) %>%
  mutate(stage = factor(stage, levels = c(
    "TST Positive",
    "TST Positive, TB ruled out",
    "Completed TPT Assessment",
    "Eligible for TPT",
    "Started TPT",
    "4+ months since starting",
    "Treatment Outcome Assigned",
    "Completed TPT"
  )))

cascade_labels <- c(
  "TST Positive"                    = "TST+",
  "TST Positive, TB ruled out"      = "TST+\nTB ruled out",
  "Completed TPT Assessment"        = "Assessment\ncompleted",
  "Eligible for TPT"                = "Eligible\nfor TPT",
  "Started TPT"                     = "Started\nTPT",
  "4+ months since starting"        = "≥4 months\nsince start",
  "Treatment Outcome Assigned"      = "Outcome\nassigned",
  "Completed TPT"                   = "Completed\nTPT"
)

plot_05.12 <- ggplot(tpt_cascade, aes(x = stage, y = count, fill = stage)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::comma(count)), vjust = -0.35, size = 5, fontface = "bold") +
  scale_x_discrete(labels = cascade_labels, drop = FALSE) +
  scale_y_continuous(name = "Number of individuals", expand = expansion(mult = c(0.02, 0.10))) +
  scale_fill_viridis_d() +
  labs(title = "TST-Positive Treatment Cascade (All patients)", x = NULL) +
  theme_light() +
  theme(
    axis.text.x  = element_text(size = 9, lineheight = 0.95),
    axis.ticks.x = element_line(),
    legend.position = "none"
  )

plot_05.12

# Save
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, paste0("plot_05.12_", current_date, ".png")),
       plot_05.12, width = 8, height = 4, dpi = 300)
