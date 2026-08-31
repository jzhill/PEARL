# Pie chart: Reasons for not completing TPT assessment (all patients)
library(tidyverse)
library(here)

# TST+ and TB ruled out but assessment not completed
tpt_not_assessed <- screening_data %>%
  filter(
    tst_read_positive == "Positive TST",
    tb_decision == "Ruled out TB" | ntp_diagnosis == "Ruled out",
    # not completed assessment:
    calc_tpt != "09 Started",
    exit_reason_screen != "TPT - withdraw consent and prefer not to continue",
    !(prerx_eligible %in% c("Yes","No"))
  ) %>%
  mutate(
    reason = case_when(
      exit_reason_screen %in% c("TPT - lost to follow-up", "Screening - lost to follow-up") ~ "LTFU",
      exit_reason_screen == "Died" ~ "Died",
      calc_tpt %in% c("02 TPT decision", "03 Test HBV", "04 Collect blood", "07 ALT done") ~ "Assessment in progress",
      calc_tpt == "01 Assess TPT" ~ "Assessment not started",
      TRUE ~ coalesce(calc_tpt, "Other/Unknown")
    )
  ) %>%
  count(reason, name = "n") %>%
  arrange(desc(n))

# Handle empty case to avoid a blank plot
if (nrow(tpt_not_assessed) == 0) {
  tpt_not_assessed <- tibble(reason = "No data", n = 1L)
}

plot_05.12_02 <- ggplot(tpt_not_assessed, aes(x = "", y = n, fill = reason)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Reasons for Not Completing TPT Assessment (All patients)",
    fill = "Reason"
  ) +
  scale_fill_viridis_d() +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5),
            size = 5, fontface = "bold", colour = "grey10")

plot_05.12_02

# Save
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, paste0("plot_05.12_02_", current_date, ".png")),
       plot_05.12_02, width = 8, height = 4, dpi = 300)
