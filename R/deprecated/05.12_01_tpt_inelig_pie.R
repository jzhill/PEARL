# Pie chart: Reasons for TPT ineligibility (all patients)
library(tidyverse)
library(here)

# Build reasons for ineligibility (all time)
tpt_ineligible <- screening_data %>%
  filter(prerx_eligible == "No" | ntp_diagnosis == "Confirmed") %>%
  mutate(
    reason = case_when(
      ntp_diagnosis == "Confirmed" ~ "Confirmed TB",
      prerx_tbsx == TRUE | (tb_decision == "Presumptive TB" & ntp_diagnosis != "Ruled out") ~ "Presumed TB",
      prerx_tptchoice == FALSE ~ "Refused",
      exit_reason_screen == "TPT - withdraw consent and prefer not to continue" ~ "Refused",
      prerx_tb12m == TRUE ~ "Treated <12 months",
      prerx_allergy == TRUE ~ "Reported allergy",
      prerx_preg == TRUE ~ "Pregnant",
      prerx_riskcat == "High" ~ "High-risk DILI",
      TRUE ~ "Other/Unknown"
    )
  ) %>%
  count(reason, name = "n") %>%
  arrange(desc(n))

# If no rows, avoid an empty pie error
if (nrow(tpt_ineligible) == 0) {
  tpt_ineligible <- tibble(reason = "No data", n = 1L)
}

plot_05.12_01 <- ggplot(tpt_ineligible, aes(x = "", y = n, fill = reason)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Reasons for TPT Ineligibility (All patients)",
    fill = "Reason"
  ) +
  scale_fill_viridis_d() +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5),
            size = 5, fontface = "bold", colour = "grey10")

plot_05.12_01

# Save
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, paste0("plot_05.12_01_", current_date, ".png")),
       plot_05.12_01, width = 8, height = 4, dpi = 300)
