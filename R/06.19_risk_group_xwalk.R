# 06.19_TPT_risk_assessment_crosswalk.R
# Cross-tab by risk group (rows) with key TPT assessment actions (columns)
# Population: all who should be assessed (TST+ and TB ruled out)

library(tidyverse)
library(flextable)
library(here)

# ---------- Filter to "should be assessed" ----------
sd_assess <- screening_data %>%
  filter(
    tst_read_positive == "Positive TST",
    tb_decision == "Ruled out TB" | ntp_diagnosis == "Ruled out"
  ) %>%
  mutate(
    # robust risk_cat with explicit "(Missing)"
    risk_cat = fct_explicit_na(as.factor(prerx_riskcat), na_level = "(Missing)") |> as.character(),
    # ALT available logic: treat NA as FALSE; accept either explicit alt result TRUE or a non-missing calc
    alt_available = if ("tptrf_alt_result" %in% names(.)) {
      coalesce(tptrf_alt_result, FALSE) | !is.na(calc_alt_last)
    } else {
      !is.na(calc_alt_last)
    },
    eligible     = prerx_eligible == "Yes",
    not_eligible = prerx_eligible == "No",
    started      = coalesce(prerx_start, FALSE),
    # Treat NA as "not started" per requirement
    not_started  = !coalesce(prerx_start, FALSE)
  )

# Optional: friendly row order if present
risk_levels <- c("High", "Moderate high", "Moderate", "Low", "Not yet known", "(Missing)")
sd_assess <- sd_assess %>%
  mutate(risk_cat = factor(risk_cat, levels = intersect(risk_levels, unique(c(risk_levels, risk_cat)))))

# ---------- Summarise to wide table ----------
tbl_06_19 <- sd_assess %>%
  group_by(risk_cat) %>%
  summarise(
    `Baseline ALT result available`     = sum(alt_available, na.rm = TRUE),
    `Baseline ALT result not available` = sum(!alt_available, na.rm = TRUE),
    `Recorded eligible for TPT`         = sum(eligible, na.rm = TRUE),
    `Recorded not eligible`             = sum(not_eligible, na.rm = TRUE),
    `Started TPT`                       = sum(started, na.rm = TRUE),
    `Not started`                       = sum(not_started, na.rm = TRUE),
    Total                               = dplyr::n(),
    .groups = "drop"
  )

# ---------- All (n / %) ----------
# Use a single scalar grand total and compute % per row from it
grand_total <- sum(tbl_06_19$Total, na.rm = TRUE)

tbl_06_19 <- tbl_06_19 %>%
  mutate(
    All_n   = Total,
    All_pct = if (grand_total > 0) round(100 * Total / grand_total, 1) else NA_real_
  )

# ---------- Grand-total row ----------
# IMPORTANT: don't sum the percent column; recompute it explicitly as 100
grand_row <- tbl_06_19 %>%
  select(-All_pct) %>%                                   # <-- prevents accidental sum of % column
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(risk_cat = "Total", All_pct = 100) %>%
  relocate(risk_cat, .before = 1)

tbl_06_19 <- bind_rows(tbl_06_19, grand_row)

# ---------- Prepare for flextable (add skinny spacer columns) ----------
tbl_06_19_ft <- tbl_06_19 %>%
  rename(`Risk group` = risk_cat) %>%
  mutate(
    ALT_spacer    = "",   # spacer columns are character
    Assess_spacer = "",
    Status_spacer = ""
  ) %>%
  select(
    `Risk group`,
    `Baseline ALT result available`, `Baseline ALT result not available`, ALT_spacer,
    `Recorded eligible for TPT`, `Recorded not eligible`, Assess_spacer,
    `Started TPT`, `Not started`, Status_spacer,
    All_n, All_pct
  )

num_cols_int <- c(
  "Baseline ALT result available",
  "Baseline ALT result not available",
  "Recorded eligible for TPT",
  "Recorded not eligible",
  "Started TPT",
  "Not started",
  "All_n"
)
num_cols_pct <- "All_pct"

# ---------- Flextable ----------
table_06.19 <- tbl_06_19_ft %>%
  flextable() %>%
  # top grouped header (include spacers in colwidths)
  add_header_row(
    values    = c("", "ALT result", "", "TPT assessment", "", "TPT status", "", "All"),
    colwidths = c(1, 2, 1, 2, 1, 2, 1, 2)
  ) %>%
  # subheader labels
  set_header_labels(
    `Risk group`                        = "Risk group",
    `Baseline ALT result available`     = "Available",
    `Baseline ALT result not available` = "Not available",
    ALT_spacer                          = "",
    `Recorded eligible for TPT`         = "Eligible",
    `Recorded not eligible`             = "Not eligible",
    Assess_spacer                       = "",
    `Started TPT`                       = "Started",
    `Not started`                       = "Not started",
    Status_spacer                       = "",
    All_n                               = "n",
    All_pct                             = "%"
  ) %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%                 # ensure visible in dark mode
  bold(part = "header") %>%
  align(j = c(num_cols_int, num_cols_pct), align = "center", part = "all") %>%
  colformat_num(j = num_cols_int, digits = 0, big.mark = ",") %>%
  colformat_num(j = num_cols_pct, digits = 1, suffix = "%") %>%
  bold(i = ~ `Risk group` == "Total") %>%
  # make spacers skinny and "invisible"
  width(j = c("ALT_spacer","Assess_spacer","Status_spacer"), width = 0.12) %>%
  # a touch of padding to breathe
  padding(padding.left = 4, padding.right = 4, part = "all") %>%
  autofit()

table_06.19

# ---------- Save ----------
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir   <- file.path(here("figures"), paste0("Outputs_", current_date))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file  <- file.path(output_dir, paste0("table_06.19_", current_date, ".png"))
save_as_image(table_06.19, path = output_file)
