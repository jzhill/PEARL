# 05_output_functions.R
# Contains all outputs as functions for use in other scripts and outputs
# Refactored from separate output scripts
# Author: Jeremy + Gemini
# Created: 2/3/26


library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(flextable)
library(officer)
library(apyramid)
library(cowplot)
library(scales)
library(sf)

# --- MAINTENANCE RULES FOR AI AGENTS ---------------------
# 1. PRAGMATIC PARAMETERS: All output functions should support scaling.
#    - Plots: Use 'base_size' (default 11) and pass to theme calls.
#    - Tables: Use 'font_size' (default 9) and 'table_width' (optional numeric in inches).
# 2. ZERO DRIFT: Never change the default styling or column logic of an existing function.
# 3. NO INTERNAL FILTERING: Assume data is pre-filtered before being passed to the function. 
#    - Exception: Mandatory time-scoping parameters (start_date/end_date) should remain.



# --- DATA INFRASTRUCTURE ------------------------------------------------------

#' Internal helper to apply the PEARL standardized styling to flextables
theme_pearl <- function(x) {
  x %>%
    flextable::theme_vanilla() %>%
    flextable::font(part = "all", fontname = "Arial") %>%
    flextable::bg(part = "header", bg = "#F2F2F2") %>%
    flextable::bg(part = "body", bg = "white") %>%
    flextable::bold(part = "header") %>%
    flextable::color(part = "all", color = "black") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::border(part = "all", border = officer::fp_border(color = "black", width = 0.5)) %>%
    flextable::border_outer(border = officer::fp_border(color = "black", width = 1.2)) %>%
    flextable::set_table_properties(align = "center", layout = "fixed")
}

#' Load and unpack the most recent tidy data file
#' @param path String. Path to the folder containing .qs files
load_latest_tidy_data <- function(path = "data-processed/tidy-data-bundles") {
  files <- list.files(here::here(path), pattern = "\\.qs$", full.names = TRUE)
  if (length(files) == 0) stop("No .qs files found in ", path)
  
  latest_file <- sort(files, decreasing = TRUE)[1]
  message("Loading latest data from: ", latest_file)
  
  data <- qs::qread(latest_file)
  list2env(data, envir = .GlobalEnv)
  message("Successfully unpacked ", length(data), " objects into the Environment.")
}



# --- ACTIVITY PLOTS -----------------------------------------------------------

#' Plot weekly activity: Households, Registrations, and TPT starts
#' @param data Dataframe. Defaults to weekly_data from the environment
out_plot_weekly_activity <- function(data = weekly_data) {
  ggplot(data, aes(x = period_start)) +
    geom_line(aes(y = hh_enum_new,   color = "Households Enumerated"), linewidth = 0.8) +
    geom_point(aes(y = hh_enum_new,  color = "Households Enumerated"), size = 1.5, na.rm = TRUE) +
    geom_line(aes(y = reg,       color = "People Registered"),     linewidth = 0.8) +
    geom_point(aes(y = reg,      color = "People Registered"),     size = 1.5, na.rm = TRUE) +
    geom_line(aes(y = tpt_start, color = "People Started on TPT"), linewidth = 0.8) +
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
      plot.title    = element_text(size = 10),
      axis.title    = element_text(size = 10),
      axis.text     = element_text(size = 9),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      legend.title  = element_blank(),
      legend.text   = element_text(size = 9),
      legend.position = "bottom"
    )
}


# --- DEMOGRAPHIC PLOTS --------------------------------------------------------

#' Plot age pyramid of PEARL participants
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_age_pyramid <- function(data = screening_data) {
  
  # Data manipulation included inside for encapsulation
  plot_data <- data %>%
    filter(en_sex %in% c("M", "F"), !is.na(age_cat)) %>% 
    mutate(en_sex = factor(en_sex, levels = c("M", "F")))
  
  # Construct output
  age_pyramid(plot_data, age_group = "age_cat", split_by = "en_sex") +
    scale_fill_viridis_d(option = "F", begin = 0.4, end = 0.6) +
    labs(
      title = "PEARL participant population pyramid",
      x = "Count",
      y = "Age category"
    ) +
    theme_light() +
    theme(legend.title = element_blank())
}



# --- GEOGRAPHIC PLOTS ---------------------------------------------------------

#' Plot registration coverage by Enumeration Area (EA)
#' @param data Dataframe. Defaults to ea_data from the environment
out_plot_ea_coverage <- function(data = ea_data) {
  
  # Data manipulation
  plot_data <- data %>%
    filter(pop_elig_new > 50) %>%  
    mutate(record_id = fct_reorder(record_id, date_enum_new, .desc = FALSE))
  
  # Calculate y-axis limit
  max_y <- max(plot_data$prop_reg_screen_hh, na.rm = TRUE)
  
  if (!is.finite(max_y)) {
    max_y <- 1
  }
  
  # Construct output
  ggplot(
    plot_data,
    aes(x = record_id, y = prop_reg_screen_hh, fill = village)
  ) +
    geom_bar(stat = "identity") +
    labs(
      x     = "EA",
      y     = "Percentage registered (screened / eligible)",
      title = "Coverage (registered/eligible) by EA",
      fill  = "Village"
    ) +
    scale_y_continuous(
      limits = c(0, max_y),
      breaks = seq(0, max_y, by = 0.1),
      minor_breaks = seq(0, max_y, by = 0.05),
      labels = scales::percent_format(accuracy = 1), # Refactored to percentage
      sec.axis = dup_axis(name = NULL, labels = scales::percent_format(accuracy = 1))
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
}



#' Plot cumulative registration coverage by village over time
#' @param data Dataframe. Defaults to village_data_cum from the environment
out_plot_village_cumulative_coverage <- function(data = village_data_cum) {
  
  # Data manipulation for clean time-series visualization
  plot_data <- data %>%
    # Drop the aggregate "Other or unknown" bucket
    filter(village != "Other or unknown") %>%
    # Keep villages with >= 100 registrations
    filter(!is.na(reg), reg >= 100) %>%
    # Drop any rows before that village actually started to avoid long flat starts
    filter(week_reg >= first_screen_date)
  
  # Construct output
  ggplot(
    plot_data,
    aes(x = week_reg, y = cum_prop_2023_ex02, color = village)
  ) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1) +
    labs(
      x = "Registration week",
      y = "Cumulative percentage screened",
      title = "Cumulative proportion of village population screened over time",
      subtitle = "Denominator: 2023 census population, excluding 0–2 years",
      color = "Village"
    ) +
    scale_x_date(
      breaks = seq(from = min_month, to = max_week, by = "month"),
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      minor_breaks = seq(0, 1, by = 0.05),
      labels = scales::percent_format(accuracy = 1) # Applied requested refactor
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}



#' Plot cumulative stacked area of screening progress by village
#' @param data Dataframe. Defaults to village_data_cum from the environment
#' @param max_y Numeric. The upper limit for the Y axis (default 25000)
out_plot_village_cumulative_screening <- function(data = village_data_cum, max_y = 25000) {
  
  # Note: The script uses 'cum_screened'. If this was intended to be 'cum_reg' 
  # from the tidy bundle, ensure the mapping matches.
  
  ggplot(data, aes(x = week_reg, y = cum_screened, fill = village)) +
    geom_area() +
    labs(
      x = "Registration Week",
      y = "Cumulative Number Screened",
      title = "Cumulative Screening by Village",
      fill = "Village"
    ) +
    scale_x_date(
      breaks = seq(from = min_month, to = max_week, by = "month"),
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, max_y),
      breaks = seq(0, max_y, by = 1000),
      labels = scales::label_comma() # Refactor: Adds commas to large numbers
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}



#' Plot map of screening counts by Enumeration Area (EA) in Betio
#' @param data sf object. Defaults to layer_betio_ea_3832 from the environment
out_plot_betio_screening_map <- function(data = layer_betio_ea_3832) {
  
  # Data manipulation: 1:1 match of the provided logic
  # Note: fill_category is created here but the plot below uses the continuous joined_reg
  plot_data <- data %>%
    mutate(fill_category = ifelse(joined_reg < 100, "Below 100", "Above 100"))
  
  # Construct output
  ggplot(plot_data) +
    geom_sf(aes(fill = joined_reg), color = "black", linewidth = 0.2) +
    scale_fill_gradient(
      low = "lightgrey", 
      high = "darkred", 
      na.value = "lightgrey",
      labels = scales::label_comma() # Refactor for readability
    ) +
    labs(
      title = "Screening Counts by Enumeration Area (VID 716)",
      fill = "Count"
    ) +
    theme_light() +
    theme(
      panel.grid = element_blank(), # Cleaner for maps
      axis.text = element_blank(),  # Coordinates usually aren't needed for EA maps
      axis.ticks = element_blank()
    )
}



#' Plot map of screening coverage (proportion) by Enumeration Area (EA) in Betio
#' @param data sf object. Defaults to layer_betio_ea_3832 from the environment
out_plot_betio_coverage_map <- function(data = layer_betio_ea_3832) {
  
  # Data manipulation
  plot_data <- data %>%
    # Create category for possible discrete filtering/analysis
    mutate(fill_category_prop_reg = ifelse(joined_reg < 0.2, "Below 20pc", "Above 20pc")) %>%
    # Create labels with EA ID and proportion (useful for geom_sf_text if needed)
    mutate(label_text = paste0(ea_2020, "\n", round(joined_reg * 100, 1), "%"))
  
  # Construct output
  ggplot(plot_data) +
    geom_sf(aes(fill = joined_reg), color = "black", linewidth = 0.2) +
    scale_fill_gradient(
      low = "lightgrey", 
      high = "darkgreen", 
      na.value = "lightgrey",
      labels = scales::label_percent(accuracy = 1) # Refactor: Display 0.2 as 20%
    ) +
    labs(
      title = "Screening Proportion by Enumeration Area (VID 716)",
      fill = "Coverage (%)"
    ) +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}



#' Plot cumulative screening coverage using project-defined eligible population
#' @param data Dataframe. Defaults to village_data_cum from the environment
#' @param v_data Dataframe. Defaults to village_data from the environment
out_plot_village_cumulative_eligible_coverage <- function(data = village_data_cum, 
                                                          v_data = village_data) {
  
  # 1. Internal Logic: Identify villages with >= 100 registrations dynamically
  # This replaces the missing 'villages_gte_100' global object
  active_villages <- v_data %>%
    dplyr::filter(reg >= 100) %>%
    dplyr::pull(village)
  
  # 2. Data manipulation
  plot_data <- data %>%
    dplyr::filter(village %in% active_villages) %>%
    # Ensure we use the most reliable first_screen_date by joining from v_data
    dplyr::select(-any_of("first_screen_date")) %>%
    dplyr::left_join(v_data %>% dplyr::select(village, first_screen_date), by = "village") %>%
    dplyr::filter(week_reg >= first_screen_date)
  
  # Define axis ranges dynamically
  min_date <- lubridate::floor_date(min(plot_data$week_reg, na.rm = TRUE), unit = "month") - months(1)
  max_date <- lubridate::floor_date(max(plot_data$week_reg, na.rm = TRUE), unit = "month") + months(1)
  
  # 3. Construct Output
  ggplot(plot_data, aes(x = week_reg, y = cum_prop_elig_new, color = village)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      x = "Registration Week",
      y = "Cumulative Percentage Screened",
      title = "Cumulative Proportion of Village Population Screened Over Time",
      subtitle = "Denominator: project-defined eligible population",
      color = "Village"
    ) +
    scale_x_date(
      breaks = seq(from = min_date, to = max_date, by = "month"),
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      minor_breaks = seq(0, 1, by = 0.05),
      labels = scales::percent_format(accuracy = 1)
    ) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}



#' Plot point map of households over EA boundaries in Betio
#' @param ea_layer sf object. Defaults to layer_betio_ea_3832
#' @param hh_layer sf object. Defaults to layer_hh_betio_3832
out_plot_betio_household_points <- function(ea_layer = layer_betio_ea_3832, 
                                            hh_layer = layer_hh_betio_3832) {
  
  ggplot() +
    # Draw EA boundaries as the base
    geom_sf(data = ea_layer, fill = "white", color = "black") +
    # Overlay household points colored by status
    geom_sf(data = hh_layer, aes(color = hh_status), size = 1, alpha = 0.8) +
    scale_color_viridis_d(option = "D", name = "Household Status") +
    labs(
      title = "Household Locations and Status: Betio",
      subtitle = "Points represent individual dwellings over EA boundaries"
    ) +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

# TODO
# Adding a Basemap or Context: While the EA boundaries give structure, it can be 
# difficult for field teams to orient themselves without geographic context. 
# Adding a "Project Office" point (which you already defined in Script 03) or 
# using a satellite tileset (via a package like ggspatial) can significantly 
# improve the map's utility for staff.





# --- PERFORMANCE & CASCADE PLOTS ----------------------------------------------




#' Plot core weekly quality indicators (Matches Core Table)
#' @param data Dataframe. Defaults to weekly_long
#' @param end_date Date. Optional; defaults to max date in data.
#' @param start_date Date. Optional; defaults to 12 weeks prior to end_date.
#' @param date_breaks Character. Spacing for the x-axis ticks (default "1 week").
#' @param base_size Numeric. Base font size for the plot (default 11).
out_plot_weekly_quality <- function(data = weekly_long, 
                                    end_date = NULL, 
                                    start_date = NULL,
                                    date_breaks = "1 week",
                                    base_size = 11) {
  
  # 1. Date Handling
  if (is.null(end_date)) {
    end_date <- max(data$period_start, na.rm = TRUE)
  }
  if (is.null(start_date)) {
    start_date <- end_date - lubridate::weeks(12)
  }
  
  # Smart date labels based on the requested breaks
  date_labels <- if (grepl("week", date_breaks)) "%d %b" else "%b %Y"
  
  # 2. Events (Filtered to the visible date window)
  events_df <- tibble::tibble(
    period_start = as.Date(c("2023-03-27", "2023-08-07", "2023-10-30", 
                             "2024-04-08", "2024-10-14", "2025-01-13")),
    event_label  = c("Commence in Temakin,\nstockout of TPT meds", "Lab tech start", 
                     "Stockout of TB meds,\nreplenishment of TPT", 
                     "Recommence screening\nin Temakin", "Xpert stockout", "New staff")
  ) %>%
    dplyr::filter(period_start >= start_date & period_start <= end_date)
  
  # 3. Data Dictionary & Extraction
  dict <- get_all_indicators_dict() %>% 
    dplyr::filter(is_core == TRUE)
  
  pct_keys <- dict %>% 
    dplyr::filter(grepl("\\(\\%\\)", Indicator_Name)) %>% 
    dplyr::pull(Indicator_Key)
  
  count_keys <- dict %>% 
    dplyr::filter(!grepl("\\(\\%\\)", Indicator_Name)) %>% 
    dplyr::pull(Indicator_Key)
  
  plot_data <- data %>%
    dplyr::filter(period_start >= start_date & period_start <= end_date) %>%
    dplyr::filter(Indicator %in% dict$Indicator_Key) %>%
    dplyr::left_join(dict, by = c("Indicator" = "Indicator_Key"))
  
  p1_data <- plot_data %>% dplyr::filter(Indicator %in% pct_keys)
  p2_data <- plot_data %>% dplyr::filter(Indicator %in% count_keys)
  
  max_y_p2 <- max(p2_data$Value, na.rm = TRUE) * 1.25
  if (!is.finite(max_y_p2) || max_y_p2 == 0) max_y_p2 <- 100
  
  # 4. Top Plot (Counts - Thin panel, no legend)
  plot_top <- ggplot(p2_data, aes(x = period_start, y = Value, color = Indicator_Name, group = Indicator_Name)) +
    geom_line(linewidth = 0.7) +
    scale_y_continuous(
      name = "Activity count", 
      labels = scales::label_comma(), 
      limits = c(0, max_y_p2)
    ) + 
    scale_x_date(
      limits = c(start_date, end_date), 
      date_breaks = date_breaks, 
      date_labels = date_labels
    ) + 
    scale_color_manual(values = c(
      "Participants Registered" = "grey70", 
      "Target Missed (Eligible - Registered)" = "indianred"
    )) +
    theme_light(base_size = base_size) + 
    theme(
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.title.x = element_blank(), 
      legend.position = "none"
    )
  
  if (nrow(events_df) > 0) {
    plot_top <- plot_top +
      geom_vline(
        data = events_df, 
        aes(xintercept = period_start), 
        linetype = "dashed", 
        color = "grey"
      ) + 
      geom_text(
        data = events_df, 
        aes(x = period_start + 3, y = max_y_p2 * 0.8, label = event_label), 
        size = base_size * 0.27, # Scale text size relative to base_size
        color = "black", 
        inherit.aes = FALSE, 
        hjust = 0
      )
  }
  
  # 5. Bottom Plot (Percentages - Thick panel, viridis colors, legend)
  plot_bottom <- ggplot(p1_data, aes(x = period_start, y = Value, color = Indicator_Name, group = Indicator_Name)) +
    geom_line(linewidth = 0.8) + 
    geom_point(size = 1.5) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"), 
      limits = c(0, 100), 
      breaks = seq(0, 100, by = 10)
    ) + 
    scale_x_date(
      limits = c(start_date, end_date), 
      date_breaks = date_breaks, 
      date_labels = date_labels
    ) + 
    scale_color_viridis_d(option = "F", begin = 0.2, end = 0.8) +
    labs(x = "Time (Weeks)", y = "Percentage (%)", color = "Indicator") +
    theme_light(base_size = base_size) + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), 
      legend.position = "bottom", 
      legend.title = element_blank()
    )
  
  # 6. Grid Assembly (Original 30/70 ratio)
  cowplot::plot_grid(
    plot_top, 
    plot_bottom, 
    ncol = 1, 
    align = "v", 
    rel_heights = c(0.3, 0.7)
  )
}




#' Plot weekly proportion of TB outcomes for the last 6 months
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tb_outcome_proportions_6m <- function(data = screening_data) {
  
  # Data manipulation: Filter for last 6 months and handle factors
  plot_data <- data %>% 
    filter(week_reg >= (max(week_reg[!is.na(week_reg)]) %m-% months(6))) %>%
    mutate(
      tb_decision = as.character(tb_decision),
      tb_decision = replace(tb_decision, is.na(tb_decision), "Missing"),
      tb_decision = factor(tb_decision, levels = c("Missing", "TB status uncertain", "Ruled out TB", "Presumptive TB"))
    ) %>% 
    group_by(week_reg, tb_decision) %>% 
    summarise(count = n(), .groups = 'drop') %>% 
    group_by(week_reg) %>% 
    mutate(proportion = count / sum(count))
  
  # Construct output
  ggplot(plot_data, aes(x = week_reg, y = proportion, fill = tb_decision)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(
      values = c(
        "Missing" = "lightgrey", 
        "TB status uncertain" = "palegreen3", 
        "Ruled out TB" = "lightblue", 
        "Presumptive TB" = "lightcoral"
      )
    ) +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + # Refactor: 0.25 -> 25%
    labs(
      x = "Week of Registration", 
      y = "Proportion", 
      title = "Weekly proportion of TB outcome (Last 6 Months)",
      fill = "TB decision"
    ) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



#' Plot weekly proportion of TST results for the last 6 months
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tst_proportions_6m <- function(data = screening_data) {
  
  # Data manipulation: Filter for 6-month window and handle factor levels
  plot_data <- data %>% 
    filter(week_reg >= (max(week_reg[!is.na(week_reg)]) %m-% months(6))) %>%
    mutate(
      tst_read_positive = as.character(tst_read_positive),
      tst_read_positive = replace(tst_read_positive, is.na(tst_read_positive), "Missing"),
      tst_read_positive = factor(tst_read_positive, levels = c("Missing", "Negative TST", "Positive TST"))
    ) %>% 
    group_by(week_reg, tst_read_positive) %>% 
    summarise(count = n(), .groups = 'drop') %>% 
    group_by(week_reg) %>% 
    mutate(proportion = count / sum(count))
  
  # Construct output
  ggplot(plot_data, aes(x = week_reg, y = proportion, fill = tst_read_positive)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(
      values = c(
        "Missing" = "lightgrey", 
        "Negative TST" = "lightblue", 
        "Positive TST" = "lightcoral"
      )
    ) +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + 
    labs(
      x = "Week of Registration", 
      y = "Proportion", 
      title = "Weekly proportion of TST result (Last 6 Months)",
      fill = "TST result"
    ) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



#' Plot TPT cascade: from TST positive to treatment completion
#' @param s_data Dataframe. Defaults to screening_data from the environment
#' @param t_data Dataframe. Defaults to treatment_data from the environment
#' @param weeks_lookback Numeric. Number of weeks to look back for the "expected outcome" cohort (default 16)
out_plot_tpt_cascade <- function(s_data = screening_data, 
                                 t_data = treatment_data,
                                 weeks_lookback = 16) {
  
  # Define the lookback date for the "Expected Outcome" cohort
  # This uses the weeks_lookback parameter instead of a hard-coded value
  lookback_date <- Sys.Date() - lubridate::weeks(weeks_lookback)
  
  # 1. Aggregate Screening-derived stages
  tpt_cascade_sd <- s_data %>%
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
    tidyr::pivot_longer(everything(), names_to = "stage", values_to = "count")
  
  # 2. Aggregate Treatment-derived stages
  tpt_cascade_td <- t_data %>%
    summarise(
      `Started TPT` = sum(!is.na(tpt_start_date), na.rm = TRUE),
      `4+ months since starting` = sum(!is.na(tpt_start_date) & tpt_start_date <= lookback_date, na.rm = TRUE),
      `Treatment Outcome Assigned` = sum(!is.na(tpt_outcome_reason), na.rm = TRUE),
      `Completed TPT` = sum(tpt_outcome_reason == "Completed", na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(everything(), names_to = "stage", values_to = "count")
  
  # 3. Combine & Order factors
  tpt_cascade <- dplyr::bind_rows(tpt_cascade_sd, tpt_cascade_td) %>%
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
  
  # Mapping labels for visual clarity
  cascade_labels <- c(
    "TST Positive"                = "TST+",
    "TST Positive, TB ruled out"  = "TST+\nTB ruled out",
    "Completed TPT Assessment"    = "Assessment\ncompleted",
    "Eligible for TPT"            = "Eligible\nfor TPT",
    "Started TPT"                 = "Started\nTPT",
    "4+ months since starting"    = paste0("≥", round(weeks_lookback/4), " months\nsince start"),
    "Treatment Outcome Assigned"  = "Outcome\nassigned",
    "Completed TPT"               = "Completed\nTPT"
  )
  
  # 4. Construct Output
  ggplot(tpt_cascade, aes(x = stage, y = count, fill = stage)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = scales::comma(count)), 
      vjust = -0.35, 
      size = 5, 
      fontface = "bold"
    ) +
    scale_x_discrete(labels = cascade_labels, drop = FALSE) +
    scale_y_continuous(
      name = "Number of individuals", 
      expand = expansion(mult = c(0.02, 0.10)),
      labels = scales::comma
    ) +
    scale_fill_viridis_d() +
    labs(title = "TST-Positive Treatment Cascade (All patients)", x = NULL) +
    theme_light() +
    theme(
      axis.text.x  = element_text(size = 9, lineheight = 0.95),
      axis.ticks.x = element_line(),
      legend.position = "none"
    )
}


#' Plot pie chart of reasons for TPT ineligibility
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tpt_ineligibility_reasons <- function(data = screening_data) {
  
  # Data manipulation: Using the pre-calculated column for efficiency
  tpt_ineligible <- data %>%
    filter(!is.na(tpt_inelig_reason)) %>%
    count(tpt_inelig_reason, name = "n") %>%
    rename(reason = tpt_inelig_reason) %>%
    arrange(desc(n))
  
  # Defensive check for empty datasets
  if (nrow(tpt_ineligible) == 0) {
    tpt_ineligible <- tibble::tibble(reason = "No data", n = 1L)
  }
  
  # Construct output
  ggplot(tpt_ineligible, aes(x = "", y = n, fill = reason)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA)) + # Added white background
    labs(title = "Reasons for TPT Ineligibility (All patients)", fill = "Reason") +
    scale_fill_viridis_d() +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 5, fontface = "bold", colour = "grey10")
}



#' Plot pie chart of reasons for incomplete TPT assessment
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tpt_assessment_gaps <- function(data = screening_data) {
  
  # Data manipulation: Leverage the pre-calculated column from Script 03
  tpt_not_assessed <- data %>%
    filter(!is.na(tpt_not_assessed_reason)) %>%
    count(tpt_not_assessed_reason, name = "n") %>%
    rename(reason = tpt_not_assessed_reason) %>%
    arrange(desc(n))
  
  # Handle empty case to avoid a blank plot error
  if (nrow(tpt_not_assessed) == 0) {
    tpt_not_assessed <- tibble::tibble(reason = "No data", n = 1L)
  }
  
  # Construct output
  ggplot(tpt_not_assessed, aes(x = "", y = n, fill = reason)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA)) + # Added white background
    labs(title = "Reasons for Not Completing TPT Assessment (All patients)", fill = "Reason") +
    scale_fill_viridis_d() +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 5, fontface = "bold", colour = "grey10")
}



#' Plot monthly treatment proportions from screening activity
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_treatment_proportions_monthly <- function(data = screening_data) {
  
  # Define readable labels for calc_any_treatment
  treatment_labels <- c(
    "No treatment" = "No treatment",
    "MDT"          = "MDT",
    "TBRx"         = "TBRx",
    "TPT"          = "TPT",
    "SDR"          = "SDR"
  )
  
  # Custom colors (Yellow for MDT, Greens/Blues for TB/TPT, Purple for SDR)
  custom_colors <- c(
    "No treatment" = "lightgrey",
    "MDT"          = "#FDE725FF",
    "TBRx"         = "#35B779FF",
    "TPT"          = "#31688EFF",
    "SDR"          = "#440154FF"
  )
  
  # Data manipulation: use pre-calculated month_reg and coalesce for simplicity
  plot_data <- data %>%
    mutate(
      calc_any_treatment = coalesce(as.character(calc_any_treatment), "No treatment"),
      calc_any_treatment = factor(calc_any_treatment, 
                                  levels = names(treatment_labels), 
                                  labels = treatment_labels)
    )
  
  # Construct output: geom_bar(position = "fill") handles the % calculations
  ggplot(plot_data, aes(x = month_reg, fill = calc_any_treatment)) +
    geom_bar(position = "fill") +
    theme_light() +
    labs(
      title = "Treatment Proportions by Month",
      x = "Month",
      y = "Proportion",
      fill = "Treatment Type"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_fill_manual(values = custom_colors) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}



#' Plot monthly follow-up and clinical quality indicators with event annotations
#' @param data Dataframe. Defaults to monthly_long from the environment
out_plot_monthly_quality_indicators <- function(data = monthly_long) {
  
  # 1. Internal configuration: Programmatic Events
  events_df <- tibble::tibble(
    period_start = as.Date(c(
      "2023-03-27", "2023-08-07", "2023-10-30",
      "2024-04-08", "2024-10-14", "2025-01-13"
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
  
  # 2. Indicator mapping
  indicator_labels_followup <- c(
    "ntp_out_pct"                 = "NTP Outcome Recorded",
    "nlp_out_pct"                 = "NLP Outcome Recorded",
    "tpt_assessed_of_should_pct"  = "TPT Assessed / Should Assess",
    "tpt_started_of_eligible_pct" = "TPT Started / Eligible",
    "tpt_eligible_of_started_pct" = "Eligible among Started"
  )
  
  # 3. Data Preparation
  plot_data <- data %>%
    dplyr::filter(Indicator %in% names(indicator_labels_followup)) %>%
    dplyr::mutate(Indicator = dplyr::recode(Indicator, !!!indicator_labels_followup))
  
  # 4. Dynamic Headroom Calculation
  y_max <- suppressWarnings(max(plot_data$Value, na.rm = TRUE))
  y_cap <- if (is.finite(y_max) && y_max > 0) ceiling(y_max / 10) * 10 else 100
  
  # 5. Construct Output
  ggplot(plot_data, aes(x = period_start, y = Value, color = Indicator, group = Indicator)) +
    geom_line(linewidth = 0.9) + # Modernized from size to linewidth
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
      plot.title      = element_text(size = 11, face = "bold"),
      axis.title      = element_text(size = 10),
      axis.text       = element_text(size = 9),
      legend.title    = element_blank(),
      legend.text     = element_text(size = 9),
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      axis.ticks.y.right = element_line()
    ) +
    # Annotated Events - using period_start directly to avoid numeric conversion warnings
    geom_vline(
      data = events_df,
      aes(xintercept = period_start),
      linetype = "dashed", 
      color = "grey60",
      linewidth = 0.5
    ) +
    geom_text(
      data = events_df,
      aes(x = period_start + 2, y = y_cap * 0.95, label = event_label),
      size = 3.3, 
      color = "black", 
      inherit.aes = FALSE, 
      hjust = 0, 
      vjust = 1
    )
}



#' Plot TB screening outcomes (presumptive and confirmed) by age and sex
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tb_yield_demographics <- function(data = screening_data) {
  
  # 1. Data Preparation: Vectorized Prevalence Calculation
  # We only calculate for those with a 'valid_tb_outcome' (Presumptive or Ruled Out)
  tb_plot_df <- data %>%
    filter(!is.na(age_cat), en_sex %in% c("M", "F")) %>%
    mutate(valid_tb_outcome = tb_decision %in% c("Presumptive TB", "Ruled out TB")) %>%
    filter(valid_tb_outcome == TRUE) %>%
    group_by(age_cat, en_sex) %>%
    summarise(
      # Using mean() on logicals automatically calculates the proportion (0-1)
      `Screened Positive` = mean(tb_decision == "Presumptive TB", na.rm = TRUE),
      `Confirmed TB`      = mean(ntp_diagnosis == "Confirmed", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Screened Positive`, `Confirmed TB`),
      names_to = "metric", 
      values_to = "proportion"
    ) %>%
    mutate(
      metric = factor(metric, levels = c("Screened Positive", "Confirmed TB")),
      en_sex = factor(en_sex, levels = c("F","M"), labels = c("Female","Male"))
    )
  
  # 2. Dynamic Headroom Calculation
  # We work in decimals (0.15) rather than integers (15) for the cap
  y_max <- suppressWarnings(max(tb_plot_df$proportion, na.rm = TRUE))
  y_cap <- if (is.finite(y_max)) ceiling(y_max * 10) / 10 else 1.0
  y_cap <- max(y_cap, 0.1)
  
  # 3. Construct Output
  ggplot(tb_plot_df, aes(x = age_cat, y = proportion, fill = en_sex)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65, na.rm = TRUE) +
    # CHANGE: Set scales to "free_y"
    facet_wrap(~ metric, nrow = 1, scales = "free_y") + 
    scale_y_continuous(
      # REMOVED: limits = c(0, y_cap)
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.15)) # Increased headroom for free labels
    ) +
    scale_fill_viridis_d(option = "F", begin = 0.2, end = 0.8) +
    labs(
      title  = "TB screening outcomes by age group and sex",
      subtitle = "Denominator: Individuals with a completed TB diagnostic decision",
      x      = "Age group",
      y      = "Prevalence (%)",
      fill   = "Sex"
    ) +
    theme_light() +
    theme(
      plot.title   = element_text(size = 11, face = "bold"),
      axis.title   = element_text(size = 10),
      axis.text    = element_text(size = 9),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text.x  = element_text(angle = 45, hjust = 1)
    )
}



#' Plot Leprosy screening outcomes (presumptive and confirmed) by age and sex
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_lep_yield_demographics <- function(data = screening_data) {
  
  # 1. Data Preparation: Use vectorized mean for prevalence
  # We filter for those with a valid leprosy referral decision (TRUE/FALSE)
  lep_plot_df <- data %>%
    filter(!is.na(age_cat), en_sex %in% c("M","F"), !is.na(lep_refer)) %>%
    group_by(age_cat, en_sex) %>%
    summarise(
      `Screened Positive` = mean(lep_refer == TRUE, na.rm = TRUE),
      `Confirmed Leprosy` = mean(nlp_diagnosis == "Confirmed", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Screened Positive`, `Confirmed Leprosy`),
      names_to = "metric", 
      values_to = "proportion"
    ) %>%
    mutate(
      metric = factor(metric, levels = c("Screened Positive", "Confirmed Leprosy")),
      en_sex = factor(en_sex, levels = c("F","M"), labels = c("Female","Male"))
    )
  
  # 2. Dynamic Headroom Calculation
  # For Leprosy, we use a tighter rounding (to the nearest 5%)
  y_max <- suppressWarnings(max(lep_plot_df$proportion, na.rm = TRUE))
  y_cap <- if (is.finite(y_max)) ceiling(y_max * 20) / 20 else 0.2
  y_cap <- max(y_cap, 0.05)
  
  # 3. Construct Output
  ggplot(lep_plot_df, aes(x = age_cat, y = proportion, fill = en_sex)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65, na.rm = TRUE) +
    # CHANGE: Set scales to "free_y"
    facet_wrap(~ metric, nrow = 1, scales = "free_y") + 
    scale_y_continuous(
      # REMOVED: limits = c(0, y_cap)
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.15))
    ) +
    scale_fill_viridis_d(option = "F", begin = 0.2, end = 0.8) +
    labs(
      title  = "Leprosy screening outcomes by age group and sex",
      subtitle = "Denominator: Individuals with a recorded leprosy referral decision",
      x      = "Age group",
      y      = "Prevalence (%)",
      fill   = "Sex"
    ) +
    theme_light() +
    theme(
      plot.title      = element_text(size = 11, face = "bold"),
      axis.title      = element_text(size = 10),
      axis.text       = element_text(size = 9),
      legend.title    = element_blank(),
      legend.position = "bottom",
      strip.text      = element_text(size = 10, face = "bold"),
      axis.text.x      = element_text(angle = 45, hjust = 1)
    )
}



#' Plot TPT Risk Assessment Cascade
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tpt_risk_cascade <- function(data = screening_data) {
  
  # 1. Data Preparation: Aggregate counts for each stage
  # We use the logical columns (tptrf_...) pre-calculated in Script 03
  tptrf_cascade <- data %>%
    summarise(
      `TST Positive, TB ruled out` = sum(
        (tst_read_positive == "Positive TST") &
          (tb_decision == "Ruled out TB" | ntp_diagnosis == "Ruled out"),
        na.rm = TRUE
      ),
      `Risk factors assessed`    = sum(tptrf_assessed,        na.rm = TRUE),
      `Baseline ALT needed`      = sum(tptrf_alt_needed,      na.rm = TRUE),
      `Baseline ALT requested`   = sum(tptrf_alt_requested,   na.rm = TRUE),
      `Baseline ALT resulted`    = sum(tptrf_alt_result,      na.rm = TRUE),
      `Risk group assigned`      = sum(tptrf_risk_assigned,   na.rm = TRUE),
      `Risk matches expected`    = sum(tptrf_risk_as_expected, na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(everything(), names_to = "stage", values_to = "count") %>%
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
  
  # 2. Short multi-line labels for x-axis clarity
  cascade_labels <- c(
    "TST Positive, TB ruled out" = "TST+\nTB ruled out",
    "Risk factors assessed"      = "Risk factors\nassessed",
    "Baseline ALT needed"        = "Baseline ALT\nneeded",
    "Baseline ALT requested"     = "Baseline ALT\nrequested",
    "Baseline ALT resulted"      = "Baseline ALT\nresulted",
    "Risk group assigned"        = "Risk group\nassigned",
    "Risk matches expected"      = "Risk matches\nexpected"
  )
  
  # 3. Construct Output
  ggplot(tptrf_cascade, aes(x = stage, y = count, fill = stage)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = scales::comma(count)), 
      vjust = -0.35, 
      size = 5, 
      fontface = "bold"
    ) +
    scale_x_discrete(labels = cascade_labels, drop = FALSE) +
    scale_y_continuous(
      name = "Number of individuals",
      expand = expansion(mult = c(0.02, 0.10))
    ) +
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
}


# --- TREATMENT & OUTCOME PLOTS ------------------------------------------------

#' Plot monthly proportions of TPT outcomes
#' @param data Dataframe. Defaults to treatment_data from the environment
out_plot_tpt_outcome_proportions <- function(data = treatment_data) {
  
  # Define labels for the factor mapping
  tpt_outcome_labels <- c(
    "Not yet assigned" = "Not yet assigned",
    "Lost to follow-up" = "Lost to Follow-up",
    "Withdrew consent"  = "Withdrew Consent",
    "Discontinued"      = "Discontinued (Medical Reason)",
    "Died"              = "Died",
    "Completed"         = "Completed TPT"
  )
  
  # Data manipulation: Use existing month_start and handle factor mapping
  plot_data <- data %>%
    mutate(
      # Coalesce is a more concise way to replace NAs than ifelse
      tpt_outcome_reason = coalesce(as.character(tpt_outcome_reason), "Not yet assigned"),
      tpt_outcome_reason = factor(tpt_outcome_reason, 
                                  levels = names(tpt_outcome_labels), 
                                  labels = tpt_outcome_labels)
    )
  
  # Construct output: Passing individual records to geom_bar(position = "fill")
  # allows ggplot to calculate the percentages automatically.
  ggplot(plot_data, aes(x = month_start, fill = tpt_outcome_reason)) +
    geom_bar(position = "fill") +
    theme_light() +
    labs(
      title = "TPT Outcome Proportions by Month",
      x = "Month",
      y = "Proportion",
      fill = "TPT Outcome"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_fill_viridis_d(direction = -1) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}



#' Plot monthly treatment follow-up and completion rates by start cohort
#' @param data Dataframe. Defaults to monthly_long from the environment
out_plot_tpt_followup_monthly <- function(data = monthly_long) {
  
  # 1. Internal configuration: Programmatic Events
  # (Consider centralizing this as per the previous efficiency note)
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
  
  # 2. Indicator mapping
  indicator_labels_tpt <- c(
    "tpt_1m_done_pct"           = "1-Month Review Done / Expected",
    "tpt_3m_done_pct"           = "3-Month Review Done / Expected",
    "tpt_4m_done_pct"           = "4-Month Review Done / Expected",
    "tpt_outcome_assigned_pct"  = "Outcome Assigned / Started",
    "tpt_completed_pct"         = "Completed / Started"
  )
  
  # 3. Data Preparation
  plot_data <- data %>%
    filter(Indicator %in% names(indicator_labels_tpt)) %>%
    mutate(Indicator = recode(Indicator, !!!indicator_labels_tpt))
  
  # 4. Dynamic Headroom Calculation
  y_max <- suppressWarnings(max(plot_data$Value, na.rm = TRUE))
  y_cap <- if (is.finite(y_max)) ceiling(y_max / 10) * 10 else 100
  if (y_cap < 100) y_cap <- 100 # ensure standard 0-100 range at minimum
  
  # 5. Construct Output
  ggplot(plot_data, aes(x = period_start, y = Value, color = Indicator, group = Indicator)) +
    geom_line(linewidth = 0.9) +
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
      title = "Monthly treatment follow-up & completion (start cohorts)",
      x = "Month",
      y = "Percent",
      color = "Indicator"
    ) +
    theme_light() +
    theme(
      plot.title      = element_text(size = 11),
      axis.title      = element_text(size = 10),
      axis.text       = element_text(size = 9),
      legend.title    = element_blank(),
      legend.text     = element_text(size = 9),
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      axis.ticks.y.right = element_line()
    ) +
    # Annotated Events
    geom_vline(
      data = events_df,
      aes(xintercept = as.numeric(period_start)),
      linetype = "dashed", color = "grey"
    ) +
    geom_text(
      data = events_df,
      aes(x = period_start + 3, y = y_cap * 0.95, label = event_label),
      size = 3.3, color = "black", inherit.aes = FALSE, hjust = 0, vjust = 0
    )
}



#' Plot TST-Positive treatment retention (Step Function)
#' @param data Dataframe. Defaults to treatment_data from the environment
#' @param max_day Integer. Number of days to plot (default 168 / 24 weeks)
out_plot_tpt_retention_step <- function(data = treatment_data, 
                                        max_day = 168) {
  
  # 1. Build the cohort
  td_cohort <- data %>%
    filter(!is.na(tpt_start_date), !is.na(tpt_dur_real)) %>%
    transmute(record_id, dur = as.integer(tpt_dur_real)) %>%
    mutate(dur = if_else(is.na(dur) | dur < 0, 0L, dur))
  
  cohort_n <- nrow(td_cohort)
  days_vec <- 0:max_day
  
  # 2. Optimized Vectorization
  if (cohort_n == 0) {
    retention <- tibble(day = days_vec, on_n = 0L, pct_on = NA_real_)
  } else {
    retention <- tibble(
      day  = days_vec,
      on_n = rowSums(outer(days_vec, td_cohort$dur, "<=")) 
    ) %>%
      mutate(pct_on = (on_n / cohort_n) * 100)
  }
  
  # 3. Construct Output
  ggplot(retention, aes(x = day, y = pct_on)) +
    geom_step(linewidth = 1, color = "midnightblue") +
    # Weekly anchors
    geom_point(data = filter(retention, day %% 7 == 0), size = 1.6) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 10),
      # Fix: Use label_number with suffix instead of label_append
      labels = scales::label_number(suffix = "%")
    ) +
    scale_x_continuous(
      limits = c(0, max_day),
      breaks = seq(0, max_day, 14),
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
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_line(color = "grey95")
    )
}


#' Plot age-sex pyramid for the TPT treatment cohort
#' @param data Dataframe. Defaults to treatment_data from the environment
out_plot_tpt_age_pyramid <- function(data = treatment_data) {
  
  # 1. Data Preparation
  # Note: apyramid handles the 'split_by' variable as a factor automatically,
  # but pre-calculating the factor ensures the legend order is consistent.
  plot_data <- data %>%
    filter(tpt_sex %in% c("M", "F"), !is.na(age_cat)) %>%
    mutate(tpt_sex = factor(tpt_sex, levels = c("M", "F"), labels = c("Male", "Female")))
  
  # 2. Construct Output using apyramid::age_pyramid
  apyramid::age_pyramid(
    data      = plot_data,
    age_group = "age_cat",
    split_by  = "tpt_sex"
  ) +
    scale_fill_viridis_d(option = "F", begin = 0.4, end = 0.7) +
    labs(
      title    = "Age–sex distribution of people on TPT",
      subtitle = "Treatment dataset only",
      x        = "Count",
      y        = "Age category"
    ) +
    theme_light() +
    theme(
      legend.title    = element_blank(),
      legend.position = "bottom"
    )
}



#' Plot prevalence of any symptoms during TPT by age group and sex
#' @param data Dataframe. Defaults to treatment_data from the environment
out_plot_tpt_symptoms_demographics <- function(data = treatment_data) {
  
  # 1. Data Preparation: Aggregate symptoms across all monitoring timepoints
  plot_data <- data %>%
    filter(tpt_sex %in% c("M", "F"), 
           !is.na(age_cat), 
           !is.na(tpt_start_date)) %>%
    mutate(
      # Standardize sex factor for consistent visual layout
      tpt_sex = factor(tpt_sex, levels = c("F", "M"), labels = c("Female", "Male")),
      # Vectorized logic for 'any symptom' across the treatment journey
      any_sx = coalesce(`1m_any_true`, FALSE) |
        coalesce(`3m_any_true`, FALSE) |
        coalesce(`4m_any_true`, FALSE) |
        coalesce(`ae_any_true`, FALSE)
    ) %>%
    group_by(age_cat, tpt_sex) %>%
    summarise(
      # Calculate prevalence as a decimal for scales compatibility
      prevalence = mean(any_sx, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # 2. Dynamic Headroom Calculation (Rounded to nearest 5%)
  y_max <- suppressWarnings(max(plot_data$prevalence, na.rm = TRUE))
  y_cap <- if (is.finite(y_max)) max(0.05, ceiling(y_max * 20) / 20) else 0.05
  
  # 3. Construct Output
  ggplot(plot_data, aes(x = age_cat, y = prevalence, fill = tpt_sex)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    scale_y_continuous(
      limits = c(0, y_cap),
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_fill_viridis_d(option = "F", begin = 0.2, end = 0.8, name = "Sex") +
    labs(
      title = "Symptoms during TPT monitoring by age group and sex",
      subtitle = "Denominator: Individuals started on TPT with at least one follow-up record",
      x = "Age group",
      y = "Prevalence of any symptom (%)"
    ) +
    theme_light() +
    theme(
      plot.title      = element_text(size = 11, face = "bold"),
      axis.title      = element_text(size = 10),
      axis.text       = element_text(size = 9),
      legend.position = "bottom",
      axis.text.x      = element_text(angle = 45, hjust = 1)
    )
}


# --- CLINICAL & DIAGNOSTIC PLOTS ----------------------------------------------

#' Plot TST positivity prevalence by age category
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tst_positivity_by_age <- function(data = screening_data) {
  
  # Data manipulation: Calculate prevalence as a decimal (0-1)
  tst_summary <- data %>%
    filter(!is.na(tst_read_positive), !is.na(age_cat)) %>%
    group_by(age_cat) %>%
    summarise(
      prevalence = mean(tst_read_positive == "Positive TST", na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) 
  
  # Construct output
  ggplot(tst_summary, aes(x = age_cat, y = prevalence)) +
    geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
    # Apply scales::percent for consistent, professional labels
    geom_text(
      aes(label = scales::percent(prevalence, accuracy = 0.1)), 
      vjust = -0.5, 
      size = 4
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.15)) # Adds headroom for the labels
    ) +
    labs(
      x = "Age Category (10-year groups)",
      y = "Percentage with Positive TST",
      title = "Proportion of Positive TST by Age Group",
      subtitle = "Prevalence of latent TB infection by age cohort"
    ) +
    theme_light()
}



#' Plot TST positivity proportions by age for 5mm and 10mm thresholds
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tst_thresholds_age <- function(data = screening_data) {
  
  # 1. Prep data: calculate proportions in each age category
  prop_by_age <- data %>%
    filter(!is.na(tst_read_mm), !is.na(age_cat)) %>%
    group_by(age_cat) %>%
    summarise(
      prop_5mm = mean(tst_read_mm >= 5),
      prop_10mm = mean(tst_read_mm >= 10),
      .groups = "drop"
    )
  
  # 2. Get overall proportions for benchmarking
  overall <- data %>%
    filter(!is.na(tst_read_mm)) %>%
    summarise(
      prop_5mm = mean(tst_read_mm >= 5),
      prop_10mm = mean(tst_read_mm >= 10)
    )
  
  # 3. Pivot data to long format
  prop_long <- prop_by_age %>%
    tidyr::pivot_longer(
      cols = starts_with("prop_"),
      names_to = "threshold",
      values_to = "proportion"
    ) %>%
    mutate(
      threshold = recode(threshold, prop_5mm = "≥5mm", prop_10mm = "≥10mm"),
      threshold = factor(threshold, levels = c("≥5mm", "≥10mm"))
    )
  
  # 4. Color map
  color_map <- c("≥5mm" = "coral4", "≥10mm" = "turquoise4")
  
  # 5. Construct Output
  ggplot(prop_long, aes(x = age_cat, y = proportion, group = threshold, color = threshold)) +
    # Lines and Points
    geom_line(linewidth = 1) +
    geom_point(aes(shape = threshold), size = 3) +
    
    # Benchmarks: Using mapping inside geom_hline makes them part of the legend
    geom_hline(aes(yintercept = prop_5mm, color = "≥5mm"), data = overall, linetype = "dashed", alpha = 0.6) +
    geom_hline(aes(yintercept = prop_10mm, color = "≥10mm"), data = overall, linetype = "dashed", alpha = 0.6) +
    
    # Scales and Aesthetics
    scale_color_manual(values = color_map) +
    scale_shape_manual(values = c("≥5mm" = 16, "≥10mm" = 17)) + # Robust solid shapes (circle/triangle)
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "Age Category",
      y = "Percentage Screen Test ≥ Threshold",
      color = "Threshold",
      shape = "Threshold",
      title = "Proportion of Participants with Skin Test ≥ 5mm or ≥ 10mm by Age Category",
      subtitle = "Dashed lines indicate project-wide averages for each threshold"
    ) +
    theme_light() +
    theme(legend.position = "top")
}



#' Plot TST positivity prevalence by age group and sex
#' @param data Dataframe. Defaults to screening_data from the environment
out_plot_tst_yield_demographics <- function(data = screening_data) {
  
  # 1. Data Preparation: Calculate prevalence among those with a valid TST read
  tst_plot_df <- data %>%
    filter(!is.na(age_cat), 
           en_sex %in% c("M", "F"), 
           tst_read_bin == TRUE) %>%
    group_by(age_cat, en_sex) %>%
    summarise(
      # Using mean() on the logical condition for efficiency
      prevalence = mean(tst_read_positive == "Positive TST", na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      en_sex = factor(en_sex, levels = c("F","M"), labels = c("Female","Male"))
    )
  
  # 2. Dynamic Headroom Calculation (Rounded to nearest 5%)
  y_max <- suppressWarnings(max(tst_plot_df$prevalence, na.rm = TRUE))
  y_cap <- if (is.finite(y_max)) max(0.05, ceiling(y_max * 20) / 20) else 0.05
  
  # 3. Construct Output
  ggplot(tst_plot_df, aes(x = age_cat, y = prevalence, fill = en_sex)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    scale_y_continuous(
      limits = c(0, y_cap),
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_viridis_d(option = "F", begin = 0.2, end = 0.8, name = "Sex") +
    labs(
      title = "TST positivity by age group and sex",
      subtitle = "Denominator: Individuals with a successfully read TST",
      x = "Age group",
      y = "Prevalence (%)"
    ) +
    theme_light() +
    theme(
      plot.title      = element_text(size = 11, face = "bold"),
      axis.title      = element_text(size = 10),
      axis.text       = element_text(size = 9),
      legend.position = "bottom",
      axis.text.x      = element_text(angle = 45, hjust = 1)
    )
}


# --- SUMMARY TABLES -----------------------------------------------------------

#' Generate a flextable activity summary using pre-aggregated weekly indicators
#' @param data Dataframe. Defaults to weekly_data (the tidy summary object)
#' @param target_week Date. Optional; defaults to the latest week in data
out_tab_activity_summary <- function(data = weekly_data, target_week = NULL) {
  
  # 1. Determine reporting dates
  if (is.null(target_week)) {
    target_week <- max(data$period_start, na.rm = TRUE)
  }
  last_week_date <- target_week - lubridate::weeks(1)
  
  # 2. Extract specific week rows and Cumulative sums
  wd_this <- data %>% dplyr::filter(period_start == target_week)
  wd_last <- data %>% dplyr::filter(period_start == last_week_date)
  
  # Map indicators to their display names
  indicator_map <- c(
    "hh_enum_new"  = "Households Enumerated",
    "hh_screened"  = "Households Screened",
    "reg"          = "People Registered",
    "tst_read"     = "TSTs Completed",
    "ref_ntp"      = "Referred to NTP",
    "ref_nlp"      = "Referred to NLP",
    "ref_hbv"      = "Referred to Hep B",
    "cxr_done"     = "X-Rays Performed",
    "cxr_result"   = "X-Rays Resulted",
    "xpert"        = "Xpert Tests Done",
    "tpt_start"    = "Started on TPT",
    "tpt_completed" = "Completed TPT"
  )
  
  # Build the 3-column data frame
  # Refactored: using sum(..., na.rm = TRUE) to safely handle empty dataframes 
  # (e.g., weeks with 0 activity) instead of %||% 0.
  summary_df <- tibble::tibble(Indicator_Key = names(indicator_map)) %>%
    mutate(
      Indicator = unname(indicator_map),
      This_Week = purrr::map_dbl(Indicator_Key, ~ sum(wd_this[[.x]], na.rm = TRUE)),
      Last_Week = purrr::map_dbl(Indicator_Key, ~ sum(wd_last[[.x]], na.rm = TRUE)),
      Total_Count = purrr::map_dbl(Indicator_Key, ~ sum(data[[.x]], na.rm = TRUE))
    ) %>%
    select(Indicator, This_Week, Last_Week, Total_Count)
  
  # 3. Styling
  ft <- summary_df %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      This_Week = "This Week", Last_Week = "Last Week", Total_Count = "Cumulative"
    ) %>%
    flextable::add_header_lines(values = paste("Table: PEARL Activity Summary: Week of", format(target_week, "%d %b %Y"))) %>%
    theme_pearl() %>%
    flextable::autofit()
  
  return(ft)
}

# --- PERFORMANCE TABLES -------------------------------------------------------

#' Master dictionary of all aggregated PEARL indicators
get_all_indicators_dict <- function() {
  tibble::tribble(
    ~Group,                        ~Indicator_Key,                ~Indicator_Name,          ~is_core,
    # --- 1. HH Enumeration & Targets ---
    "1. HH Enumeration & Targets", "hh_enum_new",                 "HHs Enumerated",         FALSE,
    "1. HH Enumeration & Targets", "target_hh",                    "Target HHs",             FALSE,
    "1. HH Enumeration & Targets", "target_all",                   "Target Pop (All)",       FALSE,
    "1. HH Enumeration & Targets", "target_eligible",              "Target Pop (Eligible)",   FALSE,
    "1. HH Enumeration & Targets", "target_non_eligible",          "Target Pop (Ineligible)", FALSE,
    "1. HH Enumeration & Targets", "hh_screened",                  "HHs Screened",           FALSE,
    # --- 2. Registration ---
    "2. Registration",             "reg",                         "Registered",              TRUE,
    "2. Registration",             "reg_m",                       "Registered (Male)",       FALSE,
    "2. Registration",             "reg_f",                       "Registered (Female)",     FALSE,
    "2. Registration",             "target_missed",                "Number Missing",         TRUE,
    # --- 3. X-Ray ---
    "3. X-Ray",                    "cxr_elig",                    "XR Eligible",            FALSE,
    "3. X-Ray",                    "cxr_done",                    "XR Performed",           FALSE,
    "3. X-Ray",                    "cxr_pct",                     "XR Performed (%)",        TRUE,
    "3. X-Ray",                    "cxr_result",                  "XR Resulted",            FALSE,
    "3. X-Ray",                    "cxr_res_pct",                 "XR Resulted (%)",         FALSE,
    # --- 4. Sputum / Xpert ---
    "4. Sputum / Xpert",           "xpert",                       "Sputum tested",          FALSE,
    "4. Sputum / Xpert",           "xpert_pct",                   "Sputum tested (%)",       TRUE,
    # --- 5. TB Decision ---
    "5. TB Decision",              "tbdec",                       "TB Dec",                 FALSE,
    "5. TB Decision",              "tbdec_pct",                   "TB Dec (%)",              TRUE,
    "5. TB Decision",              "reg_complete",                "TB Dec Complete",        FALSE,
    "5. TB Decision",              "tbdec_comp_pct",              "TB Dec Complete (%)",     TRUE,
    "5. TB Decision",              "tbdec_prestb",                "TB Presumptive",         FALSE,
    "5. TB Decision",              "tbdec_ro",                    "TB Ruled Out",           FALSE,
    "5. TB Decision",              "tbdec_unc",                   "TB Uncertain",           FALSE,
    "5. TB Decision",              "tbdec_missing",               "TB Missing",             FALSE,
    "5. TB Decision",              "ntp_outcome_recorded",        "NTP Outcome",            FALSE,
    "5. TB Decision",              "ntp_out_pct",                 "NTP Outcome (%)",         FALSE,
    # --- 6. TST Placement & Results ---
    "6. TST Placement & Results",  "tst_placed",                  "TST Placed",             FALSE,
    "6. TST Placement & Results",  "tst_place_pct",               "TST Placed (%)",          FALSE,
    "6. TST Placement & Results",  "tst_read",                    "TST Read",               FALSE,
    "6. TST Placement & Results",  "tst_read_pct",                "TST Read (%)",            TRUE,
    "6. TST Placement & Results",  "tst_pos",                     "TST Pos",                FALSE,
    "6. TST Placement & Results",  "tst_neg",                     "TST Neg",                FALSE,
    "6. TST Placement & Results",  "tst_missing",                 "TST Missing",            FALSE,
    # --- 7. TPT Assessment ---
    "7. TPT Assessment",           "tpt_should_assess",           "TPT To Assess",          FALSE,
    "7. TPT Assessment",           "tpt_assessment_done",         "TPT Assessed",           FALSE,
    "7. TPT Assessment",           "tpt_assessed_of_should_pct",  "Assessed / Should (%)",   TRUE,
    "7. TPT Assessment",           "tpt_eligible_n",              "TPT Eligible",           FALSE,
    "7. TPT Assessment",           "tpt_inelig_n",                "TPT Ineligible",         FALSE,
    "7. TPT Assessment",           "tpt_assessing_n",             "TPT Pending",            FALSE,
    "7. TPT Assessment",           "tpt_started_n",               "TPT Started",            FALSE,
    "7. TPT Assessment",           "tpt_started_of_eligible_pct", "Started / Eligible (%)",  FALSE,
    "7. TPT Assessment",           "tpt_eligible_of_started_pct", "Eligible / Started (%)",  FALSE,
    # --- 8. Leprosy & Hep B ---
    "8. Leprosy & Hep B",          "ref_nlp",                     "NLP Referred",           FALSE,
    "8. Leprosy & Hep B",          "nlp_outcome_recorded",        "NLP Outcome",            FALSE,
    "8. Leprosy & Hep B",          "nlp_out_pct",                 "NLP Outcome (%)",         FALSE,
    "8. Leprosy & Hep B",          "ref_hbv",                     "Hep B Referred",         FALSE,
    "8. Leprosy & Hep B",          "anyrx",                       "Any Rx",                 FALSE,
    "8. Leprosy & Hep B",          "anyrx_pct",                   "Any Rx (%)",              TRUE
  )
}

#' Generate a weekly team performance table
#' @param font_size Numeric. Base font size for the table.
#' @param table_width Numeric. Total table width in inches. Defaults to NULL (autofit).
out_tab_team_weekly_review <- function(data = team_weekly_data, 
                                       weekly_df = weekly_data,
                                       target_week = NULL, 
                                       period = c("current", "previous"),
                                       core_only = TRUE,
                                       font_size = 9,
                                       table_width = NULL) {
  
  period <- match.arg(period)
  
  if (is.null(target_week)) {
    target_week <- max(data$period_start, na.rm = TRUE)
  }
  
  report_date <- if (period == "current") target_week else target_week - lubridate::weeks(1)
  
  dict <- get_all_indicators_dict()
  if (core_only) {
    dict <- dict %>% dplyr::filter(is_core == TRUE)
  }
  
  ind_keys <- dict$Indicator_Key
  pct_keys <- dict %>% 
    dplyr::filter(grepl("\\(\\%\\)", Indicator_Name)) %>% 
    dplyr::pull(Indicator_Key)
  
  report_week <- data %>% 
    dplyr::filter(period_start == report_date, !is.na(team)) %>%
    dplyr::select(team, dplyr::any_of(ind_keys))
  
  if (nrow(report_week) == 0) {
    return(flextable::flextable(data.frame(Msg = "No Data Available")))
  }
  
  all_row <- weekly_df %>%
    dplyr::filter(period_start == report_date) %>%
    dplyr::mutate(team = "ALL") %>%
    dplyr::select(team, dplyr::any_of(ind_keys))
  
  matrix_data <- dplyr::bind_rows(report_week, all_row) %>%
    tidyr::pivot_longer(
      cols = -team, 
      names_to = "Indicator_Key", 
      values_to = "Value"
    ) %>%
    dplyr::mutate(Val = dplyr::case_when(
      is.na(Value) ~ "-",
      Indicator_Key %in% pct_keys ~ paste0(sprintf("%.1f", Value), "%"),
      TRUE ~ formatC(Value, format = "f", digits = 0, big.mark = ",")
    )) %>%
    dplyr::select(team, Indicator_Key, Val) %>%
    tidyr::pivot_wider(
      names_from = team, 
      values_from = Val, 
      values_fill = "-"
    )
  
  final_df <- dict %>% 
    dplyr::left_join(matrix_data, by = "Indicator_Key") %>% 
    dplyr::select(-Indicator_Key, -is_core)
  
  # Identify the dynamic team columns for width allocation
  # (Everything except 'Group' and 'Indicator_Name')
  val_cols <- setdiff(names(final_df), c("Group", "Indicator_Name"))
  
  title_text <- paste0("Team Performance (", format(report_date, "%d %b %Y"), ")")
  
  ft <- final_df %>% 
    {if(core_only) dplyr::select(., -Group) else .} %>% 
    flextable::flextable() %>%
    flextable::add_header_lines(values = title_text) %>%
    theme_pearl() %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::bold(j = "ALL") %>%
    flextable::bg(j = "ALL", bg = "#F9F9F9", part = "body")
  
  if (!core_only) {
    ft <- ft %>% flextable::fontsize(size = font_size - 1, part = "body")
  }
  
  # Corrected Width Logic: Distribute the total width
  if (!is.null(table_width)) {
    num_val_cols <- length(val_cols)
    # splits the indicator vs week column width
    ft <- ft %>% 
      flextable::width(j = 1, width = table_width * 0.25) %>%
      flextable::width(j = val_cols, width = (table_width * 0.75) / num_val_cols)
  }
  
  return(ft)
}





#' Generate a weekly project performance trend table
#' @param font_size Numeric. Base font size.
#' @param table_width Numeric. Total table width in inches.
out_tab_project_weekly_review <- function(data = weekly_data, 
                                          end_date = NULL, 
                                          start_date = NULL,
                                          core_only = TRUE,
                                          font_size = 8,
                                          table_width = NULL) {
  
  if (is.null(end_date)) {
    end_date <- max(data$period_start, na.rm = TRUE)
  }
  if (is.null(start_date)) {
    start_date <- end_date - lubridate::weeks(12)
  }
  
  dict <- get_all_indicators_dict()
  if (core_only) {
    dict <- dict %>% dplyr::filter(is_core == TRUE)
  }
  
  ind_keys <- dict$Indicator_Key
  pct_keys <- dict %>% 
    dplyr::filter(grepl("\\(\\%\\)", Indicator_Name)) %>% 
    dplyr::pull(Indicator_Key)
  
  matrix_data <- data %>% 
    dplyr::filter(period_start >= start_date & period_start <= end_date) %>%
    dplyr::arrange(period_start) %>%
    dplyr::mutate(week_label = format(period_start, "%d %b")) %>%
    dplyr::select(week_label, dplyr::any_of(ind_keys)) %>%
    tidyr::pivot_longer(
      cols = -week_label, 
      names_to = "Indicator_Key", 
      values_to = "Value"
    ) %>%
    dplyr::mutate(Val = dplyr::case_when(
      is.na(Value) ~ "-",
      Indicator_Key %in% pct_keys ~ paste0(sprintf("%.1f", Value), "%"),
      TRUE ~ formatC(Value, format = "f", digits = 0, big.mark = ",")
    )) %>%
    dplyr::select(week_label, Indicator_Key, Val) %>%
    tidyr::pivot_wider(
      names_from = week_label, 
      values_from = Val, 
      values_fill = "-"
    )
  
  final_df <- dict %>% 
    dplyr::left_join(matrix_data, by = "Indicator_Key") %>% 
    dplyr::select(-Indicator_Key, -is_core)
  
  week_cols <- setdiff(names(final_df), c("Group", "Indicator_Name"))
  title_text <- paste0("Project Trend: ", format(start_date, "%d %b"), " to ", format(end_date, "%d %b %Y"))
  
  ft <- final_df %>% 
    {if(core_only) dplyr::select(., -Group) else .} %>% 
    flextable::flextable() %>%
    flextable::add_header_lines(values = title_text) %>%
    theme_pearl() %>%
    flextable::fontsize(size = font_size, part = "all")
  
  if (!is.null(table_width)) {
    col_count <- length(week_cols)
    ft <- ft %>% 
      flextable::width(j = 1, width = table_width * 0.3) %>%
      flextable::width(j = week_cols, width = (table_width * 0.7) / col_count)
  }
  
  return(ft)
}


# --- TREATMENT & OUTCOME TABLES -----------------------------------------------

#' Generate a flextable of monthly TPT outcomes (Count and Row-wise %)
#' @param data Dataframe. Defaults to treatment_data from the environment
#' @param weeks_lag Integer. Number of weeks to look back for the cohort (default 16)
out_tab_tpt_outcomes_monthly <- function(data = treatment_data, 
                                         weeks_lag = 16) {
  
  # 1. Configuration & Labels
  tpt_outcome_labels <- c(
    "Completed"         = "Completed TPT",
    "Died"              = "Died",
    "Discontinued"      = "Discontinued (Medical Reason)",
    "Lost to follow-up" = "Lost to Follow-up",
    "Withdrew consent"  = "Withdrew Consent",
    "Not yet assigned"  = "Not yet assigned"
  )
  
  # 2. Exclusion Window: Only analyze cohorts that should have finished (16 weeks)
  cutoff_date <- Sys.Date() - lubridate::weeks(weeks_lag)
  
  tpt_filtered <- data %>%
    dplyr::filter(!is.na(tpt_start_date), tpt_start_date < cutoff_date)
  
  # 3. Data Aggregation (Long to Wide)
  tpt_counts <- tpt_filtered %>%
    dplyr::mutate(
      month = lubridate::floor_date(tpt_start_date, "month"),
      tpt_outcome_reason = dplyr::coalesce(as.character(tpt_outcome_reason), "Not yet assigned"),
      tpt_outcome_reason = factor(tpt_outcome_reason, 
                                  levels = names(tpt_outcome_labels), 
                                  labels = tpt_outcome_labels)
    ) %>%
    dplyr::count(month, tpt_outcome_reason) %>%
    tidyr::pivot_wider(names_from = tpt_outcome_reason, values_from = n, values_fill = 0) %>%
    dplyr::arrange(month)
  
  # Add Row Totals and format Date
  tpt_counts <- tpt_counts %>%
    dplyr::mutate(
      Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE),
      month_label = format(as.Date(month), "%b %Y")
    ) %>%
    dplyr::select(month_label, everything(), -month)
  
  # Add Column Summary (Total Row)
  total_row <- tpt_counts %>%
    dplyr::summarise(across(where(is.numeric), sum)) %>%
    dplyr::mutate(month_label = "Total")
  
  tpt_counts_final <- dplyr::bind_rows(tpt_counts, total_row)
  
  # 4. Compute Proportions for "Count (%)" formatting
  tpt_proportions <- tpt_counts_final %>%
    dplyr::mutate(across(-c(month_label, Total), 
                         ~ round(.x / ifelse(Total == 0, 1, Total) * 100, 1)))
  
  # Format as "N (%)"
  tpt_final_table <- tpt_counts_final %>%
    dplyr::mutate(across(
      -c(month_label, Total), 
      ~ paste0(.x, " (", tpt_proportions[[cur_column()]], "%)")
    ))
  
  # 5. Styling
  ft <- tpt_final_table %>%
    flextable::flextable() %>%
    flextable::add_header_lines(values = paste("Table: TPT Outcome Counts by Month (Started >", weeks_lag, "weeks ago)")) %>%
    flextable::set_header_labels(month_label = "Month") %>%
    theme_pearl() %>%
    # Multi-page flow settings
    flextable::add_footer_lines("Table continued on next page...") %>%
    flextable::bold(i = nrow(tpt_final_table), part = "body") %>% 
    # Landscape Optimization
    flextable::width(j = 1, width = 1.2) %>%
    flextable::width(j = -1, width = 1.1) %>%
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}



#' Generate a flextable of monthly treatment proportions (Counts and Row-wise %)
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_treatment_proportions_monthly <- function(data = screening_data) {
  
  # 1. Configuration & Labels
  treatment_labels <- c(
    "SDR"          = "SDR",
    "TPT"          = "TPT",
    "TBRx"         = "TBRx",
    "MDT"          = "MDT",
    "No treatment" = "No treatment"
  )
  
  # 2. Data Aggregation
  treatment_counts <- data %>%
    mutate(
      month = lubridate::floor_date(en_date_visit, "month"),
      calc_any_treatment = coalesce(as.character(calc_any_treatment), "No treatment"),
      calc_any_treatment = factor(calc_any_treatment, 
                                  levels = names(treatment_labels), 
                                  labels = treatment_labels)
    ) %>%
    dplyr::count(month, calc_any_treatment) %>%
    tidyr::pivot_longer(cols = calc_any_treatment, names_to = "Indicator_Key", values_to = "Metric") %>% 
    tidyr::pivot_wider(names_from = Metric, values_from = n, values_fill = 0) %>%
    dplyr::arrange(month)
  
  # 3. Add Calculated Columns
  treatment_counts <- treatment_counts %>%
    dplyr::mutate(
      `Any treatment` = SDR + TPT + TBRx + MDT,
      Total = `Any treatment` + `No treatment`,
      month_label = format(as.Date(month), "%b %Y")
    ) %>%
    dplyr::select(month_label, SDR, TPT, TBRx, MDT, `Any treatment`, `No treatment`, Total)
  
  # Total Row
  total_row <- treatment_counts %>%
    dplyr::summarise(across(where(is.numeric), sum)) %>%
    dplyr::mutate(month_label = "Total")
  
  treatment_counts_final <- dplyr::bind_rows(treatment_counts, total_row)
  
  # 4. Compute Proportions
  treatment_proportions <- treatment_counts_final %>%
    dplyr::mutate(across(-c(month_label, Total), 
                         ~ round(.x / ifelse(Total == 0, 1, Total) * 100, 1)))
  
  treatment_final_table <- treatment_counts_final %>%
    dplyr::mutate(across(
      -c(month_label, Total), 
      ~ paste0(.x, " (", treatment_proportions[[cur_column()]], "%)")
    ))
  
  # 5. Styling
  ft <- treatment_final_table %>%
    flextable::flextable() %>%
    flextable::add_header_lines(values = "Table: Treatment Proportions by Month (Participant Yield)") %>%
    flextable::set_header_labels(month_label = "Month") %>%
    theme_pearl() %>%
    flextable::bg(j = "Any treatment", bg = "#F9F9F9") %>%
    flextable::bold(j = "Any treatment") %>%
    flextable::bold(i = nrow(treatment_final_table), part = "body") %>% 
    # Landscape & Flow
    flextable::width(j = 1, width = 1.2) %>%
    flextable::width(j = -1, width = 1.0) %>%
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}



#' Generate a flextable of TPT patients by age and sex
#' @param data Dataframe. Defaults to treatment_data from the environment
out_tab_tpt_demographics_count <- function(data = treatment_data) {
  
  # 1. Data Preparation
  table_data <- data %>%
    filter(tpt_sex %in% c("M", "F"), !is.na(age_cat)) %>%
    mutate(
      tpt_sex = factor(tpt_sex, levels = c("M", "F")),
      age_cat = factor(age_cat) 
    ) %>%
    count(age_cat, tpt_sex, name = "n") %>%
    tidyr::complete(
      age_cat = factor(levels(data$age_cat), levels = levels(data$age_cat)),
      tpt_sex = factor(c("M", "F"), levels = c("M", "F")),
      fill = list(n = 0)
    ) %>%
    arrange(age_cat, tpt_sex) %>%
    tidyr::pivot_wider(names_from = tpt_sex, values_from = n, values_fill = 0) %>%
    mutate(Total = M + F)
  
  total_row <- table_data %>%
    summarise(
      age_cat = "Total",
      M = sum(M, na.rm = TRUE),
      F = sum(F, na.rm = TRUE),
      Total = sum(Total, na.rm = TRUE)
    )
  
  table_final <- bind_rows(table_data, total_row)
  
  # 3. Styling
  title_text <- paste0("Table: TPT Patients by Age and Sex (Generated ", format(Sys.Date(), "%d %b %Y"), ")")
  
  ft <- table_final %>%
    rename(`Age group` = age_cat) %>%
    flextable::flextable() %>%
    flextable::add_header_lines(values = title_text) %>%
    theme_pearl() %>%
    flextable::bold(i = ~ `Age group` == "Total", part = "body") %>%
    flextable::colformat_int(j = c("M", "F", "Total"), big.mark = ",") %>%
    flextable::width(j = "Age group", width = 1.5) %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a flextable of patients reporting symptoms during TPT by age and sex
#' @param data Dataframe. Defaults to treatment_data from the environment
out_tab_tpt_symptoms_count <- function(data = treatment_data) {
  
  # 1. Data Preparation
  side_effect_data <- data %>%
    filter(
      sx_any_ever == TRUE,
      tpt_sex %in% c("M","F"),
      !is.na(age_cat)
    ) %>%
    mutate(
      tpt_sex = factor(tpt_sex, levels = c("M","F")),
      age_cat = factor(age_cat)
    )
  
  tbl_counts <- side_effect_data %>%
    count(age_cat, tpt_sex, name = "n") %>%
    tidyr::complete(
      age_cat = factor(levels(data$age_cat), levels = levels(data$age_cat)),
      tpt_sex = factor(c("M","F"), levels = c("M","F")),
      fill    = list(n = 0)
    ) %>%
    arrange(age_cat, tpt_sex) %>%
    tidyr::pivot_wider(names_from = tpt_sex, values_from = n, values_fill = 0) %>%
    mutate(Total = M + F)
  
  total_row <- tbl_counts %>%
    summarise(
      age_cat = "Total",
      M       = sum(M, na.rm = TRUE),
      F       = sum(F, na.rm = TRUE),
      Total   = sum(Total, na.rm = TRUE)
    )
  
  tbl_final <- bind_rows(tbl_counts, total_row)
  
  # 4. Styling
  title_text <- paste0("Table: Patients reporting symptoms during TPT by age and sex (", format(Sys.Date(), "%d %b %Y"), ")")
  
  ft <- tbl_final %>%
    rename(`Age group` = age_cat) %>%
    flextable::flextable() %>%
    flextable::add_header_lines(values = title_text) %>%
    theme_pearl() %>%
    flextable::bold(i = ~ `Age group` == "Total", part = "body") %>%
    flextable::width(j = 1, width = 1.5) %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a detailed flextable of TPT symptoms by category and timepoint
#' @param data Dataframe. Defaults to treatment_data from the environment
out_tab_tpt_symptoms_detail <- function(data = treatment_data) {
  
  # Refactored: Relying on logical coercion from upstream pipeline (TRUE = 1, FALSE = 0)
  count_true <- function(x) sum(x, na.rm = TRUE)
  
  # 1. Configuration
  pretty_item <- function(code) {
    dplyr::recode(code,
                  fatigue  = "Fatigue", loa = "Loss of appetite", nv = "Nausea/vomiting",
                  pain = "Abdominal pain", rash = "Rash", jaundice = "Jaundice",
                  flu = "Flu-like symptoms", syncope = "Syncope/presyncope",
                  stpain = "Stomach problems", cough = "Cough", headache = "Headache",
                  dizzy = "Dizziness", regurg = "Regurgitation", const = "Constipation",
                  diar = "Diarrhoea", jtpain = "Joint pain", palp = "Palpitations",
                  sob = "Shortness of breath", sweat = "Sweating/feeling hot",
                  tired = "Feeling tired", weak = "Feeling weak",
                  .default = stringr::str_replace_all(stringr::str_to_sentence(code), "_", " ")
    )
  }
  
  tp <- c("1m", "3m", "4m", "ae")
  groups <- tibble::tribble(
    ~key,       ~label,
    "dili_sx",  "Drug-induced liver injury (DILI)",
    "rhs_sx",   "Rifamycin hypersensitivity (RHS)",
    "csx",      "Common side effects (CSX)"
  )
  
  # 2. Build Category Rows
  group_rows <- purrr::map_dfr(1:nrow(groups), function(i) {
    gkey <- groups$key[i]
    glabel <- groups$label[i]
    
    counts_tp <- purrr::map_int(tp, function(t) {
      col <- paste0(t, "_", gkey, "_any_true")
      if (!col %in% names(data)) return(0L)
      count_true(data[[col]])
    })
    
    ever_cols <- paste0(tp, "_", gkey, "_any_true")
    ever_cols <- ever_cols[ever_cols %in% names(data)]
    ever_count <- if (length(ever_cols) == 0) 0L else {
      mat <- as.matrix(data[, ever_cols])
      sum(rowSums(mat == TRUE, na.rm = TRUE) > 0, na.rm = TRUE)
    }
    
    tibble::tibble(Group = glabel, Item = "Any", `1m` = counts_tp[1], `3m` = counts_tp[2], 
                   `4m` = counts_tp[3], ae = counts_tp[4], ever = ever_count)
  })
  
  # 3. Build Individual Rows
  item_rows <- purrr::map_dfr(1:nrow(groups), function(i) {
    gkey <- groups$key[i]
    glabel <- groups$label[i]
    
    pat <- paste0("^(", paste(tp, collapse="|"), ")_", gkey, "_([a-z0-9_]+)$")
    vars <- grep(pat, names(data), value = TRUE)
    codes <- unique(sub(pat, "\\2", vars))
    codes <- codes[!codes %in% c("none", "any_true")]
    
    purrr::map_dfr(codes, function(code) {
      counts_tp <- purrr::map_int(tp, function(t) {
        col <- paste0(t, "_", gkey, "_", code)
        if (!col %in% names(data)) 0L else count_true(data[[col]])
      })
      
      ever_cols <- paste0(tp, "_", gkey, "_", code)
      ever_cols <- ever_cols[ever_cols %in% names(data)]
      ever_count <- if (!length(ever_cols)) 0L else {
        mat <- as.matrix(data[, ever_cols])
        sum(rowSums(mat == TRUE, na.rm = TRUE) > 0, na.rm = TRUE)
      }
      
      tibble::tibble(Group = glabel, Item = pretty_item(code), `1m` = counts_tp[1], 
                     `3m` = counts_tp[2], `4m` = counts_tp[3], ae = counts_tp[4], ever = ever_count)
    })
  })
  
  # 4. Final Assemblage
  sx_long <- dplyr::bind_rows(group_rows, item_rows) %>%
    dplyr::mutate(Group = factor(Group, levels = groups$label)) %>%
    dplyr::arrange(Group, desc(Item == "Any"), Item)
  
  total_row <- tibble::tibble(
    Group = "Total", Item = "Any side effect",
    `1m` = count_true(data$`1m_any_true`),
    `3m` = count_true(data$`3m_any_true`),
    `4m` = count_true(data$`4m_any_true`),
    ae   = count_true(data$ae_any_true),
    ever = count_true(data$sx_any_ever)
  )
  
  final_df <- dplyr::bind_rows(sx_long, total_row) %>%
    dplyr::rename(`1 month` = `1m`, `3 month` = `3m`, `4 month` = `4m`, `AE form` = ae, `Ever` = ever)
  
  # 5. Styling
  ft <- final_df %>%
    flextable::flextable() %>%
    flextable::add_header_lines(values = paste0("Table: TPT reported symptoms by timepoint — ", format(Sys.Date(), "%d %b %Y"))) %>%
    theme_pearl() %>%
    flextable::fontsize(size = 9, part = "body") %>%
    flextable::bold(i = ~ Item == "Any" | Group == "Total") %>%
    flextable::merge_v(j = "Group") %>%
    flextable::valign(j = "Group", valign = "top") %>%
    flextable::width(j = 1:2, width = 2) %>%
    flextable::autofit()
  
  return(ft)
}


#' Generate a summary flextable of TPT routine monitoring (1, 3, 4 months)
#' @param data Dataframe. Defaults to treatment_data from the environment
out_tab_tpt_monitoring_summary <- function(data = treatment_data) {
  
  # 1. Configuration
  timepoints <- c("1m", "3m", "4m")
  pull_or_na <- function(df, col) { if (col %in% names(df)) df[[col]] else NA }
  
  # 2. Build Summaries
  tp_summaries <- purrr::map_dfr(timepoints, function(tp) {
    any_col   <- paste0("tpt_", tp, "_any_true")
    exp_col   <- paste0("tpt_", tp, "_expected")
    done_col  <- paste0("tpt_", tp, "_done")
    sxres_col <- paste0("tpt_", tp, "_sxres")
    out_col   <- paste0("tpt_", tp, "_outcome")
    
    df_tp <- data %>%
      dplyr::mutate(.expected = as.logical(pull_or_na(data, exp_col)),
                    .done     = as.logical(pull_or_na(data, done_col)),
                    .any_sx   = as.logical(pull_or_na(data, any_col)),
                    .sxres    = as.logical(pull_or_na(data, sxres_col)),
                    .outcome  = as.character(pull_or_na(data, out_col)))
    
    expected_n <- sum(df_tp$.expected == TRUE, na.rm = TRUE)
    done_n     <- sum(df_tp$.done == TRUE, na.rm = TRUE)
    df_done <- df_tp %>% dplyr::filter(.done == TRUE)
    
    any_n      <- sum(df_done$.any_sx == TRUE, na.rm = TRUE)
    no_sx_n    <- sum(df_done$.any_sx == FALSE, na.rm = TRUE)
    resolved_n <- sum(df_done$.any_sx == TRUE & df_done$.sxres == TRUE, na.rm = TRUE)
    not_res_n  <- sum(df_done$.any_sx == TRUE & df_done$.sxres == FALSE, na.rm = TRUE)
    unknown_n  <- sum(df_done$.any_sx == TRUE & is.na(df_done$.sxres), na.rm = TRUE)
    
    complete_n <- sum(df_done$.outcome == "Complete TPT", na.rm = TRUE)
    continue_n <- sum(df_done$.outcome == "Continue TPT", na.rm = TRUE)
    suspend_n  <- sum(df_done$.outcome == "Suspend TPT",  na.rm = TRUE)
    
    tibble::tibble(Group = c(rep("Form status", 2), rep("Side effects", 5), rep("Outcome of review", 3)),
                   Row   = c("Expected", "Done", "Any side effects", "Resolved at assessment", 
                             "Not resolved", "Resolution unknown", "No side effects",
                             "Complete TPT", "Continue TPT", "Suspend TPT"),
                   tp    = tp,
                   n     = c(expected_n, done_n, any_n, resolved_n, not_res_n, unknown_n, no_sx_n,
                             complete_n, continue_n, suspend_n))
  })
  
  # 3. Reshape
  final_df <- tp_summaries %>%
    dplyr::mutate(tp = factor(tp, levels = timepoints)) %>%
    tidyr::pivot_wider(names_from = tp, values_from = n, values_fill = 0) %>%
    dplyr::rename(`1 month` = `1m`, `3 month` = `3m`, `4 month` = `4m`) %>%
    dplyr::mutate(Group = factor(Group, levels = c("Form status", "Side effects", "Outcome of review")),
                  Row   = factor(Row, levels = unique(Row))) %>%
    dplyr::arrange(Group, Row)
  
  # 4. Styling
  ft <- final_df %>%
    flextable::flextable(col_keys = c("Group", "Row", "1 month", "3 month", "4 month")) %>%
    flextable::add_header_lines(values = paste0("Table: TPT Routine Monitoring Summary — ", format(Sys.Date(), "%d %b %Y"))) %>%
    theme_pearl() %>%
    flextable::bold(i = ~ Row %in% c("Done", "Any side effects", "Continue TPT")) %>%
    flextable::merge_v(j = "Group") %>%
    flextable::valign(j = "Group", valign = "top") %>%
    flextable::width(j = 1:2, width = 1.8) %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a flextable comparing TPT outcomes by symptom reporting status
#' @param data Dataframe. Defaults to treatment_data from the environment
#' @param weeks_lag Integer. Number of weeks for cohort cutoff (default 16)
out_tab_tpt_outcomes_by_symptoms <- function(data = treatment_data, weeks_lag = 16) {
  
  # 1. Configuration
  cutoff_date <- Sys.Date() - lubridate::weeks(weeks_lag)
  tpt_outcome_labels <- c("Completed" = "Completed TPT", "Died" = "Died", "Discontinued" = "Discontinued (Medical Reason)", "Lost to follow-up" = "Lost to Follow-up", "Withdrew consent" = "Withdrew Consent", "Not yet assigned" = "Not yet assigned")
  df_cohort <- data %>% dplyr::filter(!is.na(tpt_start_date), tpt_start_date < cutoff_date)
  
  # 2. Build Counts
  base_counts <- df_cohort %>%
    dplyr::mutate(outcome_key = dplyr::coalesce(as.character(tpt_outcome_reason), "Not yet assigned"),
                  outcome_lab = factor(outcome_key, levels = names(tpt_outcome_labels), labels = tpt_outcome_labels),
                  se_status   = if_else(sx_any_ever %in% TRUE, "Reported side effects", "No side effects")) %>%
    dplyr::count(outcome_lab, se_status, name = "n") %>%
    tidyr::complete(outcome_lab, se_status = c("Reported side effects", "No side effects"), fill = list(n = 0))
  
  all_cols <- base_counts %>% dplyr::group_by(outcome_lab) %>% dplyr::summarise(n = sum(n), .groups = "drop") %>% dplyr::mutate(se_status = "All")
  counts_full <- dplyr::bind_rows(base_counts, all_cols) %>%
    dplyr::group_by(se_status) %>% dplyr::group_modify(~ { .x %>% tibble::add_row(outcome_lab = factor("Total"), n = sum(.x$n)) }) %>% dplyr::ungroup()
  
  # 3. Compute Percentages
  tbl_wide <- counts_full %>%
    dplyr::group_by(se_status) %>%
    dplyr::mutate(total_val = n[outcome_lab == "Total"], pct = if_else(total_val > 0, (n / total_val) * 100, 0)) %>%
    dplyr::ungroup() %>% dplyr::select(-total_val) %>%
    tidyr::pivot_wider(id_cols = outcome_lab, names_from = se_status, values_from = c(n, pct), names_vary = "slowest")
  
  # 4. Styling
  ft <- tbl_wide %>%
    flextable::flextable(col_keys = c("outcome_lab", "n_Reported side effects", "pct_Reported side effects", "n_No side effects", "pct_No side effects", "n_All", "pct_All")) %>%
    flextable::add_header_row(values = c("", "Reported side effects", "No side effects", "All Patients"), colwidths = c(1, 2, 2, 2)) %>%
    flextable::add_header_lines(values = "Table: TPT Outcomes by Symptom Reporting Status") %>%
    flextable::set_header_labels(outcome_lab = "Outcome", `n_Reported side effects` = "n", `pct_Reported side effects` = "%", `n_No side effects` = "n", `pct_No side effects` = "%", `n_All` = "n", `pct_All` = "%") %>%
    theme_pearl() %>%
    flextable::colformat_double(j = c(3, 5, 7), digits = 1, suffix = "%") %>%
    flextable::bold(i = ~ outcome_lab == "Total") %>%
    flextable::add_footer_lines(values = paste0("Cohort: Started TPT before ", format(cutoff_date, "%d %b %Y"))) %>%
    flextable::italic(part = "footer") %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a flextable summarizing types of Adverse Events recorded
#' @param data Dataframe. Defaults to treatment_data from the environment
out_tab_ae_type_summary <- function(data = treatment_data) {
  
  is_checked <- function(x) x %in% c(TRUE, 1, "1", "TRUE", "True", "Checked", "Yes", "yes")
  df_ae <- data %>% dplyr::transmute(side_effects = is_checked(tpt_ae_type_ses), illness = is_checked(tpt_ae_type_illness), meds = is_checked(tpt_ae_type_meds), pregnancy = is_checked(tpt_ae_type_preg)) %>% dplyr::mutate(any_flag = side_effects | illness | meds | pregnancy)
  denom_any <- sum(df_ae$any_flag, na.rm = TRUE)
  
  row_counts <- tibble::tibble(Type = c("Side effects", "Illness - new or changed", "Medicines - new or changed", "Pregnancy", "Any reported AE"),
                               n = c(sum(df_ae$side_effects == TRUE, na.rm = TRUE), sum(df_ae$illness == TRUE, na.rm = TRUE), sum(df_ae$meds == TRUE, na.rm = TRUE), sum(df_ae$pregnancy == TRUE, na.rm = TRUE), denom_any)) %>%
    dplyr::mutate(`Percent` = if (denom_any > 0) (n / denom_any) * 100 else 0)
  
  ft <- row_counts %>%
    flextable::flextable() %>%
    flextable::set_header_labels(Type = "AE Classification", n = "n", Percent = "% of Total AEs") %>%
    flextable::add_header_lines(values = "Table: Summary of Recorded Adverse Events (AEs)") %>%
    theme_pearl() %>%
    flextable::bold(i = ~ Type == "Any reported AE", part = "body") %>%
    flextable::colformat_double(j = 3, digits = 1, suffix = "%") %>%
    flextable::add_footer_lines(values = paste0("Data source: AE and Progress forms. Generated: ", format(Sys.Date(), "%d %b %Y"))) %>%
    flextable::italic(part = "footer") %>%
    flextable::width(j = 1, width = 2.5) %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a flextable summarizing the AE profile of patients who discontinued TPT
#' @param data Dataframe. Defaults to treatment_data from the environment
out_tab_tpt_discontinued_ae_profile <- function(data = treatment_data) {
  
  df_disc <- data %>% dplyr::filter(tpt_outcome_reason == "Discontinued")
  total_n <- nrow(df_disc)
  
  flags <- df_disc %>% dplyr::transmute(ses = dplyr::coalesce(tpt_ae_type_ses, FALSE), ill = dplyr::coalesce(tpt_ae_type_illness, FALSE), meds = dplyr::coalesce(tpt_ae_type_meds, FALSE), preg = dplyr::coalesce(tpt_ae_type_preg, FALSE), csx = dplyr::coalesce(csx_any_ever, FALSE), dili = dplyr::coalesce(dili_sx_any_ever, FALSE), rhs = dplyr::coalesce(rhs_sx_any_ever, FALSE)) %>% dplyr::mutate(any_reported = ses | ill | meds | preg | csx | dili | rhs, none_reported = !any_reported)
  
  row_counts <- tibble::tibble(Type = c("Side effects (generic flag)", "Illness - new or changed", "Medicines - new or changed", "Pregnancy", "Common side effects (ever)", "DILI symptoms (ever)", "Rifamycin hypersensitivity (ever)", "Any of the above reported", "None of the above reported", "Total Discontinued Cohort"),
                               n = c(sum(flags$ses), sum(flags$ill), sum(flags$meds), sum(flags$preg), sum(flags$csx), sum(flags$dili), sum(flags$rhs), sum(flags$any_reported), sum(flags$none_reported), total_n)) %>%
    dplyr::mutate(`Percent` = if (total_n > 0) (n / total_n) * 100 else 0)
  
  ft <- row_counts %>%
    flextable::flextable() %>%
    flextable::set_header_labels(Type = "Adverse Event / Symptom Type", n = "n", Percent = "%") %>%
    flextable::add_header_lines(values = "Table: Profile of Adverse Events among Discontinued TPT Patients") %>%
    theme_pearl() %>%
    flextable::bold(i = 8:10, part = "body") %>%
    flextable::colformat_double(j = 3, digits = 1, suffix = "%") %>%
    flextable::add_footer_lines(values = paste0("Cohort: tpt_outcome_reason == 'Discontinued'. Generated: ", format(Sys.Date(), "%d %b %Y"))) %>%
    flextable::italic(part = "footer") %>%
    flextable::width(j = 1, width = 3) %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a flextable of TPT initiation status by clinical risk category (Landscape Optimized)
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_tpt_initiation_by_risk <- function(data = screening_data) {
  
  # 1. Data Prep
  sd_assess <- data %>% dplyr::filter(tst_read_positive == "Positive TST", tb_decision == "Ruled out TB" | ntp_diagnosis == "Ruled out") %>%
    dplyr::mutate(risk_cat = forcats::fct_explicit_na(as.factor(prerx_riskcat), na_level = "(Missing)") |> as.character(), alt_available = dplyr::coalesce(tptrf_alt_result, FALSE) | !is.na(calc_alt_last), eligible = prerx_eligible == "Yes", not_eligible = prerx_eligible == "No", started = dplyr::coalesce(prerx_start, FALSE), not_started = !dplyr::coalesce(prerx_start, FALSE))
  
  risk_levels <- c("High", "Moderate high", "Moderate", "Low", "Not yet known", "(Missing)")
  sd_assess <- sd_assess %>% dplyr::mutate(risk_cat = factor(risk_cat, levels = intersect(risk_levels, unique(c(risk_levels, risk_cat)))))
  
  tbl_init <- sd_assess %>% dplyr::group_by(risk_cat) %>% dplyr::summarise(`ALT_avail` = sum(alt_available, na.rm = TRUE), `ALT_not` = sum(!alt_available, na.rm = TRUE), `Rec_eligible` = sum(eligible, na.rm = TRUE), `Rec_not` = sum(not_eligible, na.rm = TRUE), `Start_TPT` = sum(started, na.rm = TRUE), `Not_started` = sum(not_started, na.rm = TRUE), Total = dplyr::n(), .groups = "drop")
  
  grand_total <- sum(tbl_init$Total, na.rm = TRUE)
  tbl_init <- tbl_init %>% dplyr::mutate(All_n = Total, All_pct = if (grand_total > 0) round(100 * Total / grand_total, 1) else 0)
  grand_row <- tbl_init %>% dplyr::select(-All_pct) %>% dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% dplyr::mutate(risk_cat = "Total", All_pct = 100)
  tbl_final <- dplyr::bind_rows(tbl_init, grand_row) %>% dplyr::mutate(ALT_sp = "", Ass_sp = "", Sta_sp = "") %>% dplyr::select(risk_cat, ALT_avail, ALT_not, ALT_sp, Rec_eligible, Rec_not, Ass_sp, Start_TPT, Not_started, Sta_sp, All_n, All_pct)
  
  # 2. Construction and Styling
  ft <- tbl_final %>%
    flextable::flextable() %>%
    flextable::add_header_row(values = c("", "ALT result", "", "TPT assessment", "", "TPT status", "", "All"), colwidths = c(1, 2, 1, 2, 1, 2, 1, 2)) %>%
    flextable::add_header_lines(values = "Table: TPT Initiation Status by Clinical Risk Category") %>%
    flextable::set_header_labels(risk_cat = "Risk group", ALT_avail = "Available", ALT_not = "Not available", ALT_sp = "", Rec_eligible = "Eligible", Rec_not = "Not eligible", Ass_sp = "", Start_TPT = "Started", Not_started = "Not started", Sta_sp = "", All_n = "n", All_pct = "%") %>%
    theme_pearl() %>%
    flextable::bold(i = ~ risk_cat == "Total") %>%
    flextable::colformat_double(j = "All_pct", digits = 1, suffix = "%") %>%
    # LANDSCAPE OPTIMIZATION
    flextable::width(j = 1, width = 1.5) %>% 
    flextable::width(j = c(2,3,5,6,8,9,11,12), width = 0.85) %>% # Standard data columns
    flextable::width(j = c(4, 7, 10), width = 0.15) %>%          # Skinny spacers
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}


# --- PERFORMANCE & CASCADE TABLES ---------------------------------------------

#' Generate a flextable of quarterly Leprosy referral outcomes
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_lep_referral_outcomes <- function(data = screening_data) {
  
  lep_ref_raw <- data %>% dplyr::filter(lep_refer == TRUE) %>% dplyr::mutate(quarter_start = lubridate::floor_date(en_date_visit, "quarter"), quarter = paste0(lubridate::year(quarter_start), " Q", lubridate::quarter(quarter_start)), nlp_diagnosis = forcats::fct_explicit_na(factor(nlp_diagnosis), na_level = "Missing"))
  if (nrow(lep_ref_raw) > 0) { quarter_seq <- seq.Date(from = min(lep_ref_raw$quarter_start, na.rm = TRUE), to = lubridate::floor_date(Sys.Date(), "quarter"), by = "3 months"); quarter_labels <- paste0(lubridate::year(quarter_seq), " Q", lubridate::quarter(quarter_seq)) } else { quarter_labels <- character(0) }
  
  lep_ref_counts <- lep_ref_raw %>% dplyr::count(quarter, nlp_diagnosis) %>% tidyr::complete(quarter = factor(quarter_labels, levels = quarter_labels, ordered = TRUE), nlp_diagnosis, fill = list(n = 0)) %>% tidyr::pivot_wider(names_from = nlp_diagnosis, values_from = n, values_fill = 0) %>% dplyr::arrange(quarter)
  lep_ref_counts <- lep_ref_counts %>% dplyr::mutate(Total = rowSums(dplyr::select(., -quarter)))
  total_row <- lep_ref_counts %>% dplyr::summarise(across(where(is.numeric), sum)) %>% dplyr::mutate(quarter = "Total")
  lep_ref_counts_final <- dplyr::bind_rows(lep_ref_counts, total_row)
  
  lep_ref_props <- lep_ref_counts_final %>% dplyr::mutate(across(-c(quarter, Total), ~ round(.x / ifelse(Total == 0, 1, Total) * 100, 1)))
  lep_ref_final_table <- lep_ref_counts_final %>% dplyr::mutate(across(-c(quarter, Total), ~ paste0(.x, " (", lep_ref_props[[cur_column()]], "%)")))
  
  latest_q <- if(length(quarter_labels) > 0) max(quarter_labels) else "N/A"
  
  ft <- lep_ref_final_table %>%
    flextable::flextable() %>%
    flextable::add_header_row(
      values = paste("Table: Leprosy Referral Outcomes up to", latest_q), 
      colwidths = ncol(lep_ref_final_table)
    ) %>%
    theme_pearl() %>%
    flextable::bold(i = nrow(lep_ref_final_table), part = "body") %>%
    flextable::width(j = 1, width = 1.5) %>% 
    flextable::width(j = -1, width = 1.6) %>% # j = -1 applies to all other columns
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}



#' Generate a flextable of quarterly TB referral outcomes
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_tb_referral_outcomes <- function(data = screening_data) {
  
  tb_ref_raw <- data %>% dplyr::filter(tb_decision == "Presumptive TB") %>% dplyr::mutate(quarter_start = lubridate::floor_date(en_date_visit, "quarter"), quarter = paste0(lubridate::year(quarter_start), " Q", lubridate::quarter(quarter_start)), ntp_diagnosis = forcats::fct_explicit_na(factor(ntp_diagnosis), na_level = "Missing"))
  if (nrow(tb_ref_raw) > 0) { quarter_seq <- seq.Date(from = min(tb_ref_raw$quarter_start, na.rm = TRUE), to = lubridate::floor_date(Sys.Date(), "quarter"), by = "3 months"); quarter_labels <- paste0(lubridate::year(quarter_seq), " Q", lubridate::quarter(quarter_seq)) } else { quarter_labels <- character(0) }
  
  tb_ref_counts <- tb_ref_raw %>% dplyr::count(quarter, ntp_diagnosis) %>% tidyr::complete(quarter = factor(quarter_labels, levels = quarter_labels, ordered = TRUE), ntp_diagnosis, fill = list(n = 0)) %>% tidyr::pivot_wider(names_from = ntp_diagnosis, values_from = n, values_fill = 0) %>% dplyr::arrange(quarter)
  tb_ref_counts <- tb_ref_counts %>% dplyr::mutate(Total = rowSums(dplyr::select(., -quarter)))
  total_row <- tb_ref_counts %>% dplyr::summarise(across(where(is.numeric), sum)) %>% dplyr::mutate(quarter = "Total")
  tb_ref_counts_final <- dplyr::bind_rows(tb_ref_counts, total_row)
  
  tb_ref_props <- tb_ref_counts_final %>% dplyr::mutate(across(-c(quarter, Total), ~ round(.x / ifelse(Total == 0, 1, Total) * 100, 1)))
  tb_ref_final_table <- tb_ref_counts_final %>% dplyr::mutate(across(-c(quarter, Total), ~ paste0(.x, " (", tb_ref_props[[cur_column()]], "%)"))) %>% dplyr::rename(`Already on DOTS` = `Currently on TB treatment`)
  
  latest_q <- if(length(quarter_labels) > 0) max(quarter_labels) else "N/A"
  ft <- tb_ref_final_table %>%
    flextable::flextable() %>%
    flextable::add_header_row(values = paste("Table: TB Referral Outcomes up to", latest_q), colwidths = ncol(tb_ref_final_table)) %>%
    theme_pearl() %>%
    flextable::bold(i = nrow(tb_ref_final_table), part = "body") %>%
    flextable::width(width = 1.2) %>% flextable::width(j = 1, width = 1.5) %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a flextable of TB screening yield and NNS by age group
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_tb_yield_efficiency <- function(data = screening_data) {
  
  xr_positive <- c("1 cw TB", "2 CAD 50 plus", "3 Uncertain")
  calc_yield_metrics <- function(df) {
    df %>% summarise(N = n(), sx_cases = sum(calc_sx == "Sx Positive" & ntp_diagnosis == "Confirmed", na.rm = TRUE), xr_cases = sum(calc_xr %in% xr_positive & ntp_diagnosis == "Confirmed", na.rm = TRUE), pearl_cases = sum(ntp_diagnosis == "Confirmed", na.rm = TRUE)) %>%
      mutate(Symptom_Yield = (sx_cases / N) * 100, Symptom_NNS = ifelse(sx_cases > 0, N / sx_cases, NA), Xray_Yield = (xr_cases / N) * 100, Xray_NNS = ifelse(xr_cases > 0, N / xr_cases, NA), PEARL_Yield = (pearl_cases / N) * 100, PEARL_NNS = ifelse(pearl_cases > 0, N / pearl_cases, NA))
  }
  
  base_data <- data %>% filter(!is.na(age_cat), tb_decision %in% c("Presumptive TB", "Ruled out TB"))
  yield_by_age <- base_data %>% group_by(age_cat) %>% calc_yield_metrics() %>% ungroup()
  yield_10plus <- base_data %>% filter(age_cat != "0-9") %>% calc_yield_metrics() %>% mutate(age_cat = "Ages 10+")
  yield_total <- base_data %>% calc_yield_metrics() %>% mutate(age_cat = "All")
  yield_table_final <- bind_rows(yield_by_age, yield_10plus, yield_total) %>% select(age_cat, N, sx_cases, Symptom_Yield, Symptom_NNS, xr_cases, Xray_Yield, Xray_NNS, pearl_cases, PEARL_Yield, PEARL_NNS)
  
  ft <- flextable::flextable(yield_table_final) %>%
    flextable::add_header_row(values = c("", "", "Symptom Only", "X-ray (10+)", "PEARL Cumulative"), colwidths = c(1, 1, 3, 3, 3)) %>%
    flextable::add_header_lines(values = "Table: TB Screening Yield and Number Needed to Screen (NNS) by Age") %>%
    flextable::set_header_labels(age_cat = "Age Group", N = "N", sx_cases = "Cases", Symptom_Yield = "Yield (%)", Symptom_NNS = "NNS", xr_cases = "Cases", Xray_Yield = "Yield (%)", Xray_NNS = "NNS", pearl_cases = "Cases", PEARL_Yield = "Yield (%)", PEARL_NNS = "NNS") %>%
    theme_pearl() %>%
    flextable::bold(i = (nrow(yield_table_final)-1):nrow(yield_table_final)) %>%
    flextable::colformat_double(j = c("Symptom_Yield", "Xray_Yield", "PEARL_Yield"), digits = 2) %>%
    flextable::colformat_double(j = c("Symptom_NNS", "Xray_NNS", "PEARL_NNS"), digits = 1) %>%
    flextable::width(j = 1, width = 1.2) %>%
    flextable::width(j = 2, width = 0.6) %>%
    flextable::width(j = 3:11, width = 0.8) %>%
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}



#' Generate a flextable of the Sputum and GeneXpert diagnostic cascade
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_sputum_cascade <- function(data = screening_data) {
  
  xpert_any <- c("Not detected", "Trace detected", "Detected"); xpert_positive <- c("Trace detected", "Detected")
  calc_sputum_counts <- function(df) { df %>% summarise(n_registered = n(), sputum_collected = sum(spuxpt_taken == "Yes", na.rm = TRUE), sputum_tested = sum(spuxpt_labreq_lab, na.rm = TRUE), xpert_result = sum(spuxpt_mtb_res_lab %in% xpert_any, na.rm = TRUE), xpert_positive = sum(spuxpt_mtb_res_lab %in% xpert_positive, na.rm = TRUE)) }
  
  base_data <- data %>% filter(!is.na(age_cat))
  cascade_by_age <- base_data %>% group_by(age_cat) %>% calc_sputum_counts() %>% ungroup()
  cascade_10plus <- base_data %>% filter(age_cat != "0-9") %>% calc_sputum_counts() %>% mutate(age_cat = "Ages 10+")
  cascade_total <- base_data %>% calc_sputum_counts() %>% mutate(age_cat = "All")
  sputum_table <- bind_rows(cascade_by_age, cascade_10plus, cascade_total)
  total_indices <- which(sputum_table$age_cat %in% c("Ages 10+", "All"))
  
  ft <- sputum_table %>%
    flextable::flextable() %>%
    flextable::set_header_labels(age_cat = "Age Group", n_registered = "Registered", sputum_collected = "Collected", sputum_tested = "Tested", xpert_result = "Resulted", xpert_positive = "Positive") %>%
    flextable::add_header_row(values = "Table: Sputum and GeneXpert Laboratory Cascade by Age", colwidths = ncol(sputum_table)) %>%
    theme_pearl() %>%
    flextable::bold(i = total_indices, part = "body") %>%
    flextable::width(j = 1, width = 1.5) %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a detailed flextable of TB yield (Presumptive & Confirmed) by age and sex (Landscape Optimized)
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_tb_yield_demographics_table <- function(data = screening_data) {
  
  tb_summary_wide <- data %>% filter(!is.na(age_cat), en_sex %in% c("M", "F")) %>%
    mutate(presumptive_tb = tb_decision == "Presumptive TB", confirmed_tb = ntp_diagnosis == "Confirmed", valid_tb_outcome = tb_decision %in% c("Presumptive TB", "Ruled out TB")) %>%
    group_by(age_cat, en_sex) %>%
    summarise(presumptive_n = sum(presumptive_tb, na.rm = TRUE), confirmed_n = sum(confirmed_tb, na.rm = TRUE), valid_outcome_n = sum(valid_tb_outcome, na.rm = TRUE), .groups = "drop") %>%
    mutate(percent_presumptive = round(100 * presumptive_n / valid_outcome_n, 1), percent_confirmed = round(100 * confirmed_n / valid_outcome_n, 1)) %>%
    tidyr::pivot_wider(names_from = en_sex, values_from = c(presumptive_n, confirmed_n, valid_outcome_n, percent_presumptive, percent_confirmed), names_glue = "{en_sex}_{.value}") %>% arrange(age_cat)
  
  col_keys <- c("age_cat", "M_valid_outcome_n", "M_presumptive_n", "M_percent_presumptive", "M_confirmed_n", "M_percent_confirmed", "F_valid_outcome_n", "F_presumptive_n", "F_percent_presumptive", "F_confirmed_n", "F_percent_confirmed")
  header_labels <- c(age_cat = "Age Group", M_valid_outcome_n = "N", M_presumptive_n = "n", M_percent_presumptive = "%", M_confirmed_n = "n", M_percent_confirmed = "%", F_valid_outcome_n = "N", F_presumptive_n = "n", F_percent_presumptive = "%", F_confirmed_n = "n", F_percent_confirmed = "%")
  
  ft <- flextable::flextable(tb_summary_wide, col_keys = col_keys) %>%
    flextable::set_header_labels(values = header_labels) %>%
    flextable::add_header_row(values = c("Age Group", "Evaluated", "Screened Positive", "Confirmed TB", "Evaluated", "Screened Positive", "Confirmed TB"), colwidths = c(1, 1, 2, 2, 1, 2, 2)) %>%
    flextable::add_header_row(values = c("Age Group", "Male", "Female"), colwidths = c(1, 5, 5)) %>%
    flextable::add_header_lines(values = "Table: TB Screening Yield (Presumptive & Confirmed) by Age and Sex") %>%
    theme_pearl() %>%
    flextable::merge_v(part = "header") %>%
    flextable::colformat_num(j = -1, digits = 1, na_str = "–") %>%
    # Landscape optimization
    flextable::width(j = 1, width = 1.2) %>%
    flextable::width(j = -1, width = 0.75) %>%
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}





# --- CLINICAL & DIAGNOSTIC TABLES ---------------------------------------------

#' Generate a flextable of Scabies prevalence by age group and sex
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_scabies_prevalence_demographics <- function(data = screening_data) {
  
  scabies_data <- data %>% filter(!is.na(lep_scabies), en_sex %in% c("M", "F")) %>% mutate(age_cat = forcats::fct_explicit_na(factor(age_cat), na_level = "Missing"))
  scabies_wide <- scabies_data %>% group_by(age_cat, en_sex) %>% summarise(prop = round(mean(lep_scabies == TRUE, na.rm = TRUE) * 100, 1), .groups = "drop") %>% tidyr::pivot_wider(names_from = en_sex, values_from = prop)
  scabies_by_age <- scabies_data %>% group_by(age_cat) %>% summarise(Overall = round(mean(lep_scabies == TRUE, na.rm = TRUE) * 100, 1), .groups = "drop")
  scabies_by_sex <- scabies_data %>% group_by(en_sex) %>% summarise(prop = round(mean(lep_scabies == TRUE, na.rm = TRUE) * 100, 1), .groups = "drop")
  grand_total <- round(mean(scabies_data$lep_scabies == TRUE, na.rm = TRUE) * 100, 1)
  
  scabies_final <- scabies_wide %>% left_join(scabies_by_age, by = "age_cat") %>% arrange(age_cat) %>% bind_rows(tibble(age_cat = "Overall", M = scabies_by_sex$prop[scabies_by_sex$en_sex == "M"], F = scabies_by_sex$prop[scabies_by_sex$en_sex == "F"], Overall = grand_total))
  
  ft <- scabies_final %>%
    flextable::flextable() %>%
    flextable::set_header_labels(age_cat = "Age Category", M = "Male (%)", F = "Female (%)", Overall = "Total (%)") %>%
    flextable::add_header_lines(values = "Table: Scabies Prevalence by Age Group and Sex (Evaluable Participants)") %>%
    theme_pearl() %>%
    flextable::bold(i = nrow(scabies_final), part = "body") %>%
    flextable::bold(j = "Overall", part = "all") %>% 
    flextable::width(j = 1, width = 1.8) %>%
    flextable::autofit()
  
  return(ft)
}



#' Generate a detailed flextable of TST positivity by age and sex (Landscape Optimized)
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_tst_yield_demographics_table <- function(data = screening_data) {
  
  tst_summary_wide <- data %>% filter(!is.na(age_cat), en_sex %in% c("M", "F"), !is.na(tst_read_bin)) %>%
    mutate(tst_positive = tst_read_positive == "Positive TST", valid_tst    = tst_read_bin == TRUE) %>%
    group_by(age_cat, en_sex) %>% summarise(positive_n = sum(tst_positive & valid_tst, na.rm = TRUE), valid_n    = sum(valid_tst, na.rm = TRUE), .groups = "drop") %>%
    mutate(percent_positive = round(100 * positive_n / valid_n, 1)) %>%
    tidyr::pivot_wider(names_from = en_sex, values_from = c(positive_n, percent_positive, valid_n), names_glue = "{en_sex}_{.value}") %>% arrange(age_cat)
  
  col_keys <- c("age_cat", "M_positive_n", "M_percent_positive", "M_valid_n", "F_positive_n", "F_percent_positive", "F_valid_n")
  header_labels <- c(age_cat = "Age Group", M_positive_n = "n", M_percent_positive = "%", M_valid_n = "Valid N", F_positive_n = "n", F_percent_positive = "%", F_valid_n = "Valid N")
  
  ft <- flextable::flextable(tst_summary_wide, col_keys = col_keys) %>%
    flextable::set_header_labels(values = header_labels) %>%
    flextable::add_header_row(values = c("Age Group", "Positive TST", "Positive TST", "Total Evaluated", "Positive TST", "Positive TST", "Total Evaluated"), colwidths = c(1, 1, 1, 1, 1, 1, 1)) %>%
    flextable::add_header_row(values = c("Age Group", "Male", "Female"), colwidths = c(1, 3, 3)) %>%
    flextable::add_header_lines(values = "Table: TST Positivity Prevalence by Age and Sex") %>%
    theme_pearl() %>%
    flextable::merge_v(part = "header") %>%
    # Landscape optimization
    flextable::width(j = 1, width = 1.2) %>%
    flextable::width(j = -1, width = 1.2) %>%
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}



#' Generate a detailed flextable of Leprosy yield (Presumptive & Confirmed) by age and sex (Landscape Optimized)
#' @param data Dataframe. Defaults to screening_data from the environment
out_tab_lep_yield_demographics_table <- function(data = screening_data) {
  
  lep_summary_wide <- data %>% filter(!is.na(age_cat), en_sex %in% c("M", "F"), !is.na(lep_refer)) %>%
    mutate(presumptive_lep = lep_refer == TRUE, confirmed_lep   = nlp_diagnosis == "Confirmed", valid_lep       = !is.na(lep_refer)) %>%
    group_by(age_cat, en_sex) %>% summarise(presumptive_n   = sum(presumptive_lep, na.rm = TRUE), confirmed_n     = sum(confirmed_lep, na.rm = TRUE), valid_n         = sum(valid_lep, na.rm = TRUE), .groups = "drop") %>%
    mutate(percent_presumptive = round(100 * presumptive_n / valid_n, 1), percent_confirmed   = round(100 * confirmed_n / valid_n, 1)) %>%
    tidyr::pivot_wider(names_from = en_sex, values_from = c(presumptive_n, confirmed_n, valid_n, percent_presumptive, percent_confirmed), names_glue = "{en_sex}_{.value}") %>% arrange(age_cat)
  
  col_keys <- c("age_cat", "M_presumptive_n", "M_percent_presumptive", "M_confirmed_n", "M_percent_confirmed", "M_valid_n", "F_presumptive_n", "F_percent_presumptive", "F_confirmed_n", "F_percent_confirmed", "F_valid_n")
  header_labels <- c(age_cat = "Age Group", M_presumptive_n = "n", M_percent_positive = "%", M_confirmed_n = "n", M_percent_confirmed = "%", M_valid_n = "N", F_presumptive_n = "n", F_percent_presumptive = "%", F_confirmed_n = "n", F_percent_confirmed = "%", F_valid_n = "N")
  
  ft <- flextable::flextable(lep_summary_wide, col_keys = col_keys) %>%
    flextable::set_header_labels(values = header_labels) %>%
    flextable::add_header_row(values = c("Age Group", "Presumptive", "Confirmed", "Total", "Presumptive", "Confirmed", "Total"), colwidths = c(1, 2, 2, 1, 2, 2, 1)) %>%
    flextable::add_header_row(values = c("Age Group", "Male", "Female"), colwidths = c(1, 5, 5)) %>%
    flextable::add_header_lines(values = "Table: Leprosy Screening Yield (Presumptive & Confirmed) by Age and Sex") %>%
    theme_pearl() %>%
    flextable::merge_v(part = "header") %>%
    # Landscape optimization
    flextable::width(j = 1, width = 1.2) %>%
    flextable::width(j = -1, width = 0.75) %>%
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}