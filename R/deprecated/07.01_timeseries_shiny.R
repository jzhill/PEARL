library(tidyverse)
library(ggplot2)
library(shiny)
library(lubridate)

ui <- fluidPage(
  titlePanel("Interactive Time Series Visualization"),
  fluidRow(
    column(4, wellPanel(
      selectInput("time_agg", "Select Time Aggregation:",
                  choices = c("Weekly" = "week", "Monthly" = "month", 
                              "Quarterly" = "quarter", "Yearly" = "year"),
                  selected = "week"),
      dateRangeInput("date_range", "Select Date Range:"),
      checkboxGroupInput("indicators", "Select Indicators:",
                         choices = c("Total Registrations" = "reg", 
                                     "TST Placed" = "tst_placed", 
                                     "TST Read" = "tst_read", 
                                     "TST Read as % of TST Placed" = "tst_read_pct",
                                     "TB Decision as % of Registrations" = "tbdec_pct",
                                     "SDR Given as % of Registrations" = "sdr_pct"),
                         selected = "reg")
    ))
  ),
  fluidRow(
    column(12, plotOutput("time_plot"))
  )
)

server <- function(input, output, session) {
  req(weekly_data)
  
  observe({
    updateDateRangeInput(session, "date_range", 
                         start = min(weekly_data$week_reg, na.rm = TRUE),
                         end = max(weekly_data$week_reg, na.rm = TRUE),
                         min = min(weekly_data$week_reg, na.rm = TRUE),
                         max = max(weekly_data$week_reg, na.rm = TRUE))
  })
  
  filtered_data <- reactive({
    req(input$indicators)
    
    df <- weekly_data %>%
      filter(week_reg >= input$date_range[1] & week_reg <= input$date_range[2]) %>%
      mutate(time_group = case_when(
        input$time_agg == "week" ~ week_reg,
        input$time_agg == "month" ~ floor_date(week_reg, "month"),
        input$time_agg == "quarter" ~ floor_date(week_reg, "quarter"),
        input$time_agg == "year" ~ floor_date(week_reg, "year"),
        TRUE ~ week_reg  # Default to weekly if input is invalid
      )) %>%
      group_by(time_group) %>%
      summarise(
        across(all_of(setdiff(input$indicators, c("tst_read_pct", "tbdec_pct", "sdr_pct"))), ~sum(replace_na(.x, 0))),
        tst_read_pct = if("tst_read_pct" %in% input$indicators) ifelse(sum(tst_placed, na.rm = TRUE) > 0, sum(tst_read, na.rm = TRUE) / sum(tst_placed, na.rm = TRUE) * 100, NA_real_) else NA_real_,
        tbdec_pct = if("tbdec_pct" %in% input$indicators) ifelse(sum(reg, na.rm = TRUE) > 0, sum(tbdec, na.rm = TRUE) / sum(reg, na.rm = TRUE) * 100, NA_real_) else NA_real_,
        sdr_pct = if("sdr_pct" %in% input$indicators) ifelse(sum(reg, na.rm = TRUE) > 0, sum(sdr, na.rm = TRUE) / sum(reg, na.rm = TRUE) * 100, NA_real_) else NA_real_,
        .groups = "drop"
      )
    
    df %>%
      pivot_longer(-time_group, names_to = "Indicator", values_to = "Value") %>%
      filter(!is.na(Value)) 
  })
  
  output$time_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    ggplot(filtered_data(), aes(x = time_group, y = Value, color = Indicator, group = Indicator)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Aggregated Time Series Data", x = "Time", y = "Count or Percentage")
  })
}

shinyApp(ui, server)
