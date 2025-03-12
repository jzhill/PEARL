library(tidyverse)
library(ggplot2)
library(shiny)
library(lubridate)

ui <- fluidPage(
  titlePanel("Interactive Time Series Visualization"),
  fluidRow(
    column(4, wellPanel(
      selectInput("time_agg", "Select Time Aggregation:",
                  choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month", 
                              "Quarterly" = "quarter", "Yearly" = "year"),
                  selected = "day"),
      dateRangeInput("date_range", "Select Date Range:"),
      checkboxGroupInput("indicators", "Select Indicators:",
                         choices = c("Total Registrations" = "reg", 
                                     "TST Placed" = "tst_placed", 
                                     "TST Read" = "tst_read"),
                         selected = "reg")
    ))
  ),
  fluidRow(
    column(12, plotOutput("time_plot"))
  )
)

server <- function(input, output, session) {
  req(daily_data)
  
  observe({
    updateDateRangeInput(session, "date_range", 
                         start = min(daily_data$en_date_visit, na.rm = TRUE),
                         end = max(daily_data$en_date_visit, na.rm = TRUE),
                         min = min(daily_data$en_date_visit, na.rm = TRUE),
                         max = max(daily_data$en_date_visit, na.rm = TRUE))
  })
  
  filtered_data <- reactive({
    req(input$indicators)
    daily_data %>%
      filter(en_date_visit >= input$date_range[1] & en_date_visit <= input$date_range[2]) %>%
      mutate(time_group = floor_date(en_date_visit, input$time_agg)) %>%
      group_by(time_group) %>%
      summarise(across(all_of(input$indicators), ~sum(replace_na(.x, 0))), .groups = "drop") %>%
      pivot_longer(-time_group, names_to = "Indicator", values_to = "Value") %>%
      filter(Value > 0)  # Remove zero-value indicators to avoid plotting issues
  })
  
  output$time_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    ggplot(filtered_data(), aes(x = time_group, y = Value, color = Indicator, group = Indicator)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Aggregated Time Series Data", x = "Time", y = "Count")
  })
}

shinyApp(ui, server)
