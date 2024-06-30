library(shiny)
library(highcharter)
library(tidyverse)

# Load the dataset
data <- read_csv("extended_global_market_data_v2.csv")

# Define UI for application
ui <- fluidPage(
  titlePanel("Global Market Data Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(data$Country)),
      selectInput("variable", "Select Variable:", choices = colnames(data)[3:33]),
      dateRangeInput("dateRange", "Select Date Range:", start = min(data$Date), end = max(data$Date)),
      selectInput("chartType", "Select Chart Type:", choices = c("Line", "Bar", "Scatter", "Pie"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", highchartOutput("overviewChart")),
        tabPanel("Trends", highchartOutput("trendsChart")),
        tabPanel("Comparisons", highchartOutput("comparisonsChart"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(Country == input$country, 
             Date >= input$dateRange[1], 
             Date <= input$dateRange[2])
  })
  
  output$overviewChart <- renderHighchart({
    chart <- highchart() %>%
      hc_title(text = paste("Overview of", input$variable, "in", input$country)) %>%
      hc_xAxis(categories = filtered_data()$Date)
    
    if (input$chartType == "Line") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = filtered_data()[[input$variable]], type = "line")
    } else if (input$chartType == "Bar") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = filtered_data()[[input$variable]], type = "column")
    } else if (input$chartType == "Scatter") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = filtered_data()[[input$variable]], type = "scatter")
    } else if (input$chartType == "Pie") {
      chart_data <- filtered_data() %>%
        group_by(Date) %>%
        summarize(total = sum(get(input$variable), na.rm = TRUE))
      
      chart <- chart %>%
        hc_add_series(name = input$variable, data = chart_data$total, type = "pie")
    }
    
    chart
  })
  
  output$trendsChart <- renderHighchart({
    chart <- highchart() %>%
      hc_title(text = paste("Trends of", input$variable, "in", input$country)) %>%
      hc_xAxis(categories = filtered_data()$Date)
    
    if (input$chartType == "Line") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = filtered_data()[[input$variable]], type = "line")
    } else if (input$chartType == "Bar") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = filtered_data()[[input$variable]], type = "column")
    } else if (input$chartType == "Scatter") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = filtered_data()[[input$variable]], type = "scatter")
    }
    
    chart
  })
  
  output$comparisonsChart <- renderHighchart({
    comparison_data <- data %>%
      filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
      group_by(Country) %>%
      summarize(mean_value = mean(get(input$variable), na.rm = TRUE))
    
    chart <- highchart() %>%
      hc_title(text = "Comparisons of Selected Variable Across Countries") %>%
      hc_xAxis(categories = comparison_data$Country)
    
    if (input$chartType == "Line") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = comparison_data$mean_value, type = "line")
    } else if (input$chartType == "Bar") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = comparison_data$mean_value, type = "column")
    } else if (input$chartType == "Scatter") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = comparison_data$mean_value, type = "scatter")
    } else if (input$chartType == "Pie") {
      chart <- chart %>%
        hc_add_series(name = input$variable, data = comparison_data$mean_value, type = "pie")
    }
    
    chart
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
