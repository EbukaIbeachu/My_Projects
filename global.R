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
      dateRangeInput("dateRange", "Select Date Range:", start = min(data$Date), end = max(data$Date))
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
    highchart() %>%
      hc_title(text = paste("Overview of", input$variable, "in", input$country)) %>%
      hc_xAxis(categories = filtered_data()$Date) %>%
      hc_add_series(name = input$variable, data = filtered_data()[[input$variable]])
  })
  
  output$trendsChart <- renderHighchart({
    highchart() %>%
      hc_title(text = paste("Trends of", input$variable, "in", input$country)) %>%
      hc_xAxis(categories = filtered_data()$Date) %>%
      hc_add_series(name = input$variable, data = filtered_data()[[input$variable]]) %>%
      hc_add_theme(hc_theme_flat())
  })
  
  output$comparisonsChart <- renderHighchart({
    highchart() %>%
      hc_title(text = "Comparisons of Selected Variable Across Countries") %>%
      hc_xAxis(categories = unique(data$Country)) %>%
      hc_add_series(name = input$variable, data = data %>%
                      filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                      group_by(Country) %>%
                      summarize(mean_value = mean(get(input$variable), na.rm = TRUE)) %>%
                      .$mean_value)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
