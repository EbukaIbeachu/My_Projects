library(shiny)
library(highcharter)
library(tidyverse)

# Load the dataset
data <- read_csv("extended_global_market_data_v2.csv") %>%
  mutate(Date = as.Date(Date))  # Ensure Date is in proper format

# Define UI for application
ui <- fluidPage(
  titlePanel("Global Market Data Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", 
                  choices = unique(data$Country), 
                  selected = unique(data$Country)[1]),
      
      selectInput("variable", "Select Variable:", 
                  choices = colnames(data)[3:33], 
                  selected = colnames(data)[3]),
      
      dateRangeInput("dateRange", "Select Date Range:", 
                     start = min(data$Date), end = max(data$Date)),
      
      selectInput("chartType", "Select Chart Type:", 
                  choices = c("Line", "Bar", "Scatter", "Pie"))
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
    validate(
      need(nrow(filtered_data()) > 0, "No data available for selected filters.")
    )
    
    chart <- highchart() %>%
      hc_title(text = paste("Overview of", input$variable, "in", input$country)) %>%
      hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b>") %>%
      hc_add_theme(hc_theme_flat())
    
    if (input$chartType == "Pie") {
      chart_data <- filtered_data() %>%
        group_by(Date) %>%
        summarize(total = sum(get(input$variable), na.rm = TRUE)) %>%
        mutate(name = as.character(Date), y = total)
      
      chart <- chart %>%
        hc_add_series(
          type = "pie",
          name = input$variable,
          data = list_parse2(chart_data)
        )
    } else {
      chart <- chart %>%
        hc_xAxis(categories = as.character(filtered_data()$Date)) %>%
        hc_add_series(name = input$variable, 
                      data = filtered_data()[[input$variable]], 
                      type = tolower(input$chartType))
    }
    
    chart
  })
  
  output$trendsChart <- renderHighchart({
    validate(
      need(nrow(filtered_data()) > 0, "No data available for selected filters.")
    )
    
    chart <- highchart() %>%
      hc_title(text = paste("Trends of", input$variable, "in", input$country)) %>%
      hc_xAxis(categories = as.character(filtered_data()$Date)) %>%
      hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b>") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_add_series(name = input$variable, 
                    data = filtered_data()[[input$variable]], 
                    type = tolower(input$chartType))
    
    chart
  })
  
  output$comparisonsChart <- renderHighchart({
    comparison_data <- data %>%
      filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
      group_by(Country) %>%
      summarize(mean_value = mean(get(input$variable), na.rm = TRUE)) %>%
      ungroup()
    
    validate(
      need(nrow(comparison_data) > 0, "No comparison data available.")
    )
    
    chart <- highchart() %>%
      hc_title(text = "Comparisons of Selected Variable Across Countries") %>%
      hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b>") %>%
      hc_add_theme(hc_theme_flat())
    
    if (input$chartType == "Pie") {
      pie_data <- comparison_data %>%
        mutate(name = Country, y = mean_value)
      
      chart <- chart %>%
        hc_add_series(
          type = "pie",
          name = input$variable,
          data = list_parse2(pie_data)
        )
    } else {
      chart <- chart %>%
        hc_xAxis(categories = comparison_data$Country) %>%
        hc_add_series(name = input$variable, 
                      data = comparison_data$mean_value, 
                      type = tolower(input$chartType))
    }
    
    chart
  })
}

# Run the application 
shinyApp(ui = ui, server = server)