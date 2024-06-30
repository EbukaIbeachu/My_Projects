# Load necessary libraries
library(ggplot2)
library(highcharter)
library(forecast)
library(reshape2)
library(cluster)
library(rmarkdown)
library(xaringan)
library(dplyr)
library(shiny)
library(rsconnect)

# Load the dataset
data <- read.csv("extended_global_market_data_v2.csv")
data$Date <- as.Date(data$Date)
View(data)

# Summary Statistics
summary(data)

# Standard Deviation for each numeric column
sapply(data[, sapply(data, is.numeric)], sd)

# Correlation Matrix
cor_matrix <- cor(data[, sapply(data, is.numeric)])
print(cor_matrix)

# Visualize the correlation matrix with highcharter
melted_cor <- melt(cor_matrix)
hchart(melted_cor, "heatmap", hcaes(x = Var1, y = Var2, value = value)) %>%
  hc_title(text = "Correlation Heatmap") %>%
  hc_xAxis(title = list(text = "Variables")) %>%
  hc_yAxis(title = list(text = "Variables")) %>%
  hc_colorAxis(stops = color_stops())

# Simple Linear Regression of Stock Index on GDP Growth Rate
model <- lm(Stock.Index ~ GDP.Growth.Rate, data = data)
summary(model)

# Plot the regression line with highcharter
hchart(data, "scatter", hcaes(x = GDP.Growth.Rate, y = Stock.Index)) %>%
  hc_add_series(data = data.frame(x = data$GDP.Growth.Rate, y = fitted(model)), type = "line", hcaes(x = x, y = y), name = "Fitted Line") %>%
  hc_title(text = "Linear Regression: Stock Index vs GDP Growth Rate") %>%
  hc_xAxis(title = list(text = "GDP Growth Rate")) %>%
  hc_yAxis(title = list(text = "Stock Index"))

# Multiple Linear Regression of Stock Index on multiple variables
model <- lm(Stock.Index ~ GDP.Growth.Rate + Inflation.Rate + Exchange.Rate + Commodity.Price, data = data)
summary(model)

# Decompose the time series of Stock Index
ts_data <- ts(data$Stock.Index, frequency = 365)
decomposed <- stl(ts_data, s.window = "periodic")
decomposed_data <- as.data.frame(decomposed$time.series)
decomposed_data$Date <- data$Date
hchart(decomposed_data, "line", hcaes(x = Date, y = seasonal)) %>%
  hc_title(text = "Seasonal Component of Stock Index") %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Seasonal Component"))

# ARIMA modeling for Stock Index
fit <- auto.arima(ts_data)
summary(fit)
forecasted <- forecast(fit, h = 30)
forecast_df <- data.frame(Date = seq(max(data$Date) + 1, by = "day", length.out = 30), Forecast = forecasted$mean)
hchart(forecast_df, "line", hcaes(x = Date, y = Forecast)) %>%
  hc_title(text = "30-Day Forecast of Stock Index") %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Forecasted Stock Index"))

# t-Test to compare GDP Growth Rates between two countries
country1 <- subset(data, Country == "USA")
country2 <- subset(data, Country == "China")
t_test_result <- t.test(country1$GDP.Growth.Rate, country2$GDP.Growth.Rate)
print(t_test_result)

# ANOVA to test differences in Stock Index across countries
anova_result <- aov(Stock.Index ~ Country, data = data)
summary(anova_result)

# PCA on selected numeric columns
pca <- prcomp(data[, c("Stock.Index", "Commodity.Price", "Exchange.Rate", "GDP.Growth.Rate", "Inflation.Rate")], center = TRUE, scale = TRUE)
summary(pca)

# Scree plot to visualize the explained variance
screeplot(pca, type = "lines")

# PCA on selected numeric columns
pca <- prcomp(data[, c("Stock.Index", "Commodity.Price", "Exchange.Rate", "GDP.Growth.Rate", "Inflation.Rate")], center = TRUE, scale. = TRUE)
summary(pca)

# Scree plot to visualize the explained variance
screeplot(pca, type = "lines")

# Biplot to visualize PCA results
pca_data <- as.data.frame(pca$x)
pca_data$Country <- data$Country
hchart(pca_data, "scatter", hcaes(x = PC1, y = PC2, group = Country)) %>%
  hc_title(text = "PCA of Market Data") %>%
  hc_xAxis(title = list(text = "Principal Component 1")) %>%
  hc_yAxis(title = list(text = "Principal Component 2"))

# K-means clustering
data_scaled <- scale(data[, c("Stock.Index", "Commodity.Price", "Exchange.Rate", "GDP.Growth.Rate", "Inflation.Rate")])
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data_scaled, centers = 3)
data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize clustering results with highcharter
hchart(data, "scatter", hcaes(x = Stock.Index, y = GDP.Growth.Rate, group = Cluster)) %>%
  hc_title(text = "Clustering of Countries Based on Market Data") %>%
  hc_xAxis(title = list(text = "Stock Index")) %>%
  hc_yAxis(title = list(text = "GDP Growth Rate"))

# ARIMA forecasting
fit <- auto.arima(ts_data)
forecasted <- forecast(fit, h = 30)
forecast_df <- data.frame(Date = seq(max(data$Date) + 1, by = "day", length.out = 30), Forecast = forecasted$mean)
hchart(forecast_df, "line", hcaes(x = Date, y = Forecast)) %>%
  hc_title(text = "30-Day Forecast of Stock Index") %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Forecasted Stock Index"))

# Distribution Analysis with highcharter
hchart(data, "histogram", hcaes(x = Stock.Index)) %>%
  hc_title(text = "Distribution of Stock Index") %>%
  hc_xAxis(title = list(text = "Stock Index")) %>%
  hc_yAxis(title = list(text = "Frequency"))


# Trend Analysis with highcharter
hchart(data, "line", hcaes(x = Date, y = Stock.Index, group = Country)) %>%
  hc_title(text = "Trend of Stock Index Over Time") %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Stock Index"))


# GDP and Inflation Analysis with highcharter
hchart(data, "scatter", hcaes(x = GDP.Growth.Rate, y = Inflation.Rate, group = Country)) %>%
  hc_title(text = "GDP Growth Rate vs Inflation Rate") %>%
  hc_xAxis(title = list(text = "GDP Growth Rate")) %>%
  hc_yAxis(title = list(text = "Inflation Rate"))


# Exchange Rate Impact with highcharter
hchart(data, "scatter", hcaes(x = Exchange.Rate, y = Stock.Index, group = Country)) %>%
  hc_title(text = "Impact of Exchange Rate on Stock Index") %>%
  hc_xAxis(title = list(text = "Exchange Rate")) %>%
  hc_yAxis(title = list(text = "Stock Index"))

ui <- fluidPage(
  titlePanel("Global Market Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:", choices = unique(data$Country)),
      dateRangeInput("dates", "Date range:", start = min(data$Date), end = max(data$Date))
    ),
    mainPanel(
      highchartOutput("stockPlot"),
      highchartOutput("gdpPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(Country == input$country & Date >= input$dates[1] & Date <= input$dates[2])
  })
  
  output$stockPlot <- renderHighchart({
    hchart(filtered_data(), "line", hcaes(x = Date, y = Stock.Index)) %>%
      hc_title(text = paste("Stock Index for", input$country)) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Stock Index"))
  })
  
  output$gdpPlot <- renderHighchart({
    hchart(filtered_data(), "line", hcaes(x = Date, y = GDP.Growth.Rate)) %>%
      hc_title(text = paste("GDP Growth Rate for", input$country)) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "GDP Growth Rate"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

