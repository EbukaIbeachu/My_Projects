install.packages("jsonlite")
install.packages("dplyr")
install.packages("ggplot2")

library(jsonlite)
library(dplyr)
library(ggplot2)


# Load the JSON data
data <- fromJSON("transactions.json")

# Assuming the data is stored under the 'result' key
transactions <- as.data.frame(data$result)


# Convert timestamps to datetime format
transactions <- transactions %>%
  mutate(timeStamp = as.POSIXct(as.numeric(timeStamp), origin="1970-01-01"))


# Example analysis: summary statistics
# Print summary statistics
print(summary(transactions))


head(data)
View(data)

head(transactions)
View(transactions)
# Plot the number of transactions over time
ggplot(transactions, aes(x = timeStamp)) +
  geom_histogram(binwidth = 86400, color="blue", fill="lightblue") +  # binwidth = 1 day
  labs(title = "Number of Transactions Over Time",
       x = "Time",
       y = "Number of Transactions") +
  theme_minimal()



