# Install necessary packages
install.packages("jsonlite")
install.packages("dplyr")
install.packages("ggplot2")

# Load libraries
library(jsonlite)
library(dplyr)
library(ggplot2)

# Load JSON data
block_data <- fromJSON("/mnt/data/block_info.json")

# Explore the data
str(block_data)

# Extract relevant information
block_info <- block_data$result
transactions <- block_info$transactions

# Convert to Data Frame
transactions_df <- as.data.frame(transactions)

# Summary Statistics
summary(transactions_df)

# Transaction Count by Address
from_counts <- transactions_df %>% count(from, sort = TRUE)
print(from_counts)

to_counts <- transactions_df %>% count(to, sort = TRUE)
print(to_counts)

# Gas Price Analysis
transactions_df$gasPrice <- as.numeric(transactions_df$gasPrice)
summary(transactions_df$gasPrice)

ggplot(transactions_df, aes(x = gasPrice)) +
  geom_histogram(binwidth = 1e9, fill = "blue", color = "black") +
  labs(title = "Distribution of Gas Prices",
       x = "Gas Price (in Wei)",
       y = "Frequency") +
  theme_minimal()

# Transaction Values Analysis
transactions_df$value <- as.numeric(transactions_df$value)
summary(transactions_df$value)

ggplot(transactions_df, aes(x = value)) +
  geom_histogram(binwidth = 1e18, fill = "green", color = "black") +
  labs(title = "Distribution of Transaction Values",
       x = "Transaction Value (in Wei)",
       y = "Frequency") +
  theme_minimal()
