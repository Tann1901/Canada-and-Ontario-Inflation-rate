#install package
install.packages("dplyr")
install.packages("readxl")
install.packages("lubridate")
install.packages("skimr")
install.packages("RColorBrewer")

library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)


#setwd() to the right folder
setwd("C:/Users/tanis/Documents/R-project")

#QUESTION 1
###CANADA FOOD INFLATION RATE
#Using DPLYR functions
file_path <- "Food_Can.xlsx"
data_can <- read_excel(file_path)
data_can

#Add inflation rate compared to Janurary 2020
data_can <- data_can %>%
  fill(everything(), .direction = "down") %>%
  mutate(inflation_rate = paste0(formatC(round((Food - first(Food)) * 100 / first(Food),2), digits = 2)))

#Add inflation rate month by month
data_can <- data_can %>%
  fill(everything(), .direction = "down") %>%
  mutate(inflation_rate_by_month = paste0(formatC(round((Food - lag(Food)) * 100 / lag(Food),2), digits = 2)))
data_can


# Convert Date column to a Date format
data_can$Date <- as.Date(data_can$Date)

#For based in January 2020 comparison inflation
#Plot the graph
chart_ca <- ggplot(data_can, aes(x = Date, y = as.numeric(inflation_rate))) +
  geom_line() +
  geom_text(aes(label = paste0(round(as.numeric(inflation_rate), 2), "%")),
            hjust = -0.2, vjust = 0.5, size = 3, color = "darkred") +
  labs(
    title = "Comparison of Food Inflation in Canada (2020-2023)",
    subtitle = "Based on the month of January 2020",
    caption = "Source: Statistics Canada",
    x = "Date",
    y = "Food Inflation Rate (%)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0.02)) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkblue", size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.caption = element_text(face ="italic", hjust=0),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Adjust x-axis text
    axis.title.y = element_text(color = "darkblue", size = 10, face = "bold"),
    panel.background = element_blank(),  # Remove plot background
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),  # Add dashed grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.y = element_text(size = 10)  # Adjust y-axis label font size
  )
# Save the plot as a PNG file
ggsave("chart_ca_based_Jan_2020.png", chart_ca, width = 12, height = 6)

# Print the plot (optional)
print(chart_ca)

#For monthly inflation
#Plot the graph
chart_ca_monthly <- ggplot(data_can, aes(x = Date, y = as.numeric(inflation_rate_by_month))) +
  geom_line() +
  geom_text(aes(label = paste0(round(as.numeric(inflation_rate_by_month), 2), "%")),
            hjust = -0.2, vjust = 0.5, size = 3, color = "darkred") +
  labs(
    title = "Comparison of Food Inflation in Canada",
    subtitle = "Monthly from January 2020 - January 2023",
    caption = "Source: Statistics Canada",
    x = "Date",
    y = "Food Inflation Rate (%)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0.02)) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkblue", size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.caption = element_text(face ="italic", hjust=0),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Adjust x-axis text
    axis.title.y = element_text(color = "darkblue", size = 10, face = "bold"),
    panel.background = element_blank(),  # Remove plot background
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),  # Add dashed grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.y = element_text(size = 10)  # Adjust y-axis label font size
  )
# Save the plot as a PNG file
ggsave("chart_ca_monthly.png", chart_ca_monthly, width = 12, height = 6)

# Print the plot 
print(chart_ca_monthly)

###ONTARIO FOOD INFLATION RATE 
#Using DPLYR functions
file_path <- "Food_To.xlsx"
data_on <- read_excel(file_path)
data_on

#Add inflation rate compared to Janurary 2020
data_on <- data_on %>%
  fill(everything(), .direction = "down") %>%
  mutate(inflation_rate = paste0(formatC(round((Food - first(Food)) * 100 / first(Food),2), digits = 2)))

#Add inflation rate month by month
data_on <- data_on %>%
  fill(everything(), .direction = "down") %>%
  mutate(inflation_rate_by_month = paste0(formatC(round((Food - lag(Food)) * 100 / lag(Food),2), digits = 2)))

data_on
# Convert Date column to a Date format
data_on$Date <- as.Date(data_on$Date)

#Plot the graph inflation compared with 2020
chart_on <- ggplot(data_on, aes(x = Date, y = as.numeric(inflation_rate))) +
  geom_line() +
  geom_text(aes(label = paste0(round(as.numeric(inflation_rate), 2), "%")),
            hjust = -0.2, vjust = 0.5, size = 3, color = "darkgreen") +
  labs(
    title = "Comparison of Food Inflation in Ontario",
    subtitle = "Based on the month of January 2020",
    caption = "Source: Statistics Canada",
    x = "Date",
    y = "Food Inflation Rate (%)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0.02)) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkred", size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.caption = element_text(face ="italic", hjust=0),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Adjust x-axis text
    axis.title.y = element_text(color = "darkblue", size = 10, face = "bold"),
    panel.background = element_blank(),  # Remove plot background
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),  # Add dashed grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.y = element_text(size = 10)  # Adjust y-axis label font size
  )
# Save the plot as a PNG file
ggsave("chart_on_based_2020.png", chart_on, width = 12, height = 6)

# Print the plot (optional)
print(chart_on)


#For monthly inflation
#Plot the graph
chart_on_monthly <- ggplot(data_on, aes(x = Date, y = as.numeric(inflation_rate_by_month))) +
  geom_line() +
  geom_text(aes(label = paste0(round(as.numeric(inflation_rate_by_month), 2), "%")),
            hjust = -0.2, vjust = 0.5, size = 3, color = "darkgreen") +
  labs(
    title = "Comparison of Food Inflation in Ontario",
    subtitle = "Monthly from January 2020 - January 2023",
    caption = "Source: Statistics Canada",
    x = "Date",
    y = "Food Inflation Rate (%)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0.02)) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkred", size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.caption = element_text(face ="italic", hjust=0),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Adjust x-axis text
    axis.title.y = element_text(color = "darkblue", size = 10, face = "bold"),
    panel.background = element_blank(),  # Remove plot background
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),  # Add dashed grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.y = element_text(size = 10)  # Adjust y-axis label font size
  )
# Save the plot as a PNG file
ggsave("chart_on_monthly.png", chart_on_monthly, width = 12, height = 6)

# Print the plot 
print(chart_on_monthly)

#EXTRA: Select data
#Select data
selected_data_ca <- data_can[, c("Date", "Food", "inflation_rate")]
#Select data
selected_data_to <- data_to[, c("Date", "Food", "inflation_rate")]

