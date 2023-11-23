# Consumer Price Index (CPI) Data Analysis
## Introduction
In this exercise, I will delve into data analysis using R to explore the concepts of Consumer Price Index (CPI)

## Exercise Overview
This exercise is divided into three parts, each with its own set of requirements and tasks:

Download and tidy data

```
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


#setwd() to the right folder
setwd("C:/Users/tanis/Documents/R-project")
#Using DPLYR functions
file_path <- "R-CS2-row.xlsx"
data <- read_excel(file_path)
view(data)
```
### Part 1: Data Retrieval and Tidying
Task: Download the Consumer Price Index (CPI) data from Statistics Canada's Table 18-10-0004-01. Dataset link: Consumer Price Index (CPI) Data
```
# Keep only the required fields
tidy_data <- data %>%
  select(Date, Overall_CPI, CPI_Excluding_Food_Energy, Food_Price, Housing_Price, Energy_Price)
print(tidy_data)
```
![image](https://github.com/Tann1901/Canada-and-Ontario-Inflation-rate/assets/108020327/d8b63208-106a-4622-accd-f408e0708549)

```
q3_data <- data %>%
  select(Date, Food_Price, Housing_Price, Energy_Price)
print(tidy_data)

q3_data$Month <- months(q3_data$Date)  # Create a new column for months
q3_data$Year <- as.integer(format(q3_data$Date, "%Y"))  # Extract the year from the Date
# Calculate Month-over-Month (MoM) and Year-over-Year (YoY) price growth
q3_data <- q3_data %>%
  arrange(Date) %>%
  mutate(
    Food_MoM_Change = (Food_Price / lag(Food_Price) - 1) * 100,
    Food_YoY_Change = (Food_Price / lag(Food_Price, 12) - 1) * 100,
    Housing_MoM_Change = (Housing_Price / lag(Housing_Price) - 1) * 100,
    Housing_YoY_Change = (Housing_Price / lag(Housing_Price, 12) - 1) * 100,
    Energy_MoM_Change = (Energy_Price / lag(Energy_Price) - 1) * 100,
    Energy_YoY_Change = (Energy_Price / lag(Energy_Price, 12) - 1) * 100
  )

# Filter data for the past 18 months
past_18_months_data <- tail(q3_data, 18)
```

![image](https://github.com/Tann1901/Canada-and-Ontario-Inflation-rate/assets/108020327/756199bf-0a38-4bd4-9248-a379d298eef3)

### Part 2: Quarterly CPI Change
Task: Calculate and visualize the quarterly changes in the overall CPI and CPI excluding food and energy over the past 5 years. This will help to understand how the CPI has evolved over time. 
Group Data by quarter year with the format "QX-YYYY"
```
# Convert the 'Date' column to a Date type (if it's not already)
tidy_data$Date <- as.Date(tidy_data$Date)

# Order the quarters within each year

tidy_data$Quarter_Year <- factor(tidy_data$Quarter_Year, levels = unique(tidy_data$Quarter_Year))

# Create a new column 'Quarter_Year' with the format "QX-YYYY"
tidy_data$Quarter_Year <- paste0("Q", quarter(tidy_data$Date), "-", year(tidy_data$Date))

# Group data by the new quarter-year column and summarize
grouped_data <- tidy_data %>%
  group_by(Quarter_Year) %>%
  summarise(
    Average_Overall_CPI = mean(Overall_CPI),
    Average_CPI_Excluding_Food_Energy = mean(CPI_Excluding_Food_Energy),
    Average_Food_Price = mean(Food_Price),
    Average_Housing_Price = mean(Housing_Price),
    Average_Energy_Price = mean(Energy_Price)
  )%>%
  arrange(substring(Quarter_Year, 3))  # Sort by the year part of the Quarter_Year column

# Print the grouped data
print(grouped_data)

grouped_data_change <- grouped_data %>%
  mutate(
    Overall_CPI_Change = ((Average_Overall_CPI - lag(Average_Overall_CPI)) / lag(Average_Overall_CPI)) * 100,
    CPI_Excluding_Food_Energy_Change = ((Average_CPI_Excluding_Food_Energy - lag(Average_CPI_Excluding_Food_Energy)) / lag(Average_CPI_Excluding_Food_Energy)) * 100
  ) %>%
  na.omit()  # Remove the first row with NA values after calculating differences

grouped_data_change
```

Make line chart of CPI change throughout Quarters of Years
```
 Plot the line chart
line_chart <- ggplot(grouped_data_change, aes(x = Quarter_Year)) +
  geom_line(aes(y = Overall_CPI_Change, group = 1, color = "Overall CPI Change"), size = 1) +
  geom_line(aes(y = CPI_Excluding_Food_Energy_Change, group = 1, color = "CPI Excluding Food and Energy Change"), size = 1) +
  labs(title = "Change in CPI and CPI Excluding Food and Energy", y = "Change Value (%)", x = "Quarter-Year") +
  scale_color_brewer(palette = "Pastel1", name = "CPI Category",
                     labels = c("Overall CPI Change", "CPI Excluding Food and Energy Change")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months")


# Save the plot as a PNG file
ggsave("line_chart.png", line_chart, width = 12, height = 6)

# Print the plot (optional)
print(line_chart)
```
![image](https://github.com/Tann1901/Canada-and-Ontario-Inflation-rate/assets/108020327/f19cb612-6e3f-4599-b415-a3032adc84ae)



### Part 3: Monthly Price Growth
Task: Calculate and visualize the monthly price growth in the following categories: food, housing (shelter), and energy (electricity, gas, fuels) over the past 18 months. 
Compare the price index of each month to the previous month (Month over month) and the same month of the previous year (Year over year)

Calculate the monthly price growth in the following categories: food, housing (shelter), and energy (electricity, gas, fuels) over the past 18 months. 
```
# Print the calculated price growth for the past 18 months
print(past_18_months_data[, c("Date", "Food_MoM_Change", "Food_YoY_Change", "Housing_MoM_Change", "Housing_YoY_Change", "Energy_MoM_Change", "Energy_YoY_Change")])

# Convert Date to Date type
past_18_months_data$Date <- as.Date(past_18_months_data$Date)
```

Visualize with line chart
```
# Plot the line chart
line_chart_3<-ggplot(past_18_months_data, aes(x = Date)) +
  geom_line(aes(y = Food_MoM_Change, color = "Food (MoM)"), size = 1) +
  geom_line(aes(y = Food_YoY_Change, color = "Food (YoY)"), size = 1, linetype = "dashed") +
  geom_line(aes(y = Housing_MoM_Change, color = "Housing (MoM)"), size = 1) +
  geom_line(aes(y = Housing_YoY_Change, color = "Housing (YoY)"), size = 1, linetype = "dashed") +
  geom_line(aes(y = Energy_MoM_Change, color = "Energy (MoM)"), size = 1) +
  geom_line(aes(y = Energy_YoY_Change, color = "Energy (YoY)"), size = 1, linetype = "dashed") +
  labs(title = "Monthly Price Growth Over the Past 18 Months",
       x = "Timeline",
       y = "Price Growth (%)",
       color = "Price Category") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  scale_color_brewer(palette = "Spectral",
                     labels = c("Food (MoM)", "Food (YoY)", "Housing (MoM)", "Housing (YoY)", "Energy (MoM)", "Energy (YoY)")) +
  theme_minimal()
# Save the plot as a PNG file
ggsave("line_chart_3.png", line_chart_3, width = 12, height = 6)

# Print the plot (optional)
print(line_chart_3)
```
![image](https://github.com/Tann1901/Canada-and-Ontario-Inflation-rate/assets/108020327/7de2af4a-5226-422a-bbf5-193de412ef82)

**FOOD PRICE**

In the month-by-month comparison, the food prices fluctuate within a range of approximately +-10%, indicating some level of volatility in the market.

However, when comparing the food prices to the same period in the previous year (2022-2023), the analysis reveals more substantial changes. In June 2022, the prices reached a peak with an increase of 40% compared to the previous year. This indicates a significant inflationary pressure on food prices during that period. On the other hand, in June 2023, the prices experienced a decline, touching a low point of -15% compared to the previous year. This suggests a deflationary trend in food prices during that specific month.

These fluctuations in food prices can be attributed to various factors, such as the consequences of the COVID-19 pandemic and potential disruptions in the global food supply chain. Additionally, the mention of war implies that geopolitical factors and conflicts might have contributed to the instability in food supply, especially for a country like Canada that relies heavily on agricultural imports.

The unstable supply of food due to these factors can have significant implications for consumers, businesses, and policymakers. It may lead to increased food costs, reduced affordability, and challenges in ensuring food security for the population. Policymakers may need to address these issues through measures such as improving domestic food production, diversifying food sources, implementing price stabilization mechanisms, and supporting agricultural sectors.

**HOUSE PRICE AND ENERGY PRICE**

Based on the line chart, we can observe that both house prices and energy prices exhibit similar trends in both month-to-month and year-to-year comparisons.

In the month-to-month comparison, both house prices and energy prices demonstrate a relatively stable pattern. There are minimal fluctuations, indicating a consistent level of prices over the past 18 months.

However, the year-to-year comparison reveals a noticeable increase in prices for both house prices and energy prices. The chart indicates that the prices in these categories have experienced a growth range of 5-10% when compared to the same period in the previous year. This suggests a significant upward trend in prices over the longer term.

The similar trends observed in both month-to-month and year-to-year comparisons for house prices and energy prices indicate a degree of stability in these markets. The year-to-year price growth suggests a potential increase in the cost of housing and energy for consumers over time.

**CONCLUSION**

The line chart presented provides valuable insights into the overall economic situation in Canada. By tracking the monthly price growth in key sectors such as food, housing, and energy, it offers a glimpse into the country's inflation trends. While inflation is a crucial economic indicator, it is important to note that a comprehensive understanding of the Canadian economy requires consideration of various other factors.

To make informed policy decisions, social policymakers must analyze a range of indicators beyond inflation. Factors such as GDP growth, employment rates, consumer spending, investment patterns, fiscal policies, and monetary policies all contribute to the broader economic landscape. By taking these factors into account, policymakers can gain a more comprehensive view of the current economic conditions and identify areas that require attention.

Utilizing a diverse set of economic indicators, including inflation indices, empowers policymakers to implement appropriate policies tailored to address specific challenges. These policies may encompass measures such as monetary policy adjustments, fiscal stimulus initiatives, employment programs, social welfare reforms, and regulatory frameworks. By considering a wide array of indicators, policymakers can develop well-rounded strategies that promote economic stability, growth, and social well-being in Canada.

In summary, while the line chart provides valuable insights into inflation trends, it is essential for social policymakers to consider a broader set of economic indicators to accurately assess the overall economic situation. This holistic approach enables policymakers to implement the right policies that address the needs and challenges of the country effectively.
