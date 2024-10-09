
# Source of visual for revisualization: https://www.nytimes.com/2022/01/06/opinion/omicron-covid-us.html

install.packages("tidyverse")
install.packages("zoo")
install.packages("xts")

# Adding required libraries:
library(tidyverse)

# I am generating a ordered time series of dates for a period of 2 years  and create rolling average
# https://www.geeksforgeeks.org/zoo-package-in-r/
library(zoo) 
library(lubridate)
library(xts)


# Generate dates from 01 Jan 2024
# The reason for choosing seq.Date() over seq() is seq() is more general function and seq.Date() is specific to create date. 
# https://www.geeksforgeeks.org/how-to-generate-a-sequence-of-timestamps-in-r/
# https://rpubs.com/pep1024/date_sequence
date_cases <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2021-12-31"), by = "day")
num_data<-length(date_cases)
print(num_data)

# Using set.seed(), the sample dataset generated will be reproduced during all execution
set.seed(1)

# Original visual consist of count of number of cases over several months. Hence
# https://www.geeksforgeeks.org/create-random-deviates-of-uniform-distribution-in-r-programming-runif-function/
country_data <- round(runif(num_data, min = 0, max = 200))

# Add missing data of 50 randomly to make the dataset realistic. It is done through randomly selecting few datas and replacing it with NA
# https://www.geeksforgeeks.org/how-to-generate-a-sample-using-the-sample-function-in-r/- Example 5: Random Sampling of List Elements Using sample Function
# syntax: data[sample(1:length(data), size = 4)]
# https://sparkbyexamples.com/r-programming/replace-values-in-r/
NA_data <- sample(1:num_data, size=50)
country_data[NA_data] <- NA

# Create the dataset required for visualization 
sample_dataset<- tibble(
  Date=date_cases,
  Number_of_cases = country_data
)
# Print few sample datasets
head(sample_dataset)

# Save it as .csv file:
write.csv(sample_dataset, "sampleData.csv", row.names = F)

# To plot the cases in each month, we need to take year and month from the Date column.For this we use lubricate library 
# We extract the year and month as a numerical value 
# https://sqlpad.io/tutorial/extract-year-r-complete-guide/#:~:text=A%3A%20The%20lubridate%20package%20in,due%20to%20its%20intuitive%20functions.
# Refer section: What is the lubridate package and how does it help in extracting years?
sample_dataset <- sample_dataset %>%
  mutate(Year = year(Date), 
         Month = month(Date)) 
#https://www.stat.berkeley.edu/~s133/dates.html

# Print few sample datasets with Year and Month column added:
str(sample_dataset)

# Calculating 7 day average: In the original model, the 7-day average will be plotted for 2 years
# https://www.geeksforgeeks.org/how-to-calculate-a-rolling-average-in-r/
# https://stackoverflow.com/questions/66415472/calculate-7-day-average-in-r
sample_dataset <- sample_dataset %>%
  group_by(Year) %>%
  mutate('7-Day avg'= rollmean(Number_of_cases, k=7, fill=NA, na.rm=T)) # Data frame is already passed through pipe operator. na.rm = T is added to ignore the NA values and proceed calculation

str(sample_dataset)

# 1. Visualization using geom smooth 

ggplot(sample_dataset, aes(x = Month, y = `7-Day avg`, color = as.factor(Year), group = Year)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2, alpha=0.3) + # standard error wave is set to small 
  labs(title = "7-Day Average of COVID cases for 2020-2021 (USA)",
       x = "Month", 
       y = "7-Day Average", 
       color = "Year") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  scale_x_continuous(breaks = 1:12, labels = month.abb)  # https://forum.posit.co/t/converting-number-month-to-month-name/106389
  
# 2. Visualization using bar graph
  
# Calculate mean for each month from 7-day average and summarize it to plot monthwise 
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/summarise
mean_monthly <- sample_dataset %>%
  group_by(Year, Month) %>% # group Month and Year for plotting
  summarize(monthlyMean = mean(`7-Day avg`, na.rm = TRUE))

# Plotting bar to compare the mean values of each month over 2020 and 2021
ggplot(mean_monthly,aes(x = factor(Month, levels = 1:12, labels = month.abb), y = monthlyMean, fill = as.factor(Year))) + # Converting month into Factors to be plotted in x axis
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.6) +  # Adjusting the size of bars
  labs(
    title = "Mean (7 day average) of Cases COVID-19-Monthwise - 2020-2021",
    x = "Month",
    y = "Mean (7 day-average)",
    fill = "Year") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10),  
        axis.text.y = element_text(size = 8),   
        plot.margin = margin(1, 1, 1, 1, "cm"),  # To reduce the size of the graph
        plot.title = element_text(size = 12)) +  
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("2020" = "lightblue", "2021" = "orange"))
