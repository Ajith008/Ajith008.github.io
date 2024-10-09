# Load necessary libraries
library(ggplot2)
library(dplyr)

# Loading the CSV file of the given data
data <- read.csv("competitive_eating_data.csv")

# Convert the data to numeric if it's not in numeric form
data$Hot_Dogs_Eaten <- as.numeric(as.character(data$Hot_Dogs_Eaten))

# Handle missing values for female competitors pre-2011 by assigning NA
data <- data %>%
  mutate(Hot_Dogs_Eaten = ifelse(Gender == "Female" & Year < 2011, NA, Hot_Dogs_Eaten))



#VISUALIZATION1
# Faceted Line Plot for gender-based comparison
ggplot(data, aes(x = Year, y = Hot_Dogs_Eaten, color = Competitor, group = Competitor)) +
  geom_line(size = 1.5, na.rm = TRUE) +  # Line plot for each competitor
  geom_point(size = 3) +  # Add points for each year
  
  # Highlight record-breaking performances with larger points
  geom_point(data = subset(data, Hot_Dogs_Eaten >= 70), 
             aes(x = Year, y = Hot_Dogs_Eaten), 
             size = 5, shape = 21, fill = "yellow", color = "black", stroke = 1.5) +
  
  # Facet based on  Gender for a better comparison between male and female trends
  facet_wrap(~ Gender, scales = "free_y") +
  
  # Titles and labels
  labs(title = "Gender wise Competitive Eating Performance  ",
       x = "Year", 
       y = "Hot Dogs Eaten",
       color = "Competitor") +
  
  # Minimal theme for clarity
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
#VISUALIZATION2
# Visualization using Stacked Area Plot for cumulative performance over time
ggplot(data, aes(x = Year, y = Hot_Dogs_Eaten, fill = Competitor)) +
  geom_area(alpha = 0.6, size = 1.2, color = "black", na.rm = TRUE) +  # Stacked areas for cumulative performance
  
  # Highlight record-breaking points
  geom_point(data = subset(data, Hot_Dogs_Eaten >= 70), 
             aes(x = Year, y = Hot_Dogs_Eaten), 
             size = 5, shape = 21, fill = "yellow", color = "black", stroke = 1.5) +
  
  # Titles and labels
  labs(title = "Cumulative Performance of Competitive Eaters Over Time",
       x = "Year", 
       y = "Hot Dogs Eaten",
       fill = "Competitor") +
  
  # Minimal theme for clarity
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
