# Load necessary libraries
library(dplyr)
library(GGally)
library(ggplot2)

# Read in the two datasets
burstiness_metrics <- read.csv("D:/Downloads/out/fulloutv1/all_burstiness_metrics.csv")
people_data  <- read.csv("D:/Downloads/out/fulloutv1/all_people.csv")



# Rename the 'Person' column in burstiness_metrics to 'FILE' to match the people_data dataset
colnames(burstiness_metrics)[colnames(burstiness_metrics) == "Person"] <- "FILE"

# Merge the two datasets on the 'FILE' column
merged_data <- merge(burstiness_metrics, people_data, by = "FILE")

# Perform correlation analysis on relevant numeric columns
numeric_columns <- merged_data %>%
  select(Alternative_Burstiness, Volume, Intensity, Duration, NonWear,
         #Percentile_5, Percentile_50, Percentile_95, 
         Max, Min, SD, 
         Windsorized_SD)

# Calculate correlation matrix
correlation_matrix <- cor(numeric_columns, use = "complete.obs")

# Print correlation matrix
print(correlation_matrix)


# Select relevant numeric columns for the pairplot
numeric_columns <- merged_data %>%
  select(Alternative_Burstiness, Max, Min, SD)
         #Volume, Intensity, Duration, Percentile_50
        # Max, Min, SD, 
         #Windsorized_SD)

# Create a pairplot using ggpairs from the GGally package
ggpairs(numeric_columns, 
        title = "Pairplot of Burstiness and Health Metrics")
