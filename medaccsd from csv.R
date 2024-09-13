# Load required libraries
library(ggplot2)
library(dplyr)

# Function to read CSV and plot data
plot_activity_metrics <- function(csv_file) {
  # Read the CSV file
  data <- read.csv(csv_file)
  
  # Convert Date and Time columns to POSIXct for proper datetime handling
  data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
  
  # Ensure the Active column is treated as a factor
  data$Active <- as.factor(data$Active)
  
  # Ensure the Date column is treated as a Date type
  data$Date <- as.Date(data$Date, format="%Y-%m-%d")
  
  # Create the first plot
  plot1 <- ggplot(data, aes(x = Datetime)) +
    geom_line(aes(y = medacc, color = Active, linetype = "Median Acceleration")) +
    geom_line(aes(y = medsd, color = Active, linetype = "Median Standard Deviation")) +
    scale_color_manual(values = c("0" = "darkblue", "1" = "red")) +
    scale_linetype_manual(values = c("Median Acceleration" = "solid", "Median Standard Deviation" = "dashed")) +
    labs(title = "Median Acceleration and Standard Deviation Over Time",
         x = "Time",
         y = "Value",
         color = "Active",
         linetype = "Metric") +
    theme_minimal() +
    facet_wrap(~ Date, scales = "free_x")
  
  # Create the second plot with medacc segmented by Active and faceted by Date
  plot2 <- ggplot(data, aes(x = Datetime, y = medacc, group = 1)) +
    geom_line(aes(color = Active)) +
    scale_color_manual(values = c("0" = "darkblue", "1" = "red")) +
    labs(title = "Median Acceleration Over Time Segmented by Active Status",
         x = "Time",
         y = "Median Acceleration",
         color = "Active") +
    theme_minimal() +
    facet_wrap(~ Date, scales = "free_x")
   
  
  
  # Create the third plot comparing medacc vs medsd without grid lines and with x and y axis lines
  plot3 <- ggplot(data, aes(x = medacc, y = medsd)) +
    geom_point(aes(color = Active)) +
    scale_color_manual(values = c("0" = "darkblue", "1" = "red")) +
    labs(title = "Median Acceleration vs Median Standard Deviation",
         x = "Median Acceleration",
         y = "Median Standard Deviation",
         color = "Active") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  # Print the plots
  print(plot1)
  print(plot2)
  print(plot3)
}

# Usage
csv_file_path <- "D:/Downloads/fulloutput/P004_031202_2021-01-12 15-53-17/all_days.csv"
plot_activity_metrics(csv_file_path)
