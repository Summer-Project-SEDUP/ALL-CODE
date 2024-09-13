# Load necessary libraries
library(zoo)
library(GENEAread)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

# Function to calibrate the data
calibration <- function(binfile) {
  calibration_sphere <- GENEActiv.calibrate(binfile, printsummary = FALSE)
  calibration_offset <- calibration_sphere$offset 
  calibration_scale <- calibration_sphere$scale
  return(c(calibration_scale, calibration_offset))
}

# File paths
csv_file_path <- "D:/Downloads/out/test/P016_040173_2021-02-10 15-07-55/all_days.csv"
binfile <- "D:/Downloads/p004/P016_040173_2021-02-10 15-07-55.bin"

# Read the entire binary data file
bin_data <- read.bin(binfile)
day_data <- bin_data$data.out

# Calibrate the x, y, z components
calib <- calibration(binfile)

calibrated_data <- data.frame(
  Timestamp = as.POSIXct(day_data[,1], origin="1970-01-01"),
  X = (calib[1] * day_data[,'x']) + calib[4],
  Y = (calib[2] * day_data[,'y']) + calib[5],
  Z = (calib[3] * day_data[,'z']) + calib[6],  # Adding 1g back to the Z-axis
  Temp = day_data[,7]
)

# Load the CSV file with the timestamps
timestamp_data <- read_csv(csv_file_path)

timestamp_data <- timestamp_data %>%
  arrange(DayNo)

# Combine the Date and Time columns to create a proper Timestamp
timestamp_data <- timestamp_data %>%
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))

# Determine the start and end times for plotting
start_time <- min(timestamp_data$Timestamp, na.rm = TRUE)
end_time <- max(timestamp_data$Timestamp, na.rm = TRUE)

# Create a sequence of days
days_seq <- seq(from = start_time, to = end_time, by = "24 hours")

# Loop over each 24-hour period and create the plots
for (i in seq_along(days_seq)) {
  
  # Define the current 24-hour period
  day_start <- days_seq[i]
  day_end <- day_start + 24*60*60
  
  # Filter the data for the current 24-hour period
  day_data_filtered <- calibrated_data %>%
    filter(Timestamp >= day_start & Timestamp < day_end)
  
  timestamp_filtered <- timestamp_data %>%
    filter(Timestamp >= day_start & Timestamp < day_end)
  
  # Ensure the timestamp data is sorted and remove any NA values
  timestamp_filtered <- timestamp_filtered %>%
    filter(!is.na(Timestamp)) %>%
    arrange(Timestamp)
  
  # Ensure the day data is also sorted and remove any NA values
  day_data_filtered <- day_data_filtered %>%
    filter(!is.na(Timestamp)) %>%
    arrange(Timestamp)
  
  # Create a column indicating the segment ID for alternating colors
  day_data_filtered <- day_data_filtered %>%
    mutate(Segment = findInterval(Timestamp, timestamp_filtered$Timestamp))
  
  # Ensure the ymin and ymax cover the entire y-axis range
  y_min <- min(day_data_filtered$X, na.rm = TRUE) - 1
  y_max <- max(day_data_filtered$X, na.rm = TRUE) + 1
  
  # Create the plot for the current day
  plot_x <- ggplot(day_data_filtered) +
    geom_line(aes(x = Timestamp, y = X, group = Segment, color = as.factor(Segment %% 2))) +
    scale_color_manual(values = c("0" = "black", "1" = "red")) +
    labs(title = paste("X Component Over Time - Day", i), x = "Time", y = "X Acceleration") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "top",
      legend.title = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  
  # Save the plot to the specified directory
  ggsave(filename = paste0("D:/Downloads/X_Component_Over_Time_Day_", i, ".png"), plot = plot_x, width = 10, height = 6)
  
  # Print progress
  print(paste("Plot for Day", i, "saved."))
}

# Display the first few rows
head(calibrated_data)

  