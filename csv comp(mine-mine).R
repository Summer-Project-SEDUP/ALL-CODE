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
additional_csv_file_path <- "D:/Downloads/out/no-matrix/P016_040173_2021-02-10 15-07-55/all_days.csv"
csv_file_path <- "D:/Downloads/out/compfiles/P016_040173_2021-02-10 15-07-55/all_days.csv"
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

# Combine the Date and Time columns to create a proper Timestamp
timestamp_data <- timestamp_data %>%
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))

# Calculate the number of 24-hour segments in the data
start_time <- min(timestamp_data$Timestamp, na.rm = TRUE)
total_time <- max(timestamp_data$Timestamp, na.rm = TRUE) - start_time
num_segments <- ceiling(as.numeric(total_time, units = "hours") / 24)

# Loop through each 24-hour segment
for (i in 1:num_segments) {
  
  segment_start_time <- start_time + (i-1)*24*60*60
  segment_end_time <- segment_start_time + 12*60*60  # 12-hour window within each 24-hour period
  
  # Filter the data for the current segment
  segment_data <- calibrated_data %>%
    filter(Timestamp >= segment_start_time & Timestamp < segment_end_time)
  
  # Filter the timestamps for the same period
  segment_timestamps <- timestamp_data %>%
    filter(Timestamp >= segment_start_time & Timestamp < segment_end_time)
  
  # Create a column indicating the segment ID for changing line color
  segment_data <- segment_data %>%
    mutate(LineSegment = findInterval(Timestamp, segment_timestamps$Timestamp))
  
  # Load the additional CSV file with the timestamps
  additional_timestamp_data <- read_csv(additional_csv_file_path)
  
  # Combine the Date and Time columns to create a proper Timestamp
  additional_timestamp_data <- additional_timestamp_data %>%
    mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))
  
  # Filter the additional timestamps for the same period
  additional_timestamp_data <- additional_timestamp_data %>%
    filter(Timestamp >= segment_start_time & Timestamp < segment_end_time)
  
  # Create a column indicating the segment ID for alternating background colors
  segment_data <- segment_data %>%
    mutate(BackgroundSegment = findInterval(Timestamp, additional_timestamp_data$Timestamp))
  
  # Ensure the ymin and ymax cover the entire y-axis range
  y_min <- min(segment_data$X, na.rm = TRUE) - 1  # Adding buffer to ensure visibility
  y_max <- max(segment_data$X, na.rm = TRUE) + 1
  
  # Plot the X component over the current 24-hour segment with red lines and blue background
  plot_x_background <- ggplot(segment_data) +
    geom_rect(aes(xmin = Timestamp, xmax = lead(Timestamp), 
                  ymin = y_min, ymax = y_max, 
                  fill = as.factor(BackgroundSegment %% 2)), alpha = 0.2) +
    geom_line(aes(x = Timestamp, y = X, group = LineSegment, color = as.factor(LineSegment %% 2))) +
    scale_fill_manual(values = c("0" = "white", "1" = "blue")) +
    scale_color_manual(values = c("0" = "black", "1" = "red")) +
    labs(title = paste("X Component Over Time - 24 Hour Period", i), x = "Time", y = "X Acceleration") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "none",  # Hide legend for background segments and line colors
      legend.title = element_blank()
    ) +
    scale_y_continuous(limits = c(y_min, y_max))
  
  # Print the plot to ensure it is displayed
  print(plot_x_background)
  
  # Save the plot to the specified directory
  ggsave(filename = paste0("D:/Downloads/X_Component_Over_Time_Period_", i, ".png"), 
         plot = plot_x_background, width = 10, height = 6)
}

# Display the first few rows of the last segment
head(segment_data)
