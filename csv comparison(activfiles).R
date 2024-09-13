# Load necessary libraries
library(zoo)
library(GENEAread)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(fs)

# Function to calibrate the data
calibration <- function(binfile) {
  calibration_sphere <- GENEActiv.calibrate(binfile, printsummary = FALSE)
  calibration_offset <- calibration_sphere$offset 
  calibration_scale <- calibration_sphere$scale
  return(c(calibration_scale, calibration_offset))
}

# File paths
additional_csv_file_path <- "D:/Downloads/Re_ potential fix/P004-AP842586 204at 12Dec20 12-00am for 9d 23h 59m-VANE-PB08101256-UprightBouts.csv"
csv_file_path <- "D:/Downloads/fulloutput/P004_031202_2021-01-12 15-53-17/all_days.csv"
binfile <- "D:/Downloads/in/P004_031202_2021-01-12 15-53-17.bin"

# Extract the first four digits of the bin file name
binfile_name <- basename(binfile)
folder_name <- paste0(substr(binfile_name, 1, 4), "_comparison_plot")

# Create the folder if it doesn't exist
output_dir <- file.path("D:/Downloads", folder_name)
dir_create(output_dir)

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

# Properly read the additional CSV file by skipping the first row
timestamp_data1 <- read_delim(additional_csv_file_path, delim = ";", skip = 1, col_names = FALSE)

# Extract the relevant columns and rename them
timestamp_data1 <- timestamp_data1 %>%
  select(X2) %>%
  rename(Time_approx = X2) %>%
  separate(Time_approx, into = c("Time_approx", "extra"), sep = ";") %>%
  select(Time_approx) %>%
  mutate(Timestamp = as.POSIXct(Time_approx, format="%Y-%m-%d %H:%M:%S"))

# Combine the Date and Time columns to create a proper Timestamp
timestamp_data <- timestamp_data %>%
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))

# Define the start time from the first data point in calibrated_data
start_time <- min(calibrated_data$Timestamp, na.rm = TRUE)
end_time <- max(calibrated_data$Timestamp, na.rm = TRUE)

# Generate plots for each 24-hour period
current_start <- start_time

while (current_start < end_time) {
  current_end <- current_start + 24*60*60
  
  # Filter the data for the current 24-hour period
  day_data <- calibrated_data %>%
    filter(Timestamp >= current_start & Timestamp < current_end) %>%
    arrange(Timestamp)
  
  # Filter the timestamps for the same 24-hour period
  day_timestamps <- timestamp_data %>%
    filter(Timestamp >= current_start & Timestamp < current_end) %>%
    arrange(Timestamp) %>%
    drop_na()
  
  day_timestamps1 <- timestamp_data1 %>%
    filter(Timestamp >= current_start & Timestamp < current_end) %>%
    arrange(Timestamp) %>%
    drop_na()
  
  # Create a column indicating the segment ID for alternating colors
  day_data <- day_data %>%
    mutate(Segment = findInterval(Timestamp, day_timestamps$Timestamp))
  
  # Create the background rectangles
  background_rects <- day_timestamps1 %>%
    mutate(end = lead(Timestamp, default = max(day_data$Timestamp)),
           fill = row_number() %% 2 == 1) %>%
    filter(fill) # Filter to only include the "filled" segments
  
  # Plot the X component over the current 24 hours with alternating colors
  plot_x <- ggplot(day_data) +
    geom_rect(data = background_rects,
              aes(xmin = Timestamp, xmax = end, ymin = -Inf, ymax = Inf),
              fill = "blue", alpha = 0.4) +  # Set the fill color directly
    geom_line(aes(x = Timestamp, y = X, group = Segment, color = as.factor(Segment %% 2)), linewidth = 0.25) +
    scale_color_manual(values = c("0" = "black", "1" = "red"),
                       labels = c("Sitting", "Standing")) +  # Add labels for the legend
    labs(title = paste("X Component Over Time", format(current_start, "%Y-%m-%d")), 
         x = "Time", 
         y = "X Acceleration",
         color = "Posture") +  # Add color and fill legend titles
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "top",
      legend.title = element_blank()
    )
  
  # Print the plots to ensure they are displayed
  print(plot_x)
  
  # Save the plots to the specified directory
  ggsave(filename = file.path(output_dir, paste0("X_Component_Over_Time_", format(current_start, "%Y-%m-%d"), ".png")), plot = plot_x, width = 10, height = 6)
  
  # Move to the next 24-hour period
  current_start <- current_end
}

# Display the first few rows of the final day's data for verification
head(day_data)
