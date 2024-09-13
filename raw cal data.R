# Load necessary libraries
library(zoo)
library(GENEAread)
library(ggplot2)

# Function to calibrate the data
calibration <- function(binfile) {
  calibration_sphere <- GENEActiv.calibrate(binfile, printsummary = FALSE)
  calibration_offset <- calibration_sphere$offset 
  calibration_scale <- calibration_sphere$scale
  return(c(calibration_scale, calibration_offset))
}

# Define the binary file path
binfile <- "D:/Downloads/in/P021_027734_2021-02-10 15-37-24.bin"

# Read the entire binary data file
bin_data <- read.bin(binfile)
day_data <- bin_data$data.out

# Calibrate the x, y, z components
calib <- calibration(binfile)

calibrated_data <- cbind(
  Timestamp = as.POSIXct(day_data[,1], origin="1970-01-01"),
  X = (calib[1] * day_data[,'x']) + calib[4],
  Y = (calib[2] * day_data[,'y']) + calib[5],
  Z = (calib[3] * day_data[,'z']) + calib[6],
  Temp = day_data[,7]
)

# Convert to a data frame
raw_data_df <- as.data.frame(calibrated_data)

# Filter for the first 12 hours
start_time <- min(raw_data_df$Timestamp)
end_time <- start_time + 12*60*60
first_day_data <- raw_data_df[raw_data_df$Timestamp >= start_time & raw_data_df$Timestamp < end_time, ]

# Check if x is approximately 1
tolerance <- 0.1  # Define a tolerance level for x
if (any(abs(first_day_data$X - 1) > tolerance)) {
  warning("X component deviates significantly from 1 at some points.")
}

# Plot the Y component
plot <- ggplot(first_day_data, aes(x = Timestamp, y = Y)) +
  geom_line(color = "blue") +
  labs(title = "Y Component Over First 12 Hours", x = "Time", y = "Y Acceleration") +
  theme_minimal()

# Print the plot to ensure it is displayed
print(plot)
