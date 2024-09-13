# Load necessary libraries
library(zoo)
library(GENEAread)
library(ggplot2)
library(zoo) 
# Function to calibrate the data
calibration <- function(binfile) {
  calibration_sphere <- GENEActiv.calibrate(binfile, printsummary = FALSE)
  calibration_offset <- calibration_sphere$offset 
  calibration_scale <- calibration_sphere$scale
  return(c(calibration_scale, calibration_offset))
}

# Define the binary file path
binfile <- "D:/Downloads/p004/P015_031194_2021-02-10 14-16-24.bin"

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
# Convert Timestamp from numeric to POSIXct
raw_data_df$Timestamp <- as.POSIXct(raw_data_df$Timestamp, origin="1970-01-01")

str(raw_data_df)

plot_time_period_data <- function(period_data, period_index, window_size = 6000) {
  # Ensure Timestamp is correctly formatted as POSIXct
  period_data$Timestamp <- as.POSIXct(period_data$Timestamp, origin="1970-01-01")
  
  # Calculate the rolling average for the Y component with a specified window size
  period_data$Y_roll_avg <- rollmean(period_data$Y, k = window_size, fill = NA, align = "right")
  
  plot <- ggplot(period_data, aes(x = Timestamp)) +
    # Use thinner lines for the data
    geom_line(aes(y = X, color = "X"), size = 0.05) +  # X-axis (thinner line)
    geom_line(aes(y = Z, color = "Z"), size = 0.05) +  # Z-axis (thinner line)
    geom_line(aes(y = Y, color = "Y"), size = 0.05) +  # Y-axis (thinner line)
    
    # Plot the rolling average for Y with a thicker line for clarity
    geom_line(aes(y = Y_roll_avg, color = "Y Roll Avg"), size = 0.75, linetype = "solid") +
    
    # Add horizontal lines for y=1 and y=0
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    
    # Customize axis labels
    labs(title = paste("Raw data with moving average", period_index), x = "Time", y = "Acceleration") +
    
    # Customize the colors for each component, including Y Roll Avg
    scale_color_manual(
      name = "Axis", 
      values = c("Y" = "green", "Z" = "lightcoral", "X" = "skyblue", "Y Roll Avg" = "black")
    ) +
    
    # Customize the x-axis to show time at midnight, 6 AM, 12 PM, 6 PM, and midnight
    scale_x_datetime(
      date_labels = "%H:%M", 
      breaks = seq(
        as.POSIXct(floor(as.numeric(min(period_data$Timestamp)) / (24 * 60 * 60)) * (24 * 60 * 60), origin = "1970-01-01"),
        max(period_data$Timestamp),
        by = "6 hours"
      )
    ) +
    
    scale_y_continuous(breaks = seq(floor(min(period_data$Z)), ceiling(max(period_data$Z)), by = 1)) +
    
    # Apply a white background theme
    theme_bw() +
    
    # Customize the theme for a clearer presentation
    theme(
      axis.line.x = element_line(size = 0.5, color = "black"),  # Thin X-axis line
      axis.line.y = element_line(size = 0.5, color = "black"),  # Thin Y-axis line
      panel.grid.minor = element_blank(),  # Remove minor grid lines for clarity
      panel.grid.major = element_blank()   # Remove major grid lines as well
    )
  
  # Print the plot to ensure it is displayed
  print(plot)
  
  # Save the plot to the same directory
  plot_path <- paste0("D:/Downloads/p004/period_", period_index, "_plot.png")
  ggsave(plot_path, plot = plot, width = 10, height = 6)
}



# Loop through each 6-hour period and plot the data
start_time <- min(raw_data_df$Timestamp) + ((24*60*60)*4)
end_time <- max(raw_data_df$Timestamp)
current_period <- 1

while (start_time < end_time) {
  period_end_time <- start_time + 24*60*60
  period_data <- raw_data_df[raw_data_df$Timestamp >= start_time & raw_data_df$Timestamp < period_end_time, ]
  
  if (nrow(period_data) > 0) {
    plot_time_period_data(period_data, current_period)
    current_period <- current_period + 1
  }
  
  start_time <- period_end_time
}
