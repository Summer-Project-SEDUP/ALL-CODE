# Load necessary libraries
library(zoo)
library(GENEAread)
library(ggplot2)
library(dplyr)
library(lubridate)

# Function to calibrate the data
calibration <- function(binfile) {
  calibration_sphere <- GENEActiv.calibrate(binfile, printsummary = FALSE)
  calibration_offset <- calibration_sphere$offset 
  calibration_scale <- calibration_sphere$scale
  return(c(calibration_scale, calibration_offset))
}

# Define the binary file path
binfile <- "D:/Downloads/in/P004_031202_2021-01-12 15-53-17.bin"

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

# Define the total duration to filter
total_duration_hours <- 48

# Filter for the total duration
start_time <- min(calibrated_data$Timestamp)
end_time <- start_time + total_duration_hours*60*60
filtered_data <- calibrated_data %>%
  filter(Timestamp >= start_time & Timestamp < end_time)

# Add ENMO
add_ENMO <- function(day_matrix) {
  ENMO <- pmax(((rowSums(day_matrix[,c('X','Y','Z')]^2))^(1/2) - 1), 0)
  day_matrix$ENMO <- ENMO
  return(day_matrix)
}

filtered_data <- add_ENMO(filtered_data)

# Determine posture (standing or sitting)
SedUp <- function(y, fs, option) {
  constant <- 10
  if (option == 1) {  
    coeffs  <- c(-2.390, 2.542, 42.394)
    uni_thr <- 0.362
    nSec    <- 4
  } else if (option == 2)  {  
    coeffs  <- c(-2.420, 2.616, 44.083)
    uni_thr <- 0.372
    nSec    <- 6
  }
  y <- y[1:(floor(length(y)/fs/constant)*fs*constant)]
  sdv <- sapply(split(y, rep(1:(length(y) / fs), each = fs)), sd)
  window1 <- nSec * constant * fs / 2
  window2 <- round(nSec * constant / 2)
  medacc <- numeric(length(sdv)/constant)
  medsd <- numeric(length(sdv)/constant)
  j1 <- 0
  j2 <- 0
  for (i in seq(from = 1, to = length(sdv), by = constant)) {
    j1 <- j1 + 1
    i1_1 <- (i - 1) * fs + 1 - window1
    i1_2 <- (i - 1) * fs + 1 + constant * fs + window1
    b <- i1_1:i1_2
    medacc[j1] <- median(y[b[b > 0 & b < length(y)]], na.rm = TRUE)
    j2 <- j2 + 1
    i2_1 <- (i - 1) - window2
    i2_2 <- (i - 1) + 1 + constant + window2
    b <- i2_1:i2_2
    medsd[j2] <- median(sdv[b[b > 0 & b < length(sdv)]], na.rm = TRUE)
  }
  logit <- coeffs[1] + coeffs[2] * medacc + coeffs[3] * medsd
  phat <- exp(logit) / (1 + exp(logit))
  standing <- ifelse(phat >= uni_thr, 1, 0)
  standing <- rep(standing, each = constant * fs)
  return(standing)
}

# Assuming the frequency is 50Hz, adjust according to your data
freq  = as.numeric(as.character(bin_data$header['Measurement_Frequency',1]))

# Add posture columns for each axis separately
filtered_data$Posture_X <- SedUp(filtered_data$X, freq, 1)
filtered_data$Posture_Y <- SedUp(filtered_data$Y, freq, 1)
filtered_data$Posture_Z <- SedUp(filtered_data$Z, freq, 1)

# Add readable time format
filtered_data$ReadableTime <- format(as.POSIXct(filtered_data$Timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")

# Convert the datetime column to POSIXct object
filtered_data$datetime <- as.POSIXct(filtered_data$ReadableTime, format = "%Y-%m-%d %H:%M:%S")

# Extract the day from the datetime column
filtered_data$day <- as.Date(filtered_data$datetime)

# Loop through each day to create and save plots
days <- unique(filtered_data$day)
for (current_day in days) {
  day_data <- filtered_data %>% filter(day == current_day)
  
  # Create segments for plotting with ggplot2 for X, Y, and Z components
  segment_data_x <- day_data %>%
    mutate(Timestamp_next = lead(Timestamp),
           X_next = lead(X)) %>%
    filter(!is.na(Timestamp_next))
  
  segment_data_y <- day_data %>%
    mutate(Timestamp_next = lead(Timestamp),
           Y_next = lead(Y)) %>%
    filter(!is.na(Timestamp_next))
  
  segment_data_z <- day_data %>%
    mutate(Timestamp_next = lead(Timestamp),
           Z_next = lead(Z)) %>%
    filter(!is.na(Timestamp_next))
  
  # Plot the X component with color based on posture using Timestamp for continuous plotting
  plot_x <- ggplot(segment_data_x) +
    geom_segment(aes(x = Timestamp, y = X, xend = Timestamp_next, yend = X_next, color = factor(Posture_X))) +
    scale_color_manual(values = c("blue", "red"), labels = c("Sitting", "Standing")) +
    labs(title = paste("X Component on", current_day), x = "Time", y = "X Acceleration", color = "Posture") +
    theme_minimal()
  
  # Plot the Y component with color based on posture using Timestamp for continuous plotting
  plot_y <- ggplot(segment_data_y) +
    geom_segment(aes(x = Timestamp, y = Y, xend = Timestamp_next, yend = Y_next, color = factor(Posture_Y))) +
    scale_color_manual(values = c("blue", "red"), labels = c("Sitting", "Standing")) +
    labs(title = paste("Y Component on", current_day), x = "Time", y = "Y Acceleration", color = "Posture") +
    theme_minimal()
  
  # Plot the Z component with color based on posture using Timestamp for continuous plotting
  plot_z <- ggplot(segment_data_z) +
    geom_segment(aes(x = Timestamp, y = Z, xend = Timestamp_next, yend = Z_next, color = factor(Posture_Z))) +
    scale_color_manual(values = c("blue", "red"), labels = c("Sitting", "Standing")) +
    labs(title = paste("Z Component on", current_day), x = "Time", y = "Z Acceleration", color = "Posture") +
    theme_minimal()
  
  # Print the plots to ensure they are displayed
  print(plot_x)
  print(plot_y)
  print(plot_z)
  
  # Save the plots to the specified directory
  ggsave(filename = paste0("D:/Downloads/X_Component_", current_day, ".png"), plot = plot_x, width = 10, height = 6)
  ggsave(filename = paste0("D:/Downloads/Y_Component_", current_day, ".png"), plot = plot_y, width = 10, height = 6)
  ggsave(filename = paste0("D:/Downloads/Z_Component_", current_day, ".png"), plot = plot_z, width = 10, height = 6)
}

# Display the first few rows of the data
head(filtered_data)
