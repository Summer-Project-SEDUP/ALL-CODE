library(zoo)
library(GENEAread)
library(ggplot2)
library(dplyr)
library(tidyr)
library(signal)
library(lubridate)

# Define the binary file path
binfile <- "D:/Downloads/in/P021_027734_2021-02-10 15-37-24.bin"

calibration <- function(binfile) {
  calibration_sphere <- GENEActiv.calibrate(binfile, printsummary = FALSE)
  calibration_offset <- calibration_sphere$offset 
  calibration_scale <- calibration_sphere$scale
  return(c(calibration_scale, calibration_offset))
}

get_interval <- function(v_bin_data, binfile, start, end, calib, freq) {
  day_data <- read.bin(binfile, start = start, end = end)$data.out
  day_data <- cbind(cbind(day_data[, 1], ((calib[1:3] * day_data[, c('x', 'y', 'z')]) + calib[4:6])), day_data[, 7])
  n_hours <- (nrow(day_data) / freq / 3600)
  non_wear_hours <- mean(rollapply(day_data, (10 * 60 * freq), non_wear_window, by = (freq * 60), by.column = FALSE)) * n_hours
  date <- as.Date(as.POSIXct(day_data[nrow(day_data) / 2, 1], origin = "1970-01-01"))
  day_of_week <- weekdays(date)
  return(list("DataFrame" = day_data, "Date" = date, "WeekDay" = day_of_week, "DayNo" = strsplit(start, " ")[[1]][1], "NonWear" = non_wear_hours))
}

non_wear_window <- function(window) {
  window2_start <- 0
  window2_end <- nrow(window) / 2
  window1_start <- window2_end + 1
  window1_end <- nrow(window)
  window1 <- window[window1_start:window1_end, ]
  window2 <- window[window2_start:window2_end, ]
  temp_in_window1 <- mean(window1[, 5])
  temp_in_window2 <- mean(window2[, 5])
  stds_in_window1 <- apply(window1[, c('x', 'y', 'z')], 2, sd)
  min_in_window1 <- apply(window1[, c('x', 'y', 'z')], 2, min)
  max_in_window1 <- apply(window1[, c('x', 'y', 'z')], 2, max)
  range_in_window1 <- max_in_window1 - min_in_window1
  t0 <- 26
  status <- 0
  stationary <- range_in_window1 < 0.05 & stds_in_window1 < 0.013
  if ((sum(stationary) >= 2) & (temp_in_window1 < t0)) {
    status <- 0
  } else if (temp_in_window1 > t0) {
    status <- 1
  } else {
    if (temp_in_window1 > temp_in_window2) {
      status <- 1
    }
    if (temp_in_window1 < temp_in_window2) {
      status <- 0
    }
  }
  return(status)
}

SedUp <- function(x, fs, option) {
  constant <- 15
  # select model of SedUp for different window size
  if (option == 1) {  # 60secs
    coeffs  <- c(-2.390, 2.542, 42.394)
    uni_thr <- 0.362
    nSec    <- 4
  } else if (option == 2)  {  # 90secs
    coeffs  <- c(-2.420, 2.616, 44.083)
    uni_thr <- 0.372
    nSec    <- 6
  }
  # trim the measurement to match desired window size
  x <- x[1:(floor(length(x)/fs/constant)*fs*constant)]
  # calculate standard deviation
  sdv <- apply(matrix(x, nrow = fs), 2, sd)
  # determine windows for metrics
  window1 <- nSec*constant*fs/2
  window2 <- round(nSec*constant/2)
  # allocate memory for metrics and initiate variables
  medacc  <- integer(length(sdv)/constant)
  medsd   <- integer(length(sdv)/constant)
  j1      <- 0
  j2      <- 0
  # calculate metrics
  for (i in seq(from = 1, to = length(sdv), by = constant)) {
    j1   <- j1 + 1
    i1_1 <- (i - 1) * fs + 1 - window1
    i1_2 <- (i - 1) * fs + 1 + constant * fs + window1
    b    <- i1_1:i1_2
    medacc[j1] <- median(x[b[b > 0 & b < length(x)]], na.rm = TRUE)
    j2   <- j2 + 1
    i2_1 <- (i - 1) - window2
    i2_2 <- (i - 1) + 1 + constant + window2
    b    <- i2_1:i2_2
    medsd[j2] <- median(sdv[b[b >0 & b < length(sdv)]], na.rm = TRUE)
  }
  # use model parameters over metrics
  logit <- coeffs[1] + coeffs[2]*medacc + coeffs[3]*medsd
  phat <- exp(logit) / (1+exp(logit))
  # determine posture
  standing <- matrix(data=NA,nrow=length(phat),ncol=1)
  standing[phat >= uni_thr] <- 1
  standing[phat < uni_thr]  <- 0
  standing <- rep(standing, each=constant*fs)
  return(standing)
}

# Add noise reduction function using a low-pass filter
apply_low_pass_filter <- function(data, cutoff_freq, sampling_rate) {
  nyquist_freq <- sampling_rate / 2
  normalized_cutoff <- cutoff_freq / nyquist_freq
  b <- butter(4, normalized_cutoff, type = "low")
  filtered_data <- filtfilt(b, data)
  return(filtered_data)
}

# Function to determine if the person is awake
is_awake <- function(x, y, z, threshold = 0.01) {
  # Calculate the standard deviation of the movement in each axis
  sd_x <- rollapply(x, width = 50, FUN = sd, align = "center", fill = NA)
  sd_y <- rollapply(y, width = 50, FUN = sd, align = "center", fill = NA)
  sd_z <- rollapply(z, width = 50, FUN = sd, align = "center", fill = NA)
  
  # Determine if there's consistent movement on at least one axis
  awake <- (sd_x > threshold) | (sd_y > threshold) | (sd_z > threshold)
  
  return(as.integer(awake))
}

compute_one_person <- function(binfile) {
  v_bin_data <- read.bin(binfile, virtual = TRUE)
  freq <- as.numeric(as.character(v_bin_data$header['Measurement_Frequency', 1]))
  calib <- calibration(binfile)
  time_stamp_list <- v_bin_data[["page.timestamps"]]
  d1 <- time_stamp_list[1]
  d2 <- time_stamp_list[length(time_stamp_list)]
  dates <- c(seq(d1, d2, by = "day"), d2)
  days <- paste(seq(length(dates)), "00:00:00")
  
  results <- list()
  
  for (i in (1:(length(days) - 1))) {
    start <- days[i]
    end <- days[i + 1]
    D1 <- get_interval(v_bin_data, binfile, start = start, end = end, calib = calib, freq = freq)
    
    if (nrow(D1$DataFrame) > 3000) {
      return(D1)
    }
  }
  
  return(NULL)
}

# Execute the function with the specified binary file
D1 <- compute_one_person(binfile)

# Display the result
D1

# Convert the DataFrame to a DataFrame object
data <- as.data.frame(D1$DataFrame)
colnames(data) <- c("Timestamp", "x", "y", "z", "Temperature")

# Convert the Unix timestamps to POSIXct objects
data$Timestamp <- as.POSIXct(data$Timestamp, origin="1970-01-01", tz="UTC")

# Apply noise reduction to accelerometer data
cutoff_freq <- 2  # Adjust the cutoff frequency as needed
sampling_rate <- 50  # Adjust the sampling rate as needed (in Hz)
data$x <- apply_low_pass_filter(data$x, cutoff_freq, sampling_rate)
data$y <- apply_low_pass_filter(data$y, cutoff_freq, sampling_rate)
data$z <- apply_low_pass_filter(data$z, cutoff_freq, sampling_rate)

# Detect if the person is awake
awake <- is_awake(data$x, data$y, data$z)

# Detect sedentary periods
sed <- SedUp(data$y, sampling_rate, 1)

# Ensure all vectors have the same length
min_length <- min(length(awake), length(sed), nrow(data))
data <- data[1:min_length, ]
awake <- awake[1:min_length]
sed <- sed[1:min_length]

# Add standing/sitting classification to the data
data$Posture <- ifelse(awake == 1 & sed == 1, "Standing", ifelse(awake == 1 & sed == 0, "Sitting", "Sitting"))

# Find indices where the posture changes
posture_change_indices <- which(diff(as.integer(factor(data$Posture))) != 0)

# Add start and end indices for segmenting the data
posture_change_indices <- c(1, posture_change_indices + 1, nrow(data) + 1)

# Plot segments between posture changes and save each plot
for (i in 1:(length(posture_change_indices) - 1)) {
  segment <- data[posture_change_indices[i]:(posture_change_indices[i + 1] - 1), ]
  segment_long <- segment %>%
    select(Timestamp, y, Posture) %>%
    pivot_longer(cols = y, names_to = "Axis", values_to = "Value") %>%
    dplyr::filter(!is.na(Posture))
  
  p <- ggplot(segment_long, aes(x = Timestamp, y = Value, color = Posture)) +
    geom_line() +
    labs(title = paste("Segment", i, "- Posture:", unique(segment$Posture)),
         x = "Time",
         y = "Accelerometer Reading",
         color = "Posture") +
    theme_minimal()
  
  ggsave(filename = paste0("plot_segment_", i, ".png"), plot = p, width = 10, height = 5)
}
