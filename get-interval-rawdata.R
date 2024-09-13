# Load necessary libraries
library(zoo)
library(GENEAread)
library(ggplot2)
library(dplyr)
library(tidyr)
library(signal)
library(dplyr)
library(ggplot2)
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

compute_all_days <- function(binfile) {
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
    day_data <- get_interval(v_bin_data, binfile, start = start, end = end, calib = calib, freq = freq)
    results[[i]] <- day_data
  }
  
  return(results)
}

# Execute the function with the specified binary file
all_days_data <- compute_all_days(binfile)

# Combine data from all days into one DataFrame
all_days_df <- do.call(rbind, lapply(all_days_data, function(x) as.data.frame(x$DataFrame)))
colnames(all_days_df) <- c("Timestamp", "x", "y", "z", "Temperature")
all_days_df$Timestamp <- as.POSIXct(all_days_df$Timestamp, origin="1970-01-01", tz="UTC")

# Apply filtering and compute metrics
cutoff_freq <- 2  # Adjust the cutoff frequency as needed
sampling_rate <- 50  # Adjust the sampling rate as needed (in Hz)
all_days_df$x <- apply_low_pass_filter(all_days_df$x, cutoff_freq, sampling_rate)
all_days_df$y <- apply_low_pass_filter(all_days_df$y, cutoff_freq, sampling_rate)
all_days_df$z <- apply_low_pass_filter(all_days_df$z, cutoff_freq, sampling_rate)

# Generate awake and posture data
all_days_df$Awake <- is_awake(all_days_df$x, all_days_df$y, all_days_df$z)

# Plotting the data
p_all_days <- ggplot(all_days_df, aes(x = Timestamp, y = y, color = as.factor(Awake))) +
  geom_line() +
  labs(title = "Accelerometer Data Over All Days",
       x = "Time",
       y = "Accelerometer Reading (y-axis)",
       color = "Awake Status") +
  theme_minimal()

# Save the plot
ggsave(filename = "all_days_plot.png", plot = p_all_days, width = 12, height = 6)
