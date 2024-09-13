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
additional_csv_file_path <- "D:/Downloads/out/004/P004_031202_2021-01-12 15-53-17/all_days.csv"
csv_file_path <- "D:/Downloads/out/004/P004_031202_2021-01-12 15-53-17/all_days.csv"
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

# Ensure the timestamps from the CSV file match the same day as the binary data
start_time <- min(timestamp_data$Timestamp, na.rm = TRUE) # Adjusted start time to the first timestamp in the CSV
end_time <- start_time + 12*60*60

first_day_data <- calibrated_data %>%
  filter(Timestamp >= start_time & Timestamp < end_time)

# Filter the timestamps for the same day and the first 12 hours
timestamp_data <- timestamp_data %>%
  filter(Timestamp >= start_time & Timestamp < end_time)

timestamp_data1 <- timestamp_data1 %>%
  filter(Timestamp >= start_time & Timestamp < end_time)

# Create a column indicating the segment ID for alternating colors
first_day_data <- first_day_data %>%
  mutate(Segment = findInterval(Timestamp, timestamp_data$Timestamp))

# Create the background rectangles
background_rects <- timestamp_data1 %>%
  mutate(end = lead(Timestamp, default = max(first_day_data$Timestamp)),
         fill = row_number() %% 2 == 1) %>%
  filter(fill) # Filter to only include the "filled" segments

# Ensure the ymin and ymax cover the entire y-axis range
y_min <- min(first_day_data$X, na.rm = TRUE) - 1 # Adding buffer to ensure visibility
y_max <- max(first_day_data$X, na.rm = TRUE) + 1

# Plot the X component over the first 12 hours with alternating colors
plot_x <- ggplot(first_day_data) +
  geom_rect(data = background_rects,
            aes(xmin = Timestamp, xmax = end, ymin = -Inf, ymax = Inf, fill = "Activity Segment"),
            alpha = 0.4) +  # Bright fill color
  geom_line(aes(x = Timestamp, y = X, group = Segment, color = as.factor(Segment %% 2))) +
  scale_fill_manual(values = c("Activity Segment" = "blue")) +
  scale_color_manual(values = c("0" = "black", "1" = "red")) +
  labs(title = "X Component Over Time", x = "Time", y = "X Acceleration") +
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
ggsave(filename = "D:/Downloads/X_Component_Over_Time.png", plot = plot_x, width = 10, height = 6)

# Display the first few rows
head(first_day_data)
