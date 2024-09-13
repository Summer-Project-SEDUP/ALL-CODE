# Load necessary libraries
library(ggplot2)

# Assuming your data is saved in a data frame called 'data'
# Read the data from the CSV file
data <- read.csv("D:/Downloads/out/compfiles/P016_040173_2021-02-10 15-07-55/all_days.csv")

# Convert the Date and Time columns to a single Datetime column
data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")

# Assuming you want to start from the earliest datetime in the data
start_time <- min(data$Datetime) + 48*60*60

# Define the end time as 48 hours after the start time
end_time <- start_time + 24*60*60  # 48 hours in seconds

# Filter the data for the 48-hour period starting from the start_time
day3_data <- subset(data, Datetime >= start_time & Datetime <= end_time)

# Plot the Active column over time for the 48-hour period
ggplot(day3_data, aes(x = Datetime, y = Active)) +
  geom_line() +
  scale_x_datetime(limits = c(start_time, end_time), date_labels = "%H:%M") +
  labs(title = "Active Over Time for 48 Hours Starting from Start Time", x = "Time", y = "Active") +
  theme_minimal()
