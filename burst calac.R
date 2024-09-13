# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Read the CSV file
file_path <- "D:/Downloads/out/P021_027734_2021-02-10 15-37-24/all_days.csv"
data <- read.csv(file_path)

# Filter the data to include only rows where Active is 1
filtered_data <- data %>% filter(Active == 1)

# Combine Date and Time columns into a single datetime column
filtered_data$DateTime <- as.POSIXct(paste(filtered_data$Date, filtered_data$Time), format="%Y-%m-%d %H:%M:%S")

# Check for duplicates
duplicate_count <- sum(duplicated(filtered_data))

# Print the number of duplicates
print(paste("Number of duplicate rows: ", duplicate_count))

# Remove duplicates
filtered_data <- filtered_data %>% distinct()

# Calculate the time difference between consecutive rows
filtered_data <- filtered_data %>%
  arrange(DateTime) %>%  # Ensure the data is sorted by DateTime
  mutate(Time_Diff = as.numeric(difftime(DateTime, lag(DateTime), units = "secs")))

# Remove the first row since it has no valid time difference
filtered_data <- filtered_data %>% filter(!is.na(Time_Diff))

# Calculate mean and standard deviation of interevent times
mean_interevent <- mean(filtered_data$Time_Diff)
sd_interevent <- sd(filtered_data$Time_Diff)

# Calculate burstiness parameter
burstiness <- (sd_interevent - mean_interevent) / (sd_interevent + mean_interevent)

# Print burstiness result
print(paste("Burstiness parameter: ", burstiness))

# Calculate coefficient of variation
r <- sd_interevent / mean_interevent

# Number of events
n <- nrow(filtered_data)

# Calculate alternative burstiness measure An(r)
An_r <- function(r, n) {
  sqrt_n_plus_1 <- sqrt(n + 1)
  sqrt_n_minus_1 <- sqrt(n - 1)
  (sqrt_n_plus_1 * r - sqrt_n_minus_1) / ((sqrt_n_plus_1 - 2) * r + sqrt_n_minus_1)
}

alternative_burstiness <- An_r(r, n)

# Print alternative burstiness measure
print(paste("Alternative burstiness measure: ", alternative_burstiness))

# Plot a histogram of interevent times
ggplot(filtered_data, aes(x = Time_Diff)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Interevent Times",
       x = "Interevent Time (seconds)",
       y = "Frequency") +
  theme_minimal()
