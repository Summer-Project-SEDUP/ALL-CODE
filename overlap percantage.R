# Load the required libraries
library(dplyr)
library(lubridate)

# Load the data from the CSV files
df2 <- read.csv("D:/Downloads/out/compfiles/P002_031145_2021-01-12 15-20-00/all_days.csv")
df1 <- read.csv("D:/Downloads/actipal thing/P002-AP842603 204at 12Dec20 12-00am for 9d 23h 59m-VANE-PB08101256-UprightBouts.csv", 
                sep = ";", header = FALSE)

# Clean and prepare df1
df1 <- df1[-1, ]  # Remove the first row with "sep="
colnames(df1) <- df1[1, ]  # Use the second row as headers
df1 <- df1[-1, ]  # Remove the row now used as headers
df1 <- df1[, -ncol(df1)]  # Remove the last column if it's NA
rownames(df1) <- NULL  # Reset row names

# Select relevant columns
df1 <- df1[, c("Time(approx)", "Event Type", "Duration (s)")]
df2 <- df2[, c("Time", "Duration", "Active", "Date")]

# Create DateTime columns
df1$Date <- as.Date(df1$`Time(approx)`, format="%Y-%m-%d %H:%M:%S")
df1$Time <- format(as.POSIXct(df1$`Time(approx)`), format="%H:%M:%S")
df1$DateTime <- as.POSIXct(paste(df1$Date, df1$Time))

df2$DateTime <- as.POSIXct(paste(df2$Date, df2$Time))

# Find common times between the two dataframes
common_times <- intersect(df1$DateTime, df2$DateTime)

# Filter the data frames to include only these common times
df1_common <- df1[df1$DateTime %in% common_times, ]
df2_common <- df2[df2$DateTime %in% common_times, ]

# Ensure that the rows are sorted by DateTime
df1_common <- df1_common[order(df1_common$DateTime), ]
df2_common <- df2_common[order(df2_common$DateTime), ]

# Compare the values where both are 1
both_one <- df1_common$`Event Type` == 1 & df2_common$Active == 1

# Calculate the percentage
percent_both_one <- (sum(both_one) / length(both_one)) * 100

# Print the result
print(percent_both_one)
