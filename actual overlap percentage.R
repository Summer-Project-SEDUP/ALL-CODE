# Load the required libraries
library(dplyr)
library(lubridate)
library(tidyr)

# Define the directories for the first set
upright_bouts <- list(
  "P002" = "D:/Downloads/actipal thing/P002-AP842603 204at 12Dec20 12-00am for 9d 23h 59m-VANE-PB08101256-UprightBouts.csv",
  "P003" = "D:/Downloads/actipal thing/P003-AP842822 204at 12Dec20 12-00am for 9d 23h 59m-VANE-PB08101256-UprightBouts.csv",
  "P004" = "D:/Downloads/actipal thing/P004-AP842586 204at 12Dec20 12-00am for 9d 23h 59m-VANE-PB08101256-UprightBouts.csv",
  "P005" = "D:/Downloads/actipal thing/P005-AP842583 204at 12Dec20 12-00am for 8d 16h 7m-VANE-PB08101256-UprightBouts.csv",
  "P006" = "D:/Downloads/actipal thing/P006-AP842589 204at 12Dec20 12-00am for 9d 23h 59m-VANE-PB08101256-UprightBouts.csv",
  "P007" = "D:/Downloads/actipal thing/P007-AP842604 204at 12Dec20 12-00am for 9d 23h 59m-VANE-PB08101256-UprightBouts.csv",
  "P008" = "D:/Downloads/actipal thing/P008-AP842822 204at 21Jan21 12-00am for 11d 23h 59m-VANE-PB08101256-UprightBouts.csv",
  "P009" = "D:/Downloads/actipal thing/P009-AP842590 204at 21Jan21 12-00am for 11d 23h 59m-VANE-PB08101256-UprightBouts.csv",
  "P011" = "D:/Downloads/actipal thing/P011-AP842604 204at 21Jan21 12-00am for 9d 5h 35m-VANE-PB08101256-UprightBouts.csv",
  "P012" = "D:/Downloads/actipal thing/P012-AP842591 204at 21Jan21 12-00am for 11d 23h 59m-VANE-PB08101256-UprightBouts.csv",
  "P013" = "D:/Downloads/actipal thing/P013-AP842589 204at 21Jan21 12-00am for 11d 23h 59m-VANE-PB08101256-UprightBouts.csv"
)

# Define the directories for the second set
all_days <- list(
  "P002" = "D:/Downloads/out/fulloutv1/P002_031145_2021-01-12 15-20-00/all_days.csv",
  "P003" = "D:/Downloads/out/fulloutv1/P003_031530_2021-01-12 15-51-24/all_days.csv",
  "P004" = "D:/Downloads/out/fulloutv1/P004_031202_2021-01-12 15-53-17/all_days.csv",
  "P005" = "D:/Downloads/out/fulloutv1/P005_031198_2021-01-12 15-53-24/all_days.csv",
  "P006" = "D:/Downloads/out/fulloutv1/P006__031155_2021-01-12 16-09-41/all_days.csv",
  "P007" = "D:/Downloads/out/fulloutv1/P007__035451_2021-01-12 16-09-49/all_days.csv",
  "P008" = "D:/Downloads/out/fulloutv1/P008_031155_2021-02-10 13-46-28/all_days.csv",
  "P009" = "D:/Downloads/out/fulloutv1/P009_027708_2021-02-10 13-46-57/all_days.csv",
  "P011" = "D:/Downloads/out/fulloutv1/P011_031166_2021-02-10 13-46-43/all_days.csv",
  "P012" = "D:/Downloads/out/fulloutv1/P012_034544_2021-02-10 14-16-39/all_days.csv",
  "P013" = "D:/Downloads/out/fulloutv1/P013_031187_2021-02-10 14-16-32/all_days.csv"
)

# Combine both sets into a list of lists
combined_list <- list(
  "P002" = list(upright_bouts = upright_bouts$P002, all_days = all_days$P002),
  "P003" = list(upright_bouts = upright_bouts$P003, all_days = all_days$P003),
  "P004" = list(upright_bouts = upright_bouts$P004, all_days = all_days$P004),
  "P005" = list(upright_bouts = upright_bouts$P005, all_days = all_days$P005),
  "P006" = list(upright_bouts = upright_bouts$P006, all_days = all_days$P006),
  "P007" = list(upright_bouts = upright_bouts$P007, all_days = all_days$P007),
  "P008" = list(upright_bouts = upright_bouts$P008, all_days = all_days$P008),
  "P009" = list(upright_bouts = upright_bouts$P009, all_days = all_days$P009),
  "P011" = list(upright_bouts = upright_bouts$P011, all_days = all_days$P011),
  "P012" = list(upright_bouts = upright_bouts$P012, all_days = all_days$P012),
  "P013" = list(upright_bouts = upright_bouts$P013, all_days = all_days$P013)
)

# Create an empty list to store the results for each participant
results_list <- list()

# Loop through each participant in the combined_list
for (participant in names(combined_list)) {
  
  # Get the file paths for the current participant
  upright_bouts_path <- combined_list[[participant]]$upright_bouts
  all_days_path <- combined_list[[participant]]$all_days
  
  # Load the data from the CSV files for the current participant
  df2 <- read.csv(all_days_path)
  df1 <- read.csv(upright_bouts_path, sep = ";", header = FALSE)
  
  # Clean and prepare df1
  df1 <- df1[-1, ]  # Remove the first row with "sep="
  colnames(df1) <- df1[1, ]  # Use the second row as headers
  df1 <- df1[-1, ]  # Remove the row now used as headers
  df1 <- df1[, -ncol(df1)]  # Remove the last column if it's NA
  rownames(df1) <- NULL  # Reset row names
  
  df1 <- df1[, c("Time(approx)", "Event Type", "Duration (s)")]
  df1$DateTime <- as.POSIXct(df1$`Time(approx)`, format="%Y-%m-%d %H:%M:%S")
  df1 <- df1[, !names(df1) %in% c("Time(approx)")]
  
  # Clean and prepare df2
  df2 <- df2[, c("Time", "Duration", "Active", "Date")]
  df2$DateTime <- as.POSIXct(paste(df2$Date, df2$Time))
  
  # Combine DateTime columns from both dataframes and remove duplicates
  df_combined_time <- bind_rows(
    df1 %>% select(DateTime),
    df2 %>% select(DateTime)
  ) %>% 
    distinct() %>% 
    arrange(DateTime)
  
  # Add 'Active' column from df2 to df_combined_time, filling NAs
  df_combined_time <- df_combined_time %>%
    left_join(df2 %>% select(DateTime, Active), by = "DateTime") %>%
    fill(Active, .direction = "down")
  
  # Add 'Event Type' column from df1 to df_combined_time, filling NAs
  df_combined_time <- df_combined_time %>%
    left_join(df1 %>% select(DateTime, `Event Type`), by = "DateTime") %>%
    fill(`Event Type`, .direction = "down")
  
  # Drop any rows that contain NA values
  df_combined_time <- df_combined_time %>%
    drop_na()
  
  # Calculate the time difference between each DateTime to get the duration of each period
  df_combined_time <- df_combined_time %>%
    arrange(DateTime) %>%
    mutate(Duration = as.numeric(difftime(lead(DateTime), DateTime, units = "secs")))
  
  # Remove the last row which has NA for duration (since there's no next row to compare)
  df_combined_time <- df_combined_time %>% filter(!is.na(Duration))
  
  # Identify rows where Active and Event Type are the same
  df_combined_time <- df_combined_time %>%
    mutate(Match = ifelse(Active == `Event Type`, TRUE, FALSE))
  
  # Calculate the total time
  total_time <- sum(df_combined_time$Duration)
  
  # Calculate the time spent with matching numbers
  matching_time <- sum(df_combined_time$Duration[df_combined_time$Match])
  
  # Calculate the percentage of time with matching numbers
  matching_percentage <- (matching_time / total_time) * 100
  
  # Store the result in the results_list
  results_list[[participant]] <- matching_percentage
}

# Calculate the average matching percentage across all participants
average_matching_percentage <- mean(unlist(results_list))

# Convert the results list to a data frame
results_df <- data.frame(
  Participant = names(results_list),
  Matching_Percentage = unlist(results_list)
)

# Add the average matching percentage as a new column to the data frame
results_df$Average <- average_matching_percentage

# Display the results data frame
print(results_df)
