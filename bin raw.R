# Load necessary libraries
library(data.table)
library(ggplot2)

# Function to read binary file and extract raw data
extract_raw_data <- function(bin_file_path) {
  # Open the binary file for reading
  con <- file(bin_file_path, "rb")
  
  # Read the first 100 lines (metadata)
  metadata <- readBin(con, what = "raw", n = 100 * 128)  # assuming each line is 128 bytes, adjust as needed
  
  # Skip the first 100 lines (metadata)
  seek(con, 100 * 128, "start")
  
  # Initialize vectors for storing data
  time_stamps <- numeric()
  x_axis <- numeric()
  y_axis <- numeric()
  z_axis <- numeric()
  light_level <- numeric()
  button <- integer()
  temperature <- numeric()
  
  # Read records in chunks for faster processing
  chunk_size <- 10000  # Adjust chunk size based on available memory
  repeat {
    # Read a chunk of data
    time_stamp_chunk <- readBin(con, "double", size = 8, n = chunk_size, endian = "little")
    if (length(time_stamp_chunk) == 0) break  # End of file reached
    
    x_axis_chunk <- readBin(con, "double", size = 8, n = chunk_size, endian = "little")
    y_axis_chunk <- readBin(con, "double", size = 8, n = chunk_size, endian = "little")
    z_axis_chunk <- readBin(con, "double", size = 8, n = chunk_size, endian = "little")
    light_level_chunk <- readBin(con, "double", size = 8, n = chunk_size, endian = "little")
    button_chunk <- readBin(con, "integer", size = 1, n = chunk_size, signed = TRUE)
    temperature_chunk <- readBin(con, "double", size = 8, n = chunk_size, endian = "little")
    
    # Append to vectors
    time_stamps <- c(time_stamps, time_stamp_chunk)
    x_axis <- c(x_axis, x_axis_chunk)
    y_axis <- c(y_axis, y_axis_chunk)
    z_axis <- c(z_axis, z_axis_chunk)
    light_level <- c(light_level, light_level_chunk)
    button <- c(button, button_chunk)
    temperature <- c(temperature, temperature_chunk)
  }
  
  # Close the connection
  close(con)
  
  # Combine all data into a data.table
  raw_data <- data.table(
    TimeStamp = time_stamps,
    X_Axis = x_axis,
    Y_Axis = y_axis,
    Z_Axis = z_axis,
    Light_Level = light_level,
    Button = button,
    Temperature = temperature
  )
  
  return(raw_data)
}

# Example usage
bin_file_path <- "D:/Downloads/in/P021_027734_2021-02-10 15-37-24.bin"
raw_data <- extract_raw_data(bin_file_path)

# Trim the data to the first hour (3600 seconds)
first_hour_data <- raw_data[100:300, ]

# Display the trimmed data
print(first_hour_data)

# Plot the X, Y, and Z axis data for the first hour
ggplot(first_hour_data, aes(x = TimeStamp)) +
 # geom_line(aes(y = X_Axis, color = "X Axis")) +
 # geom_line(aes(y = Y_Axis, color = "Y Axis")) +
  geom_line(aes(y = Z_Axis, color = "Z Axis")) +
  labs(title = "Accelerometer Data (First Hour)", x = "Time (seconds)", y = "Acceleration (g)") +
  scale_color_manual(name = "Axis", values = c("X Axis" = "red", "Y Axis" = "green", "Z Axis" = "blue")) +
  theme_minimal()
