# Load necessary libraries
library(zoo)          # For working with time series data
library(GENEAread)    # For reading GENEActiv .bin files
library(dplyr)        # For data manipulation

# Calibration function
calibration <- function(binfile) {
  # Calibrates the GENEActiv device by reading a .bin file and returns calibration scale and offset
  calibration_sphere <- GENEActiv.calibrate(binfile, printsummary = FALSE)
  calibration_offset <- calibration_sphere$offset
  calibration_scale <- calibration_sphere$scale
  return(c(calibration_scale, calibration_offset))
}

# Function to get interval data
get_interval <- function(v_bin_data, binfile, start, end, calib, freq) {
  # Reads data from a specified time interval and applies calibration
  day_data <- read.bin(binfile, start = start, end = end)$data.out
  day_data <- cbind(cbind(day_data[,1], ((calib[1:3] * day_data[,c('x', 'y', 'z')]) + calib[4:6])), day_data[,7])
  n_hours <- (nrow(day_data) / freq / 3600)
  non_wear_hours <- mean(rollapply(day_data, (10 * 60 * freq), non_wear_window, by = (freq * 60), by.column = FALSE)) * n_hours
  date <- as.Date(as.POSIXct(day_data[nrow(day_data) / 2, 1], origin = "1970-01-01"))
  day_of_week <- weekdays(date)
  return(list("DataFrame" = day_data, "Date" = date, "WeekDay" = day_of_week, "DayNo" = strsplit(start, " ")[[1]][1], "NonWear" = non_wear_hours))
}

# Function to detect non-wear windows
non_wear_window <- function(window) {
  # Identifies periods where the device was likely not worn based on temperature and movement
  window2_start <- 0
  window2_end <- nrow(window) / 2
  window1_start <- window2_end + 1
  window1_end <- nrow(window)
  window1 <- window[window1_start:window1_end,]
  window2 <- window[window2_start:window2_end,]
  temp_in_window1 <- mean(window1[,5])
  temp_in_window2 <- mean(window2[,5])
  stds_in_window1 <- apply(window1[,c('x', 'y', 'z')], 2, sd)
  min_in_window1 <- apply(window1[,c('x', 'y', 'z')], 2, min)
  max_in_window1 <- apply(window1[,c('x', 'y', 'z')], 2, max)
  range_in_window1 <- max_in_window1 - min_in_window1
  t0 <- 26
  status <- 0
  stationairy <- range_in_window1 < 0.05 & stds_in_window1 < 0.013
  if ((sum(stationairy) >= 2) & (temp_in_window1 < t0)) {
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

# Function to add ENMO (Euclidean Norm Minus One) metric
add_ENMO <- function(day_matrix) {
  # Adds the ENMO column to the dataset, which is a common measure of physical activity intensity
  ENMO <- pmax((((rowSums(day_matrix[,c('x', 'y', 'z')]^2))^(1/2)) - 1), 0)
  day_matrix <- cbind(day_matrix, ENMO)
  return(day_matrix)
}

# SedUp function for posture detection
SedUp <- function(y, fs, option) {
  # Detects periods of sedentary behavior or standing using logistic regression model
  constant <- 10
  if (option == 1) {
    coeffs <- c(-2.390, 2.542, 42.394)
    uni_thr <- 0.362
    nSec <- 4
  } else if (option == 2) {
    coeffs <- c(-2.420, 2.616, 44.083)
    uni_thr <- 0.372
    nSec <- 6
  }
  y <- y[1:(floor(length(y) / fs / constant) * fs * constant)]
  sdv <- sapply(split(y, rep(1:(length(y) / fs), each = fs)), sd)
  window1 <- nSec * constant * fs / 2
  window2 <- round(nSec * constant / 2)
  medacc <- integer(length(sdv) / constant)
  medsd <- integer(length(sdv) / constant)
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
  standing <- matrix(data = NA, nrow = length(phat), ncol = 1)
  standing[phat >= uni_thr] <- 1
  standing[phat < uni_thr] <- 0
  standing <- rep(standing, each = constant * fs)
  
  return(standing)
}

# Function to get raw data for bouts of activity
get_raw_data <- function(D, bouts, freq) {
  return(apply(bouts, 1, function(x) { D[x[1]:x[2], ] }))
}

# Function to extract metrics from a raw bout
extract_metrics <- function(raw_bout, freq) {
  # Extracts various metrics from each bout of activity, such as volume, intensity, and percentiles
  volume <- sum(raw_bout[,"ENMO"]) / freq
  intensity <- mean(raw_bout[,"ENMO"])
  timestamp <- strftime(format(as.POSIXct(raw_bout[1,1], origin = "1970-01-01"), tz = "UTC"), format = "%H:%M:%S")
  duration <- nrow(raw_bout) / freq
  percentiles <- as.vector((quantile(raw_bout[,'ENMO'], probs = seq(0, 1, 0.05))))
  sd <- sd(raw_bout[,"ENMO"])
  wind_sd <- wind_SD(raw_bout[,"ENMO"])
  list_to_return <- list(
    "Volume" = volume,
    "Intensity" = intensity,
    "Time" = timestamp,
    "Duration" = duration,
    "Percentile_5" = percentiles[2],
    "Percentile_10" = percentiles[3],
    "Percentile_15" = percentiles[4],
    "Percentile_20" = percentiles[5],
    "Percentile_25" = percentiles[6],
    "Percentile_30" = percentiles[7],
    "Percentile_35" = percentiles[8],
    "Percentile_40" = percentiles[9],
    "Percentile_45" = percentiles[10],
    "Percentile_50" = percentiles[11],
    "Percentile_55" = percentiles[12],
    "Percentile_60" = percentiles[13],
    "Percentile_65" = percentiles[14],
    "Percentile_70" = percentiles[15],
    "Percentile_75" = percentiles[16],
    "Percentile_80" = percentiles[17],
    "Percentile_85" = percentiles[18],
    "Percentile_90" = percentiles[19],
    "Percentile_95" = percentiles[20],
    "Max" = percentiles[21],
    "Min" = percentiles[1],
    "SD" = sd,
    "Windsorized_SD" = wind_sd
  )
  return(list_to_return)
}

# Function for Windsorized standard deviation
wind_SD <- function(vect) {
  # Computes Windsorized standard deviation, which reduces the influence of outliers
  top <- as.numeric(quantile(vect, probs = c(0.95)))
  bottom <- as.numeric(quantile(vect, probs = c(0.05)))
  wind_vect <- pmin(pmax(vect, bottom), top)
  return(sd(wind_vect))
}

# Function to compute bouts of activity
compute_bouts <- function(D, freq, timestamps) {
  # Detects and computes periods of activity and inactivity based on movement data
  X <- SedUp(D[,3], freq, 1)
  
  times <- as.numeric(format(as.POSIXct(timestamps, origin = "1970-01-01"), "%H"))
  X[times >= 0 & times < 6] <- 0
  
  rle_bouts <- rle(X)
  start <- head(cumsum(c(0, rle_bouts$lengths)), -1)
  end <- cumsum(rle_bouts$lengths)
  
  return(data.frame(start = start, end = end, lab = rle_bouts$values))
}

# Function to create a ghost table (empty template)
create_ghost_table <- function(row, date) {
  # Creates a template table filled with zeros, used for initializing a new day
  temp_row <- row
  temp_row <- row
  temp_row[seq(27)] <- 0
  ghost_tab <- do.call('rbind', replicate(24, temp_row, simplify = FALSE))
  times <- c("00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00", "05:00:00", "06:00:00", "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00", "13:00:00", "14:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00", "19:00:00", "20:00:00", "21:00:00", "22:00:00", "23:00:00")
  ghost_tab['Time'] <- times
  ghost_tab['Date'] <- date
  ghost_tab['WeekDay'] <- weekdays(as.Date(date))
  ghost_tab['DayNo'] <- 0
  ghost_tab['NonWear'] <- 0
  return(ghost_tab)
}

# Function to compute data for one person
compute_one_person <- function(file_name, binfolder, outfolder) {
  # Reads the binary data for one person, processes it, and saves results as CSV files
  binfile <- file.path(binfolder, file_name)
  file_name_without_bin <- strsplit(file_name, '.bin')[[1]]
  specific_out_folder <- file.path(outfolder, file_name_without_bin)
  dir.create(specific_out_folder, recursive = TRUE)
  v_bin_data <- read.bin(binfile, virtual = TRUE)
  freq <- as.numeric(as.character(v_bin_data$header['Measurement_Frequency', 1]))
  calib <- calibration(binfile)
  time_stamp_list <- v_bin_data[["page.timestamps"]]
  d1 <- time_stamp_list[1]
  d2 <- time_stamp_list[length(time_stamp_list)]
  dates <- c(seq(d1, d2, by = "day"), d2)
  days <- paste(seq(length(dates)), "00:00:00")
  filenames <- paste(paste('Day', seq(length(dates)), sep = ''), ".csv", sep = "")
  
  # Create a list to store the dataframes for each person
  person_dataframes <- list()
  
  for (i in (1:(length(filenames) - 1))) {
    print(i)
    start <- days[i]
    end <- days[i + 1]
    D1 <- get_interval(v_bin_data, binfile, start = start, end = end, calib = calib, freq = freq)
    if (nrow(D1$DataFrame) > 3000) {
      bouts <- compute_bouts(D1$DataFrame, freq, timestamps = D1$DataFrame[,1])
      ENMO_tab <- add_ENMO(D1$DataFrame)
      raw_bouts <- get_raw_data(ENMO_tab, bouts, freq)
      if (nrow(bouts) > 1) {
        list_of_bouts <- lapply(raw_bouts, extract_metrics, freq = freq)
        table_of_bouts <- t(do.call("cbind", list_of_bouts))
        rownames(table_of_bouts) <- NULL
        table_of_bouts <- cbind(table_of_bouts, "Date" = as.character(D1$Date))
        table_of_bouts <- cbind(table_of_bouts, "WeekDay" = as.character(D1$WeekDay))
        table_of_bouts <- cbind(table_of_bouts, "DayNo" = D1$DayNo)
        table_of_bouts <- cbind(table_of_bouts, "NonWear" = D1$NonWear)
        table_of_bouts <- cbind(table_of_bouts, "FILE" = file_name)
        table_of_bouts <- cbind(table_of_bouts, "Active" = bouts[,'lab'])
      }
      if (nrow(bouts) == 1) {
        table_of_bouts <- extract_metrics(ENMO_tab, freq)
        table_of_bouts["Date"] <- as.character(D1$Date)
        table_of_bouts["WeekDay"] <- as.character(D1$WeekDay)
        table_of_bouts["DayNo"] <- D1$DayNo
        table_of_bouts["NonWear"] <- D1$NonWear
        table_of_bouts["FILE"] <- file_name
        table_of_bouts["Active"] <- bouts[,'lab']
        table_of_bouts <- data.frame(table_of_bouts)
      }
      table_of_bouts <- na.omit(table_of_bouts) 
      filepath <- file.path(specific_out_folder, filenames[i])
      write.csv(table_of_bouts, filepath, row.names = FALSE)
      # Store each day's dataframe in the list
      person_dataframes[[paste0("Day", i)]] <- table_of_bouts
    }
  }
  temp <- list.files(specific_out_folder, pattern = "*.csv")
  myfiles <- lapply(file.path(specific_out_folder, temp), read.csv)
  complete_file <- do.call('rbind', myfiles)
  complete_file[,'DayNo'] <- complete_file[,'DayNo'] - (min(complete_file[,'DayNo']) - 1)
  days_in_study <- seq(as.Date(min(complete_file[,'Date'])), as.Date(max(complete_file[,'Date'])), by = 'day')
  complete_file <- na.omit(complete_file)
  write.csv(complete_file, file.path(specific_out_folder, "all_days.csv"), row.names = FALSE)
  
  # Return the list of dataframes for this person
  return(person_dataframes)
}

# Function to compute data for all persons
compute_for_all <- function(binfolder, outfolder) {
  # Processes all .bin files in a folder and aggregates the results
  all_binfiles <- list.files(binfolder, pattern = "*.bin")
  all_dataframes <- list()  # Store all dataframes from all persons
  
  for (i in all_binfiles) {
    person_dataframes <- compute_one_person(i, binfolder, outfolder)
    all_dataframes[[i]] <- person_dataframes  # Store each person's dataframes in the list
  }
  
  # Combine all individual 'all_days.csv' files into a single dataframe
  temp <- list.files(outfolder, pattern = "*all_days.csv", recursive = TRUE)
  myfiles <- lapply(file.path(outfolder, temp), read.csv)
  complete_file <- do.call('rbind', myfiles)
  write.csv(complete_file, file.path(outfolder, "all_people.csv"), row.names = FALSE)
  
  # Return a list containing all dataframes and the output folder
  return(list("dataframes" = all_dataframes, "output_folder" = outfolder))
}

# Function to calculate burstiness for a single dataframe
calculate_burstiness <- function(df) {
  df <- as.data.frame(df)
  
  required_columns <- c("Date", "Time", "Active")
  if (!all(required_columns %in% names(df))) {
    message("Skipping dataframe: Missing required columns.")
    return(NULL)
  }
  
  filtered_data <- df %>% filter(Active == 1)
  
  filtered_data$DateTime <- as.POSIXct(paste(filtered_data$Date, filtered_data$Time), format = "%Y-%m-%d %H:%M:%S")
  
  filtered_data <- filtered_data %>% distinct()
  
  filtered_data <- filtered_data %>%
    arrange(DateTime) %>%
    mutate(Time_Diff = as.numeric(difftime(DateTime, lag(DateTime), units = "secs")))
  
  filtered_data <- filtered_data %>% filter(!is.na(Time_Diff))
  
  if (nrow(filtered_data) < 2) {
    message("Not enough data to calculate burstiness.")
    return(NULL)
  }
  
  mean_interevent <- mean(filtered_data$Time_Diff)
  sd_interevent <- sd(filtered_data$Time_Diff)
  
  burstiness <- (sd_interevent - mean_interevent) / (sd_interevent + mean_interevent)
  
  r <- sd_interevent / mean_interevent
  n <- nrow(filtered_data)
  
  An_r <- function(r, n) {
    sqrt_n_plus_1 <- sqrt(n + 1)
    sqrt_n_minus_1 <- sqrt(n - 1)
    (sqrt_n_plus_1 * r - sqrt_n_minus_1) / ((sqrt_n_plus_1 - 2) * r + sqrt_n_minus_1)
  }
  
  alternative_burstiness <- An_r(r, n)
  
  return(list(
    Burstiness = burstiness,
    Alternative_Burstiness = alternative_burstiness,
    Number_of_Events = n,
    Mean_Interevent_Time = mean_interevent,
    SD_Interevent_Time = sd_interevent,
    Coefficient_of_Variation = r
  ))
}

# Function to measure burstiness for all dataframes
measure_burstiness <- function(results) {
  output_folder <- results$output_folder
  dataframes <- results$dataframes
  
  burstiness_folder <- file.path(output_folder, "burstiness_measures")
  if (!dir.exists(burstiness_folder)) {
    dir.create(burstiness_folder, recursive = TRUE)
  }
  
  burstiness_results <- lapply(names(dataframes), function(person) {
    person_dataframes <- dataframes[[person]]
    person_burstiness <- lapply(names(person_dataframes), function(day) {
      df <- person_dataframes[[day]]
      if (is.null(df)) {
        message("Skipping null dataframe.")
        return(NULL)
      }
      df <- as.data.frame(df)
      burstiness_metrics <- calculate_burstiness(df)
      if (!is.null(burstiness_metrics)) {
        return(data.frame(
          Person = person,
          Day = day,
          Burstiness = burstiness_metrics$Burstiness,
          Alternative_Burstiness = burstiness_metrics$Alternative_Burstiness,
          Number_of_Events = burstiness_metrics$Number_of_Events,
          Mean_Interevent_Time = burstiness_metrics$Mean_Interevent_Time,
          SD_Interevent_Time = burstiness_metrics$SD_Interevent_Time,
          Coefficient_of_Variation = burstiness_metrics$Coefficient_of_Variation
        ))
      }
    }) %>% bind_rows()
    
    if (!is.null(person_burstiness) && nrow(person_burstiness) > 0) {
      write.csv(person_burstiness, file.path(burstiness_folder, paste0(person, "_burstiness_metrics.csv")), row.names = FALSE)
    }
    
    return(person_burstiness)
  }) %>% bind_rows()
  
  write.csv(burstiness_results, file.path(output_folder, "all_burstiness_metrics.csv"), row.names = FALSE)
  
  return(burstiness_results)
}

# Example call to the function
result <- compute_for_all("D:/Downloads/in", "D:/Downloads/out/fulloutv1")
burstiness_metrics <- measure_burstiness(result)
