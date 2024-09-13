
calibration<-function(binfile){
  
  calibration_sphere = GENEActiv.calibrate(binfile, printsummary = FALSE)
  
  calibration_offset = calibration_sphere$offset 
  
  calibration_scale = calibration_sphere$scale

  
  return(c(calibration_scale, calibration_offset))
  
  
}



get_interval<-function(v_bin_data, binfile, start, end, calib, freq){
  

  
  
  
  day_data = read.bin(binfile, start = start, end = end)$data.out
  
  day_data = cbind(cbind(day_data[,1],((calib[1:3]*day_data[,c('x', 'y','z')]) + calib[4:6])), day_data[,7])
  
  n_hours = (nrow(day_data)/freq/3600)
  
  
  
  non_wear_hours = mean(rollapply(day_data, (10*60*freq), non_wear_window, by = (freq*60), by.column = FALSE)) * n_hours
  
  date = as.Date(as.POSIXct(day_data[nrow(day_data)/2,1],  origin="1970-01-01"))
  
  day_of_week = weekdays(date)

  
  return(list("DataFrame" = day_data, "Date" = date, "WeekDay" = day_of_week, "DayNo"=strsplit(start, " ")[[1]][1], "NonWear" = non_wear_hours))
  
}









non_wear_window<-function(window){
  
  window2_start = 0
  
  window2_end = nrow(window)/2
  
  window1_start = window2_end+1
  
  window1_end = nrow(window)
  
  window1 = window[window1_start:window1_end,]
  
  window2 = window[window2_start:window2_end,]
  
  
  
  temp_in_window1 = mean(window1[,5])
  
  temp_in_window2 = mean(window2[,5])
  
  
  
  stds_in_window1 = apply(window1[,c('x', 'y', 'z')], 2, sd)
  
  min_in_window1 = apply(window1[,c('x', 'y', 'z')], 2, min)
  
  max_in_window1 = apply(window1[,c('x', 'y', 'z')], 2, max)
  
  range_in_window1 = max_in_window1 - min_in_window1
  
  
  
  t0 = 26
  
  
  
  status = 0
  
  
  
  stationairy = range_in_window1<0.05 & stds_in_window1<0.013
  
  
  
  if ((sum(stationairy)>=2) & (temp_in_window1<t0)){
    
    status = 0
    
  }
  
  else if (temp_in_window1>t0){
    
    status = 1
    
  }
  
  else{
    
    if (temp_in_window1>temp_in_window2){
      
      status = 1
      
    }
    
    if (temp_in_window1<temp_in_window2){
      
      status = 0
      
    }
    
  }
  
  return(status)
}













add_ENMO<-function(day_matrix){
  
  ENMO = pmax((((rowSums(day_matrix[,c('x','y','z')]**2))**(1/2))-1), 0)
  
  day_matrix = cbind(day_matrix, ENMO)
  
  return(day_matrix)
  
}









SedUp <- function(y, fs, option) {
  
  constant <- 10
  
  
  
  # select model of SedUp for different window size
  
  if (option == 1) {  
    
    coeffs  <- c(-2.390, 2.542, 42.394)
    
    uni_thr <- 0.362
    
    nSec    <- 4
    
  } else if (option == 2)  {  
    
    coeffs  <- c(-2.420, 2.616, 44.083)
    
    uni_thr <- 0.372
    
    nSec    <- 6
    
  }
  
  
  # trim the measurement to match desired window size
  
  y <- y[1:(floor(length(y)/fs/constant)*fs*constant)]
  
  # y not x ? 
  
  # calculate standard deviation
  
  sdv <- apply(matrix(y, nrow = fs), 2, sd)

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
    
    medacc[j1] <- median(y[b[b > 0 & b < length(y)]], na.rm = TRUE)
    
    
    
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













get_raw_data<-function(D, bouts, freq){
  
  return(apply(bouts,1, function(x){D[x[1]:x[2],]}))
  
}





extract_metrics<-function(raw_bout, freq){
  
  #raw_bout = unname(raw_bout)[[1]]
  
  volume = sum(raw_bout[,"ENMO"])/freq
  
  intensity = mean(raw_bout[,"ENMO"])
  
  timestamp = strftime(format(as.POSIXct(raw_bout[1,1],  origin="1970-01-01"), tz = "UTC"), format = "%H:%M:%S")
  
  duration = nrow(raw_bout)/freq
  
  percentiles =  as.vector((quantile(raw_bout[,'ENMO'], probs = seq(0,1,0.05))))
  
  sd = sd(raw_bout[,"ENMO"])
  
  wind_sd = wind_SD(raw_bout[,"ENMO"])
  
  list_to_return = list(
    
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



wind_SD<-function(vect){
  
  top = as.numeric(quantile(vect, probs = c(0.95)))
  
  bottom = as.numeric(quantile(vect, probs = c(0.05)))
  
  wind_vect = pmin(pmax(vect, bottom), top)
  
  return(sd(wind_vect))
  
}





compute_bouts <- function(D, freq, timestamps) {
  # Assuming D is already loaded with your data
  X <- SedUp(D[,3], freq, 1)
  
  times <- as.numeric(format(as.POSIXct(timestamps, origin = "1970-01-01"), "%H"))
  X[times < 6] <- 0
  
  # Calculate bouts using the adjusted X
  rle_bouts <- rle(X)
  start <- head(cumsum(c(0, rle_bouts$lengths)), -1)
  end <- cumsum(rle_bouts$lengths)
  
  return(data.frame(start = start, end = end, lab = rle_bouts$values))
}



create_ghost_table<-function(row, date){
  
  temp_row = row
  
  temp_row = row
  
  temp_row[seq(27)] = 0
  
  ghost_tab = do.call('rbind',replicate(24,temp_row, simplify = FALSE))
  
  times = c("00:00:00","01:00:00","02:00:00","03:00:00","04:00:00","05:00:00","06:00:00", "07:00:00", "08:00:00",  "09:00:00",  "10:00:00", "11:00:00","12:00:00", "13:00:00" ,"14:00:00" ,"15:00:00", "16:00:00" ,"17:00:00" ,"18:00:00", "19:00:00", "20:00:00", "21:00:00", "22:00:00","23:00:00")
  
  ghost_tab['Time'] = times
  
  ghost_tab['Date'] = date
  
  ghost_tab['WeekDay'] = weekdays(as.Date(date))
  
  ghost_tab['DayNo'] = 0
  
  ghost_tab['NonWear'] = 0
  
  
  
  return(ghost_tab)
  
  
  
}







#install.packages(c("GENEAread", "zoo"))

library(zoo)

library(GENEAread)







compute_one_person<-function(file_name, binfolder, outfolder){
  
  
  
  binfile = file.path(binfolder, file_name)
  
  file_name_without_bin = strsplit(file_name, '.bin')[[1]]
  
  
  
  specific_out_folder = file.path(outfolder, file_name_without_bin)
  

  dir.create(specific_out_folder, recursive = TRUE)
  
  
  
  v_bin_data = read.bin(binfile, virtual = TRUE)
  

  freq  = as.numeric(as.character(v_bin_data$header['Measurement_Frequency',1]))
  print("freq")
  print(freq)
  calib = calibration(binfile)
  
  
  
  time_stamp_list = v_bin_data[["page.timestamps"]]
  
  d1 = time_stamp_list[1]
  
  d2 = time_stamp_list[length(time_stamp_list)]
  
  dates = c(seq(d1, d2, by = "day"), d2)
  
  
  
  days = paste(seq(length(dates)), "00:00:00")
  
  filenames = paste(paste('Day', seq(length(dates)), sep = ''), ".csv", sep = "")
  
  
  
  for (i in (1:(length(filenames)-1)))
    
  {
    
    
    
    print(i)
    
    start = days[i]
    
    end = days[i+1]
    
    
    
    D1 = get_interval(v_bin_data, binfile,start = start,end = end, calib = calib, freq = freq)
    
    
    
    if (nrow(D1$DataFrame)>3000){
      
      
      
      bouts <- compute_bouts(D1$DataFrame, freq, timestamps = D1$DataFrame[,1])
      
      ENMO_tab = add_ENMO(D1$DataFrame)
      
      raw_bouts = get_raw_data(ENMO_tab, bouts, freq)
      
      if (nrow(bouts)>1){
        
        
        
        list_of_bouts = lapply(raw_bouts,extract_metrics, freq = freq)
        
        table_of_bouts = t(do.call("cbind",list_of_bouts))
        
        rownames(table_of_bouts) <- NULL
        
        table_of_bouts = cbind(table_of_bouts, "Date" = as.character(D1$Date))
        
        table_of_bouts = cbind(table_of_bouts, "WeekDay"= as.character(D1$WeekDay))
        
        table_of_bouts = cbind(table_of_bouts, "DayNo" = D1$DayNo)
        
        table_of_bouts = cbind(table_of_bouts, "NonWear" = D1$NonWear)
        
        table_of_bouts = cbind(table_of_bouts, "FILE" = file_name)
        
        table_of_bouts = cbind(table_of_bouts, "Active" = bouts[,'lab'])
        
      }
      
      if (nrow(bouts)==1){
        
        table_of_bouts = extract_metrics(ENMO_tab, freq)
        
        table_of_bouts["Date"] = as.character(D1$Date)
        
        table_of_bouts["WeekDay"] = as.character(D1$WeekDay)
        
        table_of_bouts["DayNo"] = D1$DayNo
        
        table_of_bouts["NonWear"] = D1$NonWear
        
        table_of_bouts["FILE"] = file_name
        
        table_of_bouts["Active"] = bouts[,'lab']
        
        table_of_bouts = data.frame(table_of_bouts)
        
      }
      
      
      
      filepath = file.path(specific_out_folder, filenames[i])
      
      write.csv(table_of_bouts, filepath, row.names = FALSE)
      
    } 
    
  }
  
  
  
  temp = list.files(specific_out_folder,pattern="*.csv")
  
  myfiles = lapply(file.path(specific_out_folder, temp), read.csv)
  
  complete_file = do.call('rbind',myfiles)
  
  
  
  complete_file[,'DayNo'] = complete_file[,'DayNo'] - (min(complete_file[,'DayNo'])-1)
  
  
  
  days_in_study = seq(as.Date(min(complete_file[,'Date'])),as.Date(max(complete_file[,'Date'])), by = 'day')
  
  write.csv(complete_file,file.path(specific_out_folder, "all_days.csv"), row.names = FALSE)
  
}







compute_for_all<-function(binfolder, outfolder){

  
  all_binfiles = list.files(binfolder, pattern = "*.bin")

  for (i in all_binfiles){
    
    compute_one_person(i, binfolder, outfolder)
    
  }
  

  temp = list.files(outfolder,pattern="*all_days.csv", recursive = TRUE)

  myfiles = lapply(file.path(outfolder, temp), read.csv)

  complete_file = do.call('rbind',myfiles)

  write.csv(complete_file,file.path(outfolder, "all_people.csv"), row.names = FALSE)

}

compute_for_all("D:/Downloads/p004", "D:/Downloads/out/test")




# Function to run and time the computation
run_computation <- function() {
  system.time({
    # Start timing here *
    compute_for_all("D:/Downloads/p004", "D:/Downloads/out/004")
    # End timing here **
  })
}

# Running the computation three times and storing the times
#execution_time1 <- run_computation()
#execution_time2 <- run_computation()
#execution_time3 <- run_computation()

# Printing the execution times
print(execution_time1)
print(execution_time2)
print(execution_time3)

