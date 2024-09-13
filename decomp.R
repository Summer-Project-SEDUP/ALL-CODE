binfolder <- "D:/Downloads/in"
outfolder <- "D:/Downloads/out"
file_name <- "P021_027734_2021-02-10 15-37-24.bin"


binfile = file.path(binfolder, file_name)
  
file_name_without_bin = strsplit(file_name, '.bin')[[1]]
  
  
  
specific_out_folder = file.path(outfolder, file_name_without_bin)
  
  
dir.create(specific_out_folder, recursive = TRUE)
  
  
  
v_bin_data = read.bin(binfile, virtual = TRUE)
  
  
freq  = as.numeric(as.character(v_bin_data$header['Measurement_Frequency',1]))
print("freq")
print(freq)


# cali function 

calibration_sphere = GENEActiv.calibrate(binfile, printsummary = FALSE)

calibration_offset = calibration_sphere$offset 

calibration_scale = calibration_sphere$scale

calib = c(calibration_scale, calibration_offset)

#########################################################################################################################################################
  
  
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
    
    
    
  # get interval 
  
  day_data = read.bin(binfile, start = start, end = end)$data.out
  
  day_data = cbind(cbind(day_data[,1],((calib[1:3]*day_data[,c('x', 'y','z')]) + calib[4:6])), day_data[,7])
  
  n_hours = (nrow(day_data)/freq/3600)
  
  # non_wear_window 
  
  
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
    
  #########################################################################################################################################################
  
  non_wear_hours = mean(rollapply(day_data, (10*60*freq), non_wear_window, by = (freq*60), by.column = FALSE)) * n_hours
  
  date = as.Date(as.POSIXct(day_data[nrow(day_data)/2,1],  origin="1970-01-01"))
  
  day_of_week = weekdays(date)
  
  D1 = list("DataFrame" = day_data, "Date" = date, "WeekDay" = day_of_week, "DayNo"=strsplit(start, " ")[[1]][1], "NonWear" = non_wear_hours)
  
  #########################################################################################################################################################
    
    
    
  if (nrow(D1$DataFrame)>3000){
      
      
    # compute_bouts
    #sedup
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
    #########################################################################################################################################################
    X = SedUp(D[,3], freq, 1)
    
    rle_bouts = rle(X) 
    
    start = head(cumsum(c(0,rle_bouts$lengths)), -1)
    
    end=cumsum(rle_bouts$lengths)
    
    
    bouts= data.frame(start=start, end=end, lab = rle_bouts$values)
    
    #########################################################################################################################################################
    
    # add_ENMO 
    
    ENMO = pmax((((rowSums(day_matrix[,c('x','y','z')]**2))**(1/2))-1), 0)
    
    day_matrix = cbind(day_matrix, ENMO)
    
    ENMO_tab = day_matrix
    #########################################################################################################################################################
    
    # get_raw_bouts
    raw_bouts = apply(bouts,1, function(x){D[x[1]:x[2],]})
      
    #########################################################################################################################################################
    
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
  

