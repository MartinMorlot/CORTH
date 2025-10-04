library(terra)
library(zoo)

stations <- vect("retained_station_with_correction_more.gpkg")

stations_ids <- stations$Station

station_files <- stations$File_location

min_year = sort(stations$`Start Year`)[2]
max_year = 2025

tS <-  seq(as.Date(paste0(min_year, "/1/1")), as.Date(Sys.Date()))

tS_d <- format(tS, "%j")


dataFrame_combined_data <- data.frame(date=tS)

dataFrame_anomaly_data <- data.frame(date=tS)

file_to_read = station_files[100]

i= 1
for(file_to_read in station_files){
  print(i)
  data <- read.csv(file_to_read, sep=';', header=F, col.names = c("DateTime", "deltaH"))
  data$DateTime <- as.POSIXct(data$DateTime, tz="UTC")
  data$Date <- as.Date(data$DateTime)
  serie_data <- rep(NA, length(tS))
  data_date <- aggregate(deltaH~Date, data, FUN=min)
  data_match <- match(data_date$Date, tS)
  data_NOT_in_match <- which(is.na(data_match))
  if(length(data_NOT_in_match)>0){
    serie_data[data_match[-c(data_NOT_in_match)]] <- data_date$deltaH[-c(data_NOT_in_match)] 
  } else {
    serie_data[data_match] <- data_date$deltaH
  }

  nonNA <- which(!is.na(serie_data))
  interval_data <- min(nonNA):max(nonNA)
  serie_data[interval_data] <- na.approx(serie_data)
  
  non_na_serie <- which(!is.na(serie_data))
  
  mean_serie <- aggregate(serie_data, by=list(tS_d), FUN=mean, na.rm=T)
  
  anomaly <- (serie_data - mean_serie$x[as.numeric(tS_d)]) / mean_serie$x[as.numeric(tS_d)] * 100
  # 
  # plot(serie_data[non_na_serie], type='l')
  # lines(mean_serie$x[as.numeric(tS_d)][non_na_serie], col='blue')
  # 
  # plot((serie_data - mean_serie$x[as.numeric(tS_d)])[non_na_serie], type='l', col='red')
  # lines(mean_serie$x[as.numeric(tS_d)][non_na_serie], col='blue')
  # 
  # plot(((serie_data - mean_serie$x[as.numeric(tS_d)])[non_na_serie]) / mean_serie$x[as.numeric(tS_d)][non_na_serie] * 100 , type='l', col='green')
  
  # plot(anomaly[non_na_serie])
  # abline(h=0)
  id <- stations_ids[i]
  dataFrame_combined_data[id] <- serie_data
  dataFrame_anomaly_data[id] <- anomaly
  i<- i+1
}

matplot(dataFrame_anomaly_data$date[which(format(dataFrame_anomaly_data$date, "%Y") > 2015)],
        dataFrame_anomaly_data[c(which(format(dataFrame_anomaly_data$date, "%Y") > 2015)),-c(1)],
         type='l', ylab='DeltaH', xlab='Date', ylim=c(-100,400))

library(ggcorrplot)

data_post_2015 <- dataFrame_anomaly_data[which(format(dataFrame_anomaly_data$date, "%Y") > 2015),]

# Remove the Customer Value column
reduced_data <- subset(data_post_2015, select = -date)

# Compute correlation at 2 decimal places
corr_matrix = round(cor(reduced_data, use = "pairwise.complete.obs"), 2)

# Compute and show the  result
ggcorrplot(corr_matrix[1:30,1:30], type = "lower",
           lab = TRUE)


pdf("Correlations_anomaly_over_08_new.pdf")
for(col in colnames(corr_matrix)){
  data <-corr_matrix[,col]
  rm_slot <- which(names(data) == col)
  good <- names(which(data[-rm_slot] > 0.8))
  
  if(length(good) > 0){
    matplot(data_post_2015$date, data_post_2015[,c(col,good)],
            type='l', ylab='DeltaH', xlab='Date', ylim=c(-100,400),
            main=paste0("Good correlations for ", col)
    )
    legend("topleft",
           legend = c(col,good),
           col = 1:6,
           lty = 1:5
           )
  }
  break
}
dev.off()




