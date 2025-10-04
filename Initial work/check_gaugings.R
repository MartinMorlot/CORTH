library(terra)
library(zoo)

stations <- vect("geodata_station_with_correction_more.gpkg")

stations_ids <- stations$Station

station_files <- stations$File_location

min_year = sort(stations$`Start Year`)[2]
max_year = 2025

tS <-  seq(as.Date(paste0(min_year, "/1/1")), as.Date(Sys.Date()))

stations$ratio_gauging <- NA


data_unique_station <- list()

i= 1
for(i in 1:nrow(stations)){
  print(i)
  file_to_read <- stations$File_location[i]
  data <- read.csv(file_to_read, sep=';', header=F, col.names = c("DateTime", "deltaH"))
  data$DateTime <- as.POSIXct(data$DateTime, tz="UTC")
  data$Date <- as.Date(data$DateTime)
  to_rem <- which(data$deltaH == 0)
  if(length(to_rem) > 0) data <- data[,-c(to_rem), drop=F]
  if(nrow(data)==0) next
  data$Gauging <- NA
  gauging_file <- paste0("Gauging_data/", stations$Station[i], "-Jaugeages.csv")
  print(gauging_file)
  if(file.exists(gauging_file)){
    gauging_data <- read.csv(gauging_file, fill=T, header=T)
    gauging_data$Datetime <- as.POSIXct(gauging_data$Date.de.début..TU., format='%Y-%m-%dT%T.000Z', tz="UTC")
    gauging_data$Date <- as.Date(gauging_data$Datetime)
    date_less <- which(data$Date < min(gauging_data$Date))
    if(length(date_less) > 0 )  data <- data[,-c(date_less), drop=F]
    matches <- match(gauging_data$Date,data$Date)
    unique_m <- unique(matches)
    non_na <- !is.na(unique_m)
    if(any(non_na)){
      data$Gauging[unique_m[non_na]] <- "Yes"
    }
    ratio <- length(unique_m[non_na]) / nrow(data)
    stations$ratio_gauging[i] <- ratio
  }
  data_unique_station[[i]] <- data
}

View(data.frame(stations))

writeVector(stations, "Station_gauging_info.gpkg", overwrite=T)

sel_station <- !is.na(stations$ratio_gauging) & (stations$ratio_gauging > 0)

png("stations_gauging_to_correction_ration_no_zero_post_start_with_data_sel.png")
hist(stations$ratio_gauging[sel_station], xlab="Ratio of gaugings to corrections", ylab="# of stations", main="Histogram of station correction to gaugings ratios")
dev.off()
hist(stations$ratio_gauging[sel_station], xlab="Ratio of gaugings to corrections", ylab="# of stations", main="Histogram of station correction to gaugings ratios")

length(which(stations$ratio_gauging[sel_station] > 0.5))
