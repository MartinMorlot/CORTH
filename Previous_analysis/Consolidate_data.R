rm(list=ls())
gc()
library(terra)
library(zoo)
library(sf)
library(units)
library(ggcorrplot)

stations <- vect("Shp_files/Test_kept_stations_with_grass.gpkg")

stations_ids <- stations$Station

station_files <- stations$File_location

combined_data <- data.frame(matrix(nrow=0,ncol=3))
colnames(combined_data) <- c("DateTime", "deltaH", "Station")

stations$avg_per_year <- NA

i= 1
for(file_to_read in station_files){
  print(i)
  data <- read.csv(file_to_read, sep=';', header=F, col.names = c("DateTime", "deltaH"))
  if(nrow(data) == 0){
    i <- i+1
    next
  }
  station <- stations_ids[i]
  completed_data <- data
  completed_data$Station <- station
  i <- i+1
  year_data <- format(as.POSIXct(data$DateTime), "%Y")
  if(max(year_data) <= 2025){
    max_year <- max(year_data) 
  } else {
    max_year <- 2025
  }
  sorted_years <- sort(as.numeric(year_data))
  min_year <- 0
  j <- 1
  while(min_year < 1950){
    min_year <- sorted_years[j]
    j <- j + 1
  }
  all_years <- min_year:max_year
  nb_points <- rep(0, length(all_years))
  names(nb_points) <- all_years
  year_points <- table(year_data)
  year_points <- year_points[names(year_points) %in% all_years]
  nb_points[match(names(year_points),all_years)] <- year_points
  stations$avg_per_year[i] <- mean(nb_points)
  combined_data <- rbind(combined_data, completed_data[year_data %in% all_years,])
  
  break
}
# 
# write.csv(combined_data, "clean_Data/combined_Data.csv", row.names = F, quote = F)
# 
# writeVector(stations, "Shp_files/Stations_with_data_avg.gpkg",overwrite=T)
