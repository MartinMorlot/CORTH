rm(list=ls())
gc()
library(terra)
library(zoo)
library(sf)
library(units)
library(ggcorrplot)

setwd("/home/mmorlot/dev/CORTH/")

stations <- vect("Shp_files/Test_kept_stations_with_grass.gpkg")

stations_ids <- stations$Station

station_files <- stations$File_location

gauging_files <- list.files("Gauging_data")

name_station_gauging <- unlist(lapply(strsplit(gauging_files, '-'), "[[", 1))

combined_data <- data.frame(matrix(nrow=0,ncol=3))
colnames(combined_data) <- c("DateTime", "deltaH", "Station")

stations$avg_per_year <- NA

folder_loc <- getwd()

i= 1
for(file_to_read in station_files){
  setwd(folder_loc)
  print(i)
  data <- read.csv(file_to_read, sep=';', header=F, col.names = c("DateTime", "deltaH"))
  if(nrow(data) == 0){
    i <- i+1
    next
  }
  station <- stations_ids[i]
  
  station_gauging_match <- which(name_station_gauging == station)
  if(length(station_gauging_match) == 1){
    gauging_data <- read.csv(paste("Gauging_data", gauging_files[station_gauging_match], sep='/'))
    if(nrow(gauging_data) > 0) gauging_data_year <- unique(format(as.POSIXct(gauging_data$Date.de.début..TU.), "%Y"))
  }
  
  
  completed_data <- data
  completed_data$Station <- station
  i <- i+1
  year_data <- format(as.POSIXct(data$DateTime), "%Y")
  # if(max(year_data) <= 2025){
  #   max_year <- max(year_data) 
  # } else {
    max_year <- 2025
  # }
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
  
  station_loc <- paste0(folder_loc, "/stations/", station)
  
  full_serie <- c()
  
  folder_exists <- dir.exists(station_loc)
  
  if(folder_exists){
    setwd(station_loc)
    for(year in all_years){
      to_extract <- paste0(year, "_HQ.csv.gz")
      if(file.exists(to_extract)){
        year_data <- read.csv(gzfile(to_extract), sep=';')
        leap <- omnibus::isLeapYear(year)
        days_found <- length(unique(format(as.POSIXct(year_data$dtmesure), "%j")))
        all_days <- TRUE
        nb_missing <- 0
        if(leap & !(days_found==366)){
          nb_missing <- length(which(!1:366 %in% as.numeric(unique(format(as.POSIXct(year_data$dtmesure), "%j")))))
        }else if((!leap & !(days_found==365))){
          nb_missing <- length(which(!1:365 %in% as.numeric(unique(format(as.POSIXct(year_data$dtmesure), "%j")))))
        }
        if(nb_missing >= 30){
          all_days <- FALSE
        }
      } else {
        all_days <- FALSE
      }
      full_serie <- c(full_serie,all_days)
    }
  }
  
  full_serie <- c(full_serie,all_days)
  
  if(length(full_serie) > 0){
    ind_nb_point_zero <- which(nb_points == 0)
    if(length(ind_nb_point_zero)){
     cond_full_serie <- full_serie[ind_nb_point_zero]
     if(any(! names(ind_nb_point_zero) %in% 2023:2025)) break
     nb_points[ind_nb_point_zero][!cond_full_serie] <- NA
     if(any(cond_full_serie) & exists("gauging_data_year")){
       cond_gauging_absent <- (!names(nb_points[ind_nb_point_zero][cond_full_serie]) %in% gauging_data_year)
       if(any(cond_gauging_absent)){
         nb_points[ind_nb_point_zero][cond_full_serie][cond_gauging_absent] <- NA
         print(nb_points)
       }
     }
    }
  }
  
  mean_nb_points <- mean(nb_points, na.rm=T)
  print(mean_nb_points)
  if(is.nan(mean_nb_points)) break
  stations$avg_per_year[i] <- mean_nb_points
  combined_data <- rbind(combined_data, completed_data[year_data %in% all_years,])
  if(exists('gauging_data')){
    rm(gauging_data)
  }
  if(exists('gauging_data_year')){
    rm(gauging_data_year)
  }
}

setwd(folder_loc)


#writeVector(stations, "Shp_files/Station_with_average_limni_gauging.gpkg")


