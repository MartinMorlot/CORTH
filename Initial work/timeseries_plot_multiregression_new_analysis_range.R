library(terra)
library(zoo)
library(sf)
library(units)

stations <- vect("retained_station_with_correction_more.gpkg")

stations_ids <- stations$Station

station_files <- stations$File_location

min_year = sort(stations$`Start Year`)[2]
max_year = 2025

tS <-  seq(as.Date(paste0(min_year, "/1/1")), as.Date(Sys.Date()))

tS_d <- format(tS, "%j")


dataFrame_combined_data <- data.frame(date=tS)

dataFrame_anomaly_data <- data.frame(date=tS)

dataFrame_normalized_data <- data.frame(date=tS)

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
  
  neg <- serie_data[which(serie_data < 0)]
  
  div_neg <- quantile(neg, 0.1)
  
  serie_data[interval_data] <- na.approx(serie_data)
  
  non_na_serie <- which(!is.na(serie_data))
  
  mean_serie <- aggregate(serie_data, by=list(tS_d), FUN=mean, na.rm=T)
  
  anomaly <- (serie_data - mean_serie$x[as.numeric(tS_d)]) / mean_serie$x[as.numeric(tS_d)] * 100
  
  normalized_serie <- serie_data
  
  normalized_serie[which(serie_data < 0)] <- - normalized_serie[which(serie_data < 0)] / div_neg
  
  
  # 
  # plot(serie_data[non_na_serie], type='l')
  # 
  # lines(normalized_serie[non_na_serie], type='l')
  
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
  dataFrame_normalized_data[id] <- normalized_serie
  i<- i+1
}

matplot(dataFrame_anomaly_data$date[which(format(dataFrame_anomaly_data$date, "%Y") > 2015)],
        dataFrame_anomaly_data[c(which(format(dataFrame_anomaly_data$date, "%Y") > 2015)),-c(1)],
         type='l', ylab='DeltaH', xlab='Date', ylim=c(-100,400))

library(ggcorrplot)

data_post_2015 <- dataFrame_anomaly_data[which(format(dataFrame_anomaly_data$date, "%Y") > 2015),]


na_counts <- colSums(is.na(data_post_2015))

# Remove the Customer Value column
reduced_data <- subset(data_post_2015, select = which(na_counts < 500))

reduced_data <- subset(reduced_data, select=-date)

# Compute correlation at 2 decimal places
corr_matrix = round(cor(reduced_data, use = "pairwise.complete.obs"), 2)

# Compute and show the  result
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE)


stations_sf <- st_as_sf(stations)

stations_sel <- stations_sf[which(stations_sf$Station %in% names(reduced_data)),]


distance_matrix <- st_distance(stations_sel, stations_sel)

distance_matrix <- as.matrix(distance_matrix)

stations_sel <- vect(stations_sel)
coordinates <- crds(stations_sel)
row.names(coordinates) <- stations_sel$Station

# Create a logical matrix for distances less than 400 km
close_stations <- distance_matrix < set_units(400000, "m")

# Apply the logical matrix to filter correlations
filtered_cor_matrix <- ifelse(close_stations, corr_matrix, NA)

colnames(filtered_cor_matrix) <- colnames(corr_matrix)
rownames(filtered_cor_matrix) <- rownames(corr_matrix)

lines_df <- data.frame(
  object=numeric(),
  x = numeric(),
  y = numeric()
)

extra_info <- data.frame(
  id = numeric(),
  name = character(),
  corr_value = numeric()
)


pdf("Correlations_anomaly_over_06_close_new.pdf")
object_count <- 1
for(col in colnames(filtered_cor_matrix)){
  data <-filtered_cor_matrix[,col]
  rm_slot <- which(names(data) == col)
  good <- names(which(data[-rm_slot] >= 0.6))
  if(length(good) > 0){
    for(stat in good){
      line <- rbind(
        c(
          coordinates[col, 1],
          coordinates[col, 2]
        ),
        c(
          coordinates[stat, 1],
          coordinates[stat, 2] 
        )
      )
      lines_df <- rbind(lines_df,
                        cbind(objet=object_count, line
                        )
      )
      
      extra_info <- rbind(extra_info,
                          data.frame(
                            id = object_count,
                            name = paste(col,stat,"-"),
                            corr_value = data[stat]
                          )
      )
      
      object_count <- object_count + 1
    }
    
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
}
dev.off()


writeVector(stations_sel, "station_good_for_correlation_anomaly.gpkg", overwrite=T)

colnames(lines_df)[1:3] <- c("object",'x', 'y')

a_numeric <- matrix(as.numeric(as.matrix(lines_df[,1:3])), nrow = nrow(lines_df), ncol = ncol(lines_df))

colnames(a_numeric)[1:3] <- c("object",'x', 'y')

good_cor_lines <- terra::vect(a_numeric, "lines", atts=extra_info, crs="+proj=longlat +datum=WGS84")

plot(good_cor_lines)

# Save the lines to a GeoPackage
writeVector(good_cor_lines, "good_correlation_anomaly.gpkg", overwrite=T)
