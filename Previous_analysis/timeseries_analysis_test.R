rm(list = ls())
gc()

library(terra)
library(zoo)
library(sf)
library(units)
library(ggcorrplot)

stations <- vect("Shp_files/Test_kept_stations_with_grass.gpkg")

stations_ids <- stations$Station

station_files <- stations$File_location

min_year <- min(stations$`Start Year`, na.rm = T)
min_year <- 2020
max_year <- 2025

tS <- seq(as.Date(paste0(min_year, "/1/1")), as.Date(Sys.Date()))

tS_d <- format(tS, "%j")


dataFrame_combined_data <- data.frame(date = tS)

dataFrame_anomaly_data <- data.frame(date = tS)

dataFrame_anomaly_data_mean <- data.frame(date = tS)

dataFrame_normalized_data <- data.frame(date = tS)

file_to_read <- station_files[40]


serie_na_replace <- function(serie) {
  Inf_serie <- is.infinite(serie$x)

  if (length(Inf_serie) > 0) serie$x[Inf_serie] <- 0

  Nan_Serie <- is.nan(serie$x)

  if (length(Nan_Serie) > 0) serie$x[Nan_Serie] <- 0


  NA_serie <- is.na(serie$x)

  if (length(NA_serie) > 0) serie$x[NA_serie] <- 0

  return(serie)
}


i <- 1
for (file_to_read in station_files) {
  print(i)
  data <- read.csv(file_to_read, sep = ";", header = F, col.names = c("DateTime", "deltaH"))
  if (nrow(data) == 0) {
    i <- i + 1
    next
  }
  station_info <- data.frame(stations[i, ])
  png_file <- paste0("/home/mmorlot/dev-work/CORTH/Previous_analysis/Per_station_pngs/", station_info$Station, ".png")
  png(png_file)
  data$DateTime <- as.POSIXct(data$DateTime, tz = "UTC")
  data$Date <- as.Date(data$DateTime)
  serie_data <- rep(NA, length(tS))
  data_date <- aggregate(deltaH ~ Date, data, FUN = mean)
  data_match <- match(data_date$Date, tS)
  data_NOT_in_match <- which(is.na(data_match))
  if (length(data_match[-c(data_NOT_in_match)]) == 0) {
    i <- i + 1
    next
  }
  if (length(data_NOT_in_match) > 0) {
    serie_data[data_match[-c(data_NOT_in_match)]] <- data_date$deltaH[-c(data_NOT_in_match)]
  } else {
    serie_data[data_match] <- data_date$deltaH
  }



  nonNA <- which(!is.na(serie_data))
  interval_data <- min(nonNA):max(nonNA)

  neg <- serie_data[which(serie_data < 0)]

  min_neg <- quantile(neg, 0.025)
  max_neg <- quantile(neg, 0.975)

  serie_data[interval_data] <- na.approx(serie_data[interval_data], maxgap = 366)

  plot(serie_data, type = "l")

  non_na_serie <- which(!is.na(serie_data))

  mean_serie <- serie_na_replace((aggregate(serie_data, by = list(tS_d), FUN = mean, na.rm = T)))

  std_dev_serie <- serie_na_replace(aggregate(serie_data, by = list(tS_d), FUN = sd, na.rm = T))

  max_serie <- serie_na_replace(aggregate(serie_data, by = list(tS_d), FUN = max, na.rm = T))

  min_serie <- serie_na_replace(aggregate(serie_data, by = list(tS_d), FUN = min, na.rm = T))

  median_serie <- serie_na_replace(aggregate(serie_data, by = list(tS_d), FUN = median, na.rm = T))

  q1090_serie <- serie_na_replace(aggregate(serie_data, by = list(tS_d), FUN = quantile, c(0.1, 0.9), na.rm = T))

  range <- c(max(max_serie$x), min(min_serie$x))

  main_title_graph <- paste(station_info$Station, station_info$Name_station)

  plot(mean_serie, type = "l", main = main_title_graph, ylim = range, col = "red", ylab = "DeltaH (m)", xlab = "Day of the year")

  lines(min_serie)
  lines(max_serie)
  lines(median_serie, col = "blue")

  polygon(c(q1090_serie$Group.1, rev(q1090_serie$Group.1)),
    c(q1090_serie$x[, 1], rev(q1090_serie$x[, 2])),
    col = rgb(0, 0, 1, alpha = 0.3), border = NA
  )


  anomaly <- (serie_data - mean_serie$x[as.numeric(tS_d)]) / std_dev_serie$x[as.numeric(tS_d)] * 100

  anomaly_mean <- (serie_data - median_serie$x[as.numeric(tS_d)]) / median_serie$x[as.numeric(tS_d)] * 100

  normalized_serie <- serie_data

  # normalized_serie[which(serie_data < 0)] <- - (normalized_serie[which(serie_data < 0)] - max_neg) / (min_neg - max_neg)

  normalized_serie <- -(normalized_serie) / (min_neg)


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
  i <- i + 1
  dev.off()
}


column_name_grass_to_rem <- c(
  "K659302001",
  "A443064001",
  "A670121001",
  "A643112002",
  "A832201001",
  "B557201001"
)
column_for_rem <- which(colnames(dataFrame_anomaly_data) %in% column_name_grass_to_rem)

dataFrame_anomaly_data <- dataFrame_anomaly_data[, -column_for_rem]

column_for_rem_normal <- which(colnames(dataFrame_normalized_data) %in% column_name_grass_to_rem)

dataFrame_normalized_data <- dataFrame_normalized_data[, -column_for_rem_normal]

matplot(dataFrame_anomaly_data$date[which(format(dataFrame_anomaly_data$date, "%Y") > 2020)],
  dataFrame_anomaly_data[c(which(format(dataFrame_anomaly_data$date, "%Y") > 2020)), -c(1)],
  type = "l", ylab = "DeltaH", xlab = "Date", ylim = c(-300, 200)
)

matplot(dataFrame_normalized_data$date[which(format(dataFrame_normalized_data$date, "%Y") > 2020)],
  dataFrame_normalized_data[c(which(format(dataFrame_normalized_data$date, "%Y") > 2020)), -c(1)],
  type = "l", ylab = "DeltaH", xlab = "Date", ylim = c(0, -1)
)

data_anomaly_post <- dataFrame_anomaly_data[which(format(dataFrame_anomaly_data$date, "%Y") >= 2021), ]

data_normalized_post <- dataFrame_normalized_data[which(format(dataFrame_normalized_data$date, "%Y") >= 2021), ]



na_counts <- colSums(is.na(data_normalized_post))

# Remove the Customer Value column
reduced_data_normal <- subset(data_normalized_post, select = which(na_counts < 1500))
reduced_data_anomaly <- subset(data_anomaly_post, select = which(na_counts < 1500))

ncol(reduced_data_normal)
ncol(reduced_data_anomaly)


matplot(reduced_data_anomaly$date,
  reduced_data_anomaly[, -c(1)],
  type = "l", ylab = "DeltaH", xlab = "Date", ylim = c(-300, 200)
)

matplot(reduced_data_normal$date,
  reduced_data_normal[, -c(1)],
  type = "l", ylab = "DeltaH", xlab = "Date", ylim = c(0, -1)
)

reduced_data_anomaly <- subset(reduced_data_anomaly, select = -date)
reduced_data_normal <- subset(reduced_data_normal, select = -date)






# Compute correlation at 2 decimal places
corr_matrix_anomaly <- round(cor(reduced_data_anomaly, use = "pairwise.complete.obs"), 2)
corr_matrix_normal <- round(cor(reduced_data_normal, use = "pairwise.complete.obs"), 2)

# Compute and show the  result
ggcorrplot(corr_matrix_normal,
  type = "lower",
  lab = TRUE
)
# Compute and show the  result
ggcorrplot(corr_matrix_anomaly,
  type = "lower",
  lab = TRUE
)


stations_sf <- st_as_sf(stations)

stations_sel <- stations_sf[which(stations_sf$Station %in% names(reduced_data_anomaly)), ]


distance_matrix <- st_distance(stations_sel, stations_sel)

distance_matrix <- as.matrix(distance_matrix)

stations_sel <- vect(stations_sel)
coordinates <- crds(stations_sel)
row.names(coordinates) <- stations_sel$Station

# Create a logical matrix for distances less than 400 km
close_stations <- distance_matrix < set_units(400000, "m")

# Apply the logical matrix to filter correlations
filtered_cor_matrix_anomaly <- ifelse(close_stations, corr_matrix_anomaly, NA)

colnames(filtered_cor_matrix_anomaly) <- colnames(corr_matrix_anomaly)
rownames(filtered_cor_matrix_anomaly) <- rownames(corr_matrix_anomaly)

thresh <- 0.7

lines_df <- data.frame(
  object = numeric(),
  x = numeric(),
  y = numeric()
)

extra_info <- data.frame(
  id = numeric(),
  name = character(),
  corr_value = numeric()
)


pdf(paste0("Plots_correlations/Comparison_Correlations_anomaly_over_", stringr::str_replace(thresh, "[.]", ""), "_close_test_new.pdf"))
object_count <- 1
for (col in colnames(filtered_cor_matrix_anomaly)) {
  data <- filtered_cor_matrix_anomaly[, col]
  rm_slot <- which(names(data) == col)
  good <- names(which(data[-rm_slot] >= thresh))
  if (length(good) > 0) {
    for (stat in good) {
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
      lines_df <- rbind(
        lines_df,
        cbind(objet = object_count, line)
      )

      extra_info <- rbind(
        extra_info,
        data.frame(
          id = object_count,
          name = paste(col, stat, "-"),
          corr_value = data[stat]
        )
      )

      object_count <- object_count + 1
    }

    matplot(data_anomaly_post$date, data_anomaly_post[, c(col, good)],
      type = "l", ylab = "DeltaH", xlab = "Date", ylim = c(-100, 400),
      main = paste0("Good correlations for ", col)
    )
    legend("topleft",
      legend = c(col, good),
      col = 1:6,
      lty = 1:5
    )
  }
}
dev.off()


writeVector(stations_sel, "Shp_files/Results/station_good_for_correlation_anomaly_test_grass.gpkg", overwrite = T)

colnames(lines_df)[1:3] <- c("object", "x", "y")

a_numeric <- matrix(as.numeric(as.matrix(lines_df[, 1:3])), nrow = nrow(lines_df), ncol = ncol(lines_df))

colnames(a_numeric)[1:3] <- c("object", "x", "y")

good_cor_lines <- terra::vect(a_numeric, "lines", atts = extra_info, crs = "+proj=longlat +datum=WGS84")

plot(good_cor_lines)
plot(stations_sel, add = T)

# Save the lines to a GeoPackage
writeVector(good_cor_lines, paste0("Shp_files/Results/good_correlation_anomaly_test_grass_", stringr::str_replace(thresh, "[.]", ""), ".gpkg"), overwrite = T)


filtered_cor_matrix_normnal <- ifelse(close_stations, corr_matrix_normal, NA)

colnames(filtered_cor_matrix_normnal) <- colnames(corr_matrix_normal)
rownames(filtered_cor_matrix_normnal) <- rownames(corr_matrix_normal)

lines_df <- data.frame(
  object = numeric(),
  x = numeric(),
  y = numeric()
)

extra_info <- data.frame(
  id = numeric(),
  name = character(),
  corr_value = numeric()
)


pdf(paste0("Plots_correlations/Comparison_Correlations_normal_over_", stringr::str_replace(thresh, "[.]", ""), "_close_test_new.pdf"))
object_count <- 1
for (col in colnames(filtered_cor_matrix_normnal)) {
  data <- filtered_cor_matrix_normnal[, col]
  rm_slot <- which(names(data) == col)
  good <- names(which(data[-rm_slot] >= thresh))
  if (length(good) > 0) {
    for (stat in good) {
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
      lines_df <- rbind(
        lines_df,
        cbind(objet = object_count, line)
      )

      extra_info <- rbind(
        extra_info,
        data.frame(
          id = object_count,
          name = paste(col, stat, "-"),
          corr_value = data[stat]
        )
      )

      object_count <- object_count + 1
    }

    matplot(data_normalized_post$date, data_normalized_post[, c(col, good)],
      type = "l", ylab = "DeltaH", xlab = "Date", ylim = c(0, -1),
      main = paste0("Good correlations for ", col)
    )
    legend("topleft",
      legend = c(col, good),
      col = 1:6,
      lty = 1:5
    )
  }
}
dev.off()


writeVector(stations_sel, "Shp_files/Results/station_good_for_correlation_normal_test_grass.gpkg", overwrite = T)

colnames(lines_df)[1:3] <- c("object", "x", "y")

a_numeric <- matrix(as.numeric(as.matrix(lines_df[, 1:3])), nrow = nrow(lines_df), ncol = ncol(lines_df))

colnames(a_numeric)[1:3] <- c("object", "x", "y")

good_cor_lines <- terra::vect(a_numeric, "lines", atts = extra_info, crs = "+proj=longlat +datum=WGS84")

plot(good_cor_lines)
plot(stations_sel, add = T)

# Save the lines to a GeoPackage
writeVector(good_cor_lines, paste0("Shp_files/Results/good_correlation_normal_test_grass_", stringr::str_replace(thresh, "[.]", ""), ".gpkg"), overwrite = T)
