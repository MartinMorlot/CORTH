rm(list = ls())
gc()

library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering visualization
library(colorspace)
library(dendextend) # for comparing two dendrograms
library(jsonlite)
library(dplyr)
library(gridExtra)
library(terra)


df <- read.csv("/home/mmorlot/dev-work/CORTH/data_regionalization/all_merged_data.csv")

station_data_official <- data.frame(vect("Shp_files/StationHydro_FXX.gpkg"))

hydro_portail_path <- list.files("/home/mmorlot/dev-work/CORTH/HydroPortail", pattern = ".txt", full.names = TRUE)

stations_meta <- ASHE::create_meta_HYDRO3(hydro_portail_path)

names_for_row <- paste(df[, 1], df$nom)
rownames(df) <- names_for_row

cnames <- colnames(df)

new_df_with_data <- data.frame(row.names = names_for_row)

new_df_with_data$stationcode <- df[, 1]

station_data_official_order <- match(new_df_with_data$stationcode, station_data_official$CdStationHydro)

station_data_official_ordered <- station_data_official[station_data_official_order, ]

new_df_with_data[, c("xlambert", "ylambert")] <- station_data_official_ordered[, c("CoordXStationHydro", "CoordYStationHydro")]

station_meta_order <- match(new_df_with_data$stationcode, stations_meta$code)
stations_meta_kept <- stations_meta[station_meta_order, ]
cnames_meta <- colnames(stations_meta_kept)
new_df_with_data[, cnames_meta] <- stations_meta_kept
View(new_df_with_data)

new_df_with_data$qrefetiage <- as.numeric(df$qrefetiage)
new_df_with_data$qix2 <- as.numeric(df$qix2)

# Start date for each year
start_col <- which(grepl("start", cnames))
# End date for each year
end_col <- which(grepl("end", cnames))
# Duration
duration_col <- which(grepl("duration", cnames))

# valeur minimal date
minimal_date_col <- which(grepl("minima_day", cnames))

# valeur minimal annuel
minimal_value_col <- which(grepl("minima_value", cnames))

# intégral normalisé par la valeur minimal annuel
integral_value_col <- which(grepl("integral_norm_year", cnames))


all_cols <- cnames[c(
    start_col,
    end_col,
    duration_col,
    minimal_date_col,
    minimal_value_col,
    integral_value_col
)]

data_from_df <- df[, all_cols]

colnames(data_from_df) <- all_cols

data_from_df <- lapply(data_from_df, function(x) as.numeric(as.character(x)))


all_data_df <- cbind(new_df_with_data, data_from_df)

View(all_data_df)

write.csv(all_data_df, "Regionalization_work/cleaned_up_data.csv")
