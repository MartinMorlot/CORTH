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


df <- read.csv("/home/mmorlot/dev-work/CORTH/data_regionalization/data_df_merge.csv")

head(df)

df_omit <- na.omit(df)
df_omit <- df_omit[-c(1, 2), ]

names_for_row <- paste(df_omit[, 1], df_omit$nom)
rownames(df_omit) <- names_for_row

cnames <- colnames(df_omit)

new_df_with_data <- data.frame(row.names = names_for_row)

new_df_with_data$bv <- df_omit$bv

empty_bv <- which(new_df_with_data$bv == "")

new_df_with_data$bv[empty_bv] <- df_omit$Surface_bv..km..[empty_bv]

empty_bv <- which(new_df_with_data$bv == "")

new_df_with_data$bv[empty_bv] <- df_omit[empty_bv, "Surface_bv..km...1"]

new_df_with_data$bv <- as.numeric(new_df_with_data$bv)

new_df_with_data$elev <- as.numeric(df_omit$ngfechelle) / 100

empty_elev <- which(is.na(new_df_with_data$elev))

new_df_with_data$elev[empty_elev] <- as.numeric(df_omit$Altitude..m.[empty_elev])

empty_elev <- which(is.na(new_df_with_data$elev))

new_df_with_data$elev[empty_elev] <- as.numeric(df_omit[empty_elev, "Altitude..m..1"])

new_df_with_data$qrefetiage <- as.numeric(df_omit$qrefetiage)
new_df_with_data$qix2 <- as.numeric(df_omit$qix2)

df_omit$geo_json <- gsub("'", "\"", df_omit$Geometry)

df_geo_json_not_empty <- which(df_omit$geo_json != "")

df_sel_geo <- df_omit[df_geo_json_not_empty, ]

# Parse the coordinates from JSON
df_transform_geo <- df_sel_geo %>%
    mutate(
        parsed = lapply(geo_json, fromJSON),
        lon = sapply(parsed, function(x) x$coordinates[1]),
        lat = sapply(parsed, function(x) x$coordinates[2])
    )

# Create a terra SpatVector from lon/lat (CRS84 ≈ EPSG:4326)
points_crs84 <- vect(df_transform_geo[, c("lon", "lat")], crs = "EPSG:4326")

# Reproject to Lambert-93 (France)
points_lambert <- project(points_crs84, "EPSG:2154")

# Extract projected coordinates
coords_lambert <- crds(points_lambert)
df_transform_geo$xlambert <- coords_lambert[, 1]
df_transform_geo$ylambert <- coords_lambert[, 2]
df_transform_geo[, c("lon", "lat", "xlambert", "ylambert")]

df_omit[df_geo_json_not_empty, c("xlambert", "ylambert")] <- df_transform_geo[, c("xlambert", "ylambert")]

# coordinates col
coords_col <- c("xlambert", "ylambert")
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

sel_cols <- c(coords_col, all_cols[grepl("[.]_[.]", all_cols)])

data_from_df <- df_omit[, sel_cols]

new_cols <- gsub("\\._\\.", "_", sel_cols)
colnames(data_from_df) <- new_cols

all_data_df <- cbind(new_df_with_data, data_from_df)

all_data_df[] <- lapply(all_data_df, function(x) as.numeric(as.character(x)))

empty_elev <- which(is.na(new_df_with_data$elev))

station_data_official <- data.frame(vect("Shp_files/StationHydro_FXX.gpkg"))

no_elev_code_hydro <- unlist(lapply(stringr::str_split(rownames(all_data_df[empty_elev, ]), " "), "[[", 1))

match_station_info <- match(no_elev_code_hydro, station_data_official$CdStationHydro)

all_data_df[empty_elev, c("xlambert", "ylambert")] <- station_data_official[match_station_info, c("CoordXStationHydro", "CoordYStationHydro")]

write.csv(all_data_df, "Regionalization_work/cleaned_up_data.csv")
