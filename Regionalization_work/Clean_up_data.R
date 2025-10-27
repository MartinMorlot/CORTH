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
head(df[, 1])

names_for_row <- paste(df[, 1], df$nom)
rownames(df) <- names_for_row

cnames <- colnames(df)

new_df_with_data <- data.frame(row.names = names_for_row)

new_df_with_data$stationcode <- df[, 1]

station_data_official_order <- match(new_df_with_data$stationcode, station_data_official$CdStationHydro)

station_data_official_ordered <- station_data_official[station_data_official_order, ]

new_df_with_data[, c("xlambert", "ylambert")] <- station_data_official_ordered[, c("CoordXStationHydro", "CoordYStationHydro")]

View(new_df_with_data)

# empty_bv <- which(new_df_with_data$bv == "")

# new_df_with_data$bv[empty_bv] <- df_omit$Surface_bv..km..[empty_bv]

# empty_bv <- which(new_df_with_data$bv == "")

# new_df_with_data$bv[empty_bv] <- df_omit[empty_bv, "Surface_bv..km...1"]

# new_df_with_data$bv <- as.numeric(new_df_with_data$bv)

# new_df_with_data$elev <- as.numeric(df_omit$ngfechelle) / 100

# empty_elev <- which(is.na(new_df_with_data$elev))

# new_df_with_data$elev[empty_elev] <- as.numeric(df_omit$Altitude..m.[empty_elev])

# empty_elev <- which(is.na(new_df_with_data$elev))

# new_df_with_data$elev[empty_elev] <- as.numeric(df_omit[empty_elev, "Altitude..m..1"])

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

all_data_df <- cbind(new_df_with_data, data_from_df)

all_data_df[] <- lapply(all_data_df, function(x) as.numeric(as.character(x)))

write.csv(all_data_df, "Regionalization_work/cleaned_up_data.csv")
