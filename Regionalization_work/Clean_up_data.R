rm(list = ls())
gc()

library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering visualization
library(colorspace)
library(dendextend) # for comparing two dendrograms
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

empty_elev <- which(is.na(new_df_with_data$elev))

elevation_france <- rast("/home/mmorlot/dev-work/frenchMap/mnt-france-metro-drom/France_metropolitaine.tif")
crs(elevation_france)
for (id in empty_elev) {
    info <- df_omit[id, ]
    if (info$Geometry == "") {
        # TODO create geometry from Lambert coordinates (x and y), add to Geometry in CRS84
        pt <- vect(cbind(as.numeric(info$xlambert), as.numeric(info$ylambert)), crs = "EPSG:2154")
        pt_proj <- project(pt, "EPSG:4326")
    }
}

new_df_with_data$qrefetiage <- as.numeric(df_omit$qrefetiage)
new_df_with_data$qix2 <- as.numeric(df_omit$qix2)

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

sel_cols <- all_cols[grepl("[.]_[.]", all_cols)]

data_from_df <- df_omit[, sel_cols]

new_cols <- gsub("\\._\\.", "_", sel_cols)
colnames(data_from_df) <- new_cols

all_data_df <- cbind(new_df_with_data, data_from_df)

write.csv(all_data_df, "Regionalization_work/cleaned_up_data.csv")


terraOptions(tempdir = "/home/mmorlot/terra_tmp")
terraOptions(memfrac = 0.75) # use 75% of RAM

elevation_france <- rast("/home/mmorlot/dev-work/frenchMap/mnt-france-metro-drom/France_metropolitaine.tif")

crs(raster_crs <- crs(elevation_france))
crs(point_crs <- crs(pt_proj))
ext(elevation_france)
crds(pt_proj)

# 2) Make sure point and raster use the same CRS (align the point to raster)
pt_aligned <- project(pt_proj, crs(elevation_france))
crds(pt_aligned) # lon/lat or projected coords as expected

# 3) Which cell does the point fall into?
cell <- cellFromXY(elevation_france, crds(pt_aligned))
cell # NA -> point outside raster extent

# 4) If cell is not NA, check raster value at that cell
if (!is.na(cell)) {
    # value via cell index
    vals <- values(elevation_france)
    vals[cell] # raw cell value (may be NA)
    # OR using single-layer extract (returns data.frame)
    ex <- terra::extract(elevation_france, pt_aligned)
    print(ex)
}

terraOptions(memfrac = 0.75) # use 75% of RAM
terraOptions(tempdir = "/fast/tmp")
