library(terra)


stations_ret <- vect("retained_station_with_correction_more.gpkg")

stations <- vect("geodata_station_with_correction_more.gpkg")

UH <- vect("../frenchMap/Re_ territoires des UH/UH_Pole2_avec_OM_JNA.shp")

plot(UH)

UH_points_retained <- extract(UH, project(stations_ret, crs(UH)))
UH_combined_points_retained <- table(UH_points_retained$cdreseau)

UH_points <- extract(UH, project(stations, crs(UH)))
UH_combined_points <- table(UH_points$cdreseau)


UH$points_correction <- NA
UH$points_retained <- NA

names(UH_combined_points)

UH$points_correction[match(names(UH_combined_points), UH$cdreseau)] <- UH_combined_points

UH$points_retained[match(names(UH_combined_points_retained), UH$cdreseau)] <- UH_combined_points_retained

UH$points_retained <- as.integer(UH$points_retained)
writeVector(UH, "UH_with_points.gpkg", overwrite=T)
