library(terra)
library(jsonlite)
library(dplyr)

# Example data frame
df <- data.frame(
    geo = c(
        "{'type': 'Point', 'crs': {'type': 'name', 'properties': {'name': 'urn:ogc:def:crs:OGC:1.3:CRS84'}}, 'coordinates': [5.96185355143439, 49.0125450473669]}",
        "{'type': 'Point', 'crs': {'type': 'name', 'properties': {'name': 'urn:ogc:def:crs:OGC:1.3:CRS84'}}, 'coordinates': [4.835659, 45.764043]}"
    ),
    stringsAsFactors = FALSE
)

# Fix single quotes → valid JSON (double quotes)
df$geo_json <- gsub("'", "\"", df$geo)

# Parse the coordinates from JSON
df <- df %>%
    mutate(
        parsed = lapply(geo_json, fromJSON),
        lon = sapply(parsed, function(x) x$coordinates[1]),
        lat = sapply(parsed, function(x) x$coordinates[2])
    )

# Create a terra SpatVector from lon/lat (CRS84 ≈ EPSG:4326)
points_crs84 <- vect(df[, c("lon", "lat")], crs = "EPSG:4326")

# Reproject to Lambert-93 (France)
points_lambert <- project(points_crs84, "EPSG:2154")

# Extract projected coordinates
coords_lambert <- crds(points_lambert)
df$x_lambert <- coords_lambert[, 1]
df$y_lambert <- coords_lambert[, 2]

df[, c("lon", "lat", "x_lambert", "y_lambert")]


terraOptions(tempdir = "/home/mmorlot/terra_tmp")
terraOptions(memfrac = 0.75) # use 75% of RAM

elevation_france <- rast("/home/mmorlot/dev-work/frenchMap/mnt-france-metro-drom/France_metropolitaine.tif")

terra::inMemory(elevation_france)

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
