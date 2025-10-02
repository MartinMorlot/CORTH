library(terra)
library(dplyr)
working_folder <- "/home/mmorlot/dev-work/CORTH/"

setwd(working_folder)

source("utility_function.r")

print(working_folder)

station <- terra::vect("/home/mmorlot/dev-work/CORTH/Shp_files/Test_kept_stations_with_grass.gpkg")

df_station <- data.frame(station)

uh <- terra::vect(
    "/home/mmorlot/dev-work/frenchMap/Re_territoires_des_UH/UH_Pole2_avec_OM_JNA.shp"
)

station_l93 <- project(station, crs(uh))

uh_sel <- uh[
    c(1,9,10),
]

station_kept <- intersect(station_l93, uh_sel)

station_kept_df <- data.frame(merge_spatvectors(station_kept))

site_names <- station_kept_df[, ""]

Extracted_loc <- "Database/Extracted_files"

extracted_files <- list.files(Extracted_loc)

files_station <- extracted_files[grepl("station.csv", extracted_files)]

setwd(Extracted_loc)

stations_db <- load_stations(files_station)

extra_code <- c(
    "A3500100",
    "A9091060",
    "A9260001",
    "A2280030",
    "A9071050"
)

stations_db_sel <- stations_db[
    which(stations_db$codehydro %in% c(site_names, extra_site_code)),
]

nosta_sel <- stations_db_sel$nosta;

df_entetecourbe_files <- extracted_files[grepl("entetecourbe", extracted_files)]

df_entetecourbe <- load_data_from_db_files(df_entetecourbe_files)

df_courbecorrection_files <- extracted_files[grepl("correction", extracted_files)]

df_courbecorrection <- load_data_from_db_files(df_courbecorrection_files)

df_courbe_files <- extracted_files[grepl("_pointcourbe", extracted_files)]

df_courbe <- load_data_from_db_files(df_courbe_files)

setwd(working_folder)

for(i in seq(1, nrow(stations_db_sel))){
    print(i)
    station <- stations_db_sel[i, ]
    nosta <- stations_db_sel$nosta[i]
    region <- stations_db_sel$region[i]
    entetecourbe_nosta_region <- sel_data_from_station(
        df_entetecourbe,
        nosta,
        region
    )

    View(data.frame(entetecourbe_nosta))
    first_curve = TRUE

    codehydro <- station$codehydro
    name <- station$nom
    river <- station$courdo

    correction_nosta_region <- sel_data_from_station(
        df_courbecorrection,
        nosta,
        region
    )

    pdf(paste0("Plots_curves/", codehydro, ".pdf"))
    for (curve_i in entetecourbe_nosta_region$noct){
        curve_sel <- which(df_courbe$noct %in% curve_i)
        if(length(curve_sel)>0){
            point_curve <- df_courbe[curve_sel, ]
            if(first_curve){
                plot(point_curve$h, point_curve$q, type='l', main=paste(station$codehydro, station$nom), ylab="Discharge [m^3/s]", xlab="Height [m]")
                first_curve=FALSE
            } else {
                lines(point_curve$h, point_curve$q)
            }
        }
    }
    dev.off()

    png(paste0("Plots_corrections/", codehydro, ".png"))
    dev.off()
}