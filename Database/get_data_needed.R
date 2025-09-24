library(terra)

working_folder <- "/home/mmorlot/dev-work/CORTH/"

setwd(working_folder)

print(working_folder)

station <- terra::vect("/home/mmorlot/dev-work/CORTH/Shp_files/Test_kept_stations_with_grass.gpkg")

df_station <- data.frame(station)

uh <- terra::vect(
    "/home/mmorlot/dev-work/frenchMap/Re_territoires_des_UH/UH_Pole2_avec_OM_JNA.shp"
)

station_l93 <- project(station, crs(uh))

plot(uh)

View(data.frame(uh))

uh_sel <- uh[
    c(1,9,10),
]

station_kept <- intersect(station_l93, uh_sel)

site_names <- data.frame(station_kept)[, "Site"]

Extracted_loc <- "Database/Extracted_files"

files_extract <- list.files(Extracted_loc)

files_station <- files_extract[grepl("station", files_extract)]

setwd(Extracted_loc)

stations_db <- rbind(lapply(files_station, read.csv))

stations_db_sel <- stations_db[which(stations_db$codehydro %in% site_names), ]

nosta_sel <- stations_db_sel$nosta

load_data_from_db_files <- function(
    file_list
) {
    i <- 0
    data <- data.frame()
    for(file in file_list){
        file_content <- read.csv(file)
        if(nrow(file_content)){
            if( i == 0){
                data <- file_content
            } else {
                data <- rbind(file_content)
            }
            i <- i+1
        }
    }
    return(data)
}

df_entetecourbe <- load_data_from_db_files(df_entetecourbe_files)

df_entetecourbe_sel <- df_entetecourbe[
    which(df_entetecourbe$nosta %in% nosta_sel),
]

df_courbecorrection_files <- files_extract[grepl("correction", files_extract)]

df_courbecorrection <- load_data_from_db_files(df_courbecorrection_files)

df_courbe_files <- files_extract[grepl("courbe", files_extract)]

df_courbe <- load_data_from_db_files(df_courbe_files)










pdf("All_curves_select_station.pdf")
for (i in seq(1,nrow(station_db_sel))){
    print(i)
    station <- station_db_sel[i, ]
    nosta <- station_db_sel$nosta[[i]]
    entetecourbe_sel_nosta <- entetecourbe_sel[
        which(entetecourbe_sel$nosta==nosta),
    ]
    list_of_curve_per_station <- entetecourbe_sel_nosta$noct
    first_curve = TRUE
    for (curve_i in list_of_curve_per_station){
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
}
dev.off()


#TODO

#redo all corrections deltaH based calculated Q and measured H

#redo all corrections based on the same Qobs and Hobs (without correction)



#compare deltaHobs and deltaHcalc

