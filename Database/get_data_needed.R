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

uh_sel <- uh[
    c(1,9,10),
]

station_kept <- intersect(station_l93, uh_sel)

site_names <- data.frame(station_kept)[, "Site"]

Extracted_loc <- "Database/Extracted_files"

files_extract <- list.files(Extracted_loc)

files_station <- files_extract[grepl("station.csv", files_extract)]

setwd(Extracted_loc)

for(station_file in files_station){
    print(station_file)
    content <- read.csv(station_file)
    region <-unlist(lapply(strsplit(station_file, "_"), "[[", 2))
    content$region <- region 
    names_of_columns = colnames(content)
    if(station_file == files_station[1]){
        default_columns = names_of_columns
        merged_content = content
    }

    not_there=which(!names_of_columns %in% default_columns)
    if(length(not_there) > 0){
        content <- content[,-not_there]
    }

    if(station_file != files_station[1]){
        default_columns = names_of_columns
        merged_content = rbind(merged_content, content)
    }
}

stations_db <- merged_content

extra_site_code <- c(
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

load_data_from_db_files <- function(
    file_list
) {
    i <- 0
    for(file_name in file_list){
        region <-unlist(lapply(strsplit(file_name, "_"), "[[", 2))
        file_content <- read.csv(file_name)
        nb_rows <- nrow(file_content)
        file_content$region <- rep(region, nb_rows)
        if(nb_rows > 0){
            if( i == 0){
                resulting_data <- file_content
            } else {
                resulting_data <- rbind(file_content)
            }
            i <- i+1
        }
    }
    if(! exists("resulting_data")){
        return(resulting_data)
    }
}

df_entetecourbe_files <- files_extract[grepl("entetecourbe", files_extract)]

df_entetecourbe <- load_data_from_db_files(df_entetecourbe_files)

df_entetecourbe_sel <- df_entetecourbe[
    which(df_entetecourbe$nosta %in% nosta_sel),
]

df_courbecorrection_files <- files_extract[grepl("correction", files_extract)]

df_courbecorrection <- load_data_from_db_files(df_courbecorrection_files)

df_courbe_files <- files_extract[grepl("_courbe", files_extract)]

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

