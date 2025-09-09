library(terra)

working_folder <- getwd()

print(working_folder)


df_entreecourbe <- read.csv("Database/Extracted_entetecourbe.csv")

df_entreecourbeautreperiode <- read.csv("Database/Extracted_entetecourbeautreperiode.csv")

station <- terra::vect("/home/mmorlot/dev-work/CORTH/Shp_files/Test_kept_stations_with_grass.gpkg")

df_station <- data.frame(station)

View(df_station)

uh <- terra::vect("/home/mmorlot/dev-work/frenchMap/Re_territoires_des_UH/UH_Pole2_avec_OM_JNA.shp")

station_l93 <- project(station, crs(uh))

uh_davidbesson <- uh[1, ]

station_kept <- intersect(station_l93, uh_davidbesson)

site_names <- data.frame(station_kept)[, "Site"]

station_db <- read.csv("Database/Extracted_station.csv")

colnames(station_db)

station_db_sel <- station_db[which(station_db$codehydro %in% site_names), ]

nosta_sel <- station_db_sel$nosta

entetecourbe_sel <- df_entreecourbe[which(df_entreecourbe$nosta %in% nosta_sel), ]
entetecourbeautreperiode_sel <- df_entreecourbeautreperiode[which(df_entreecourbeautreperiode$nosta %in% nosta_sel),]


df_courbecorrection <- read.csv("Database/Extracted")

df_courbe <- read.csv("Database/Extracted_pointcourbe.csv")

pdf("All_curves_select_station.pdf")
for (i in seq(1,nrow(station_db_sel))){
    print(i)
    station <- station_db_sel[i, ]
    nosta <- station_db_sel$nosta[[i]]
    entetecourbe_sel_nosta <- entetecourbe_sel[which(entetecourbe_sel$nosta==nosta), ]
    list_of_curve_per_station <- entetecourbe_sel_nosta$noct
    first_curve=TRUE
    for (curve_i in list_of_curve_per_station){
        curve_sel <- which(df_courbe$noct %in% curve)
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
    
    # lines below are commented because it is never the case, at least for the re.
    #entetecourbeautreperiode_sel_nosta <- entetecourbeautreperiode_sel[which(entetecourbeautreperiode_sel$nosta==nosta), ]
    #if(nrow(entetecourbeautreperiode_sel_nosta) > 0) break
}
dev.off()


#TODO

#redo all corrections deltaH based calculated Q and measured H

#redo all corrections based on the same Qobs and Hobs (without correction)

#compare deltaHobs and deltaHcalc

