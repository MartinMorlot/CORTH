rm(list = ls())
gc()

library(terra)
library(viridis)
library(dplyr)
library(data.table)
working_folder <- "/home/mmorlot/dev-work/CORTH/"

setwd(working_folder)

source("utility_function.r")

print(working_folder)

station_all <- vect("/home/mmorlot/dev-work/CORTH/Shp_files/StationHydro_FXX.gpkg")


df_station <- data.frame(station_all)

uh <- terra::vect(
    "/home/mmorlot/dev-work/frenchMap/Re_territoires_des_UH/UH_Pole2_avec_OM_JNA.shp"
)

station_l93 <- terra::project(station_all, crs(uh))

uh_sel <- uh[
    c(1, 8, 9, 10),
]

plot(uh_sel)

station_kept <- station_l93[uh_sel]

station_kept_df <- data.frame(station_kept)


View(station_kept_df)

stations_code <- station_kept_df[, "CdStationHydro"]

Extracted_loc <- "Database/Extracted_files"

extracted_files <- list.files(Extracted_loc)

files_station <- extracted_files[grepl("station.csv", extracted_files)]

setwd(Extracted_loc)

stations_db <- load_stations(files_station)

extra_site_code <- c(
    "A3500100",
    "A9091060",
    "A9260001",
    "A2280030",
    "A9071050"
)

all_retained <- unique(c(
    which(stations_db$codehydro3 %in% c(stations_code)),
    which(stations_db$codesitehydro3 %in% c(extra_site_code))
))

stations_db_sel <- stations_db[
    all_retained,
]

nosta_sel <- stations_db_sel$nosta
df_entetecourbe_files <- extracted_files[grepl("entetecourbe", extracted_files)]

df_entetecourbe <- load_data_from_db_files(df_entetecourbe_files)

df_courbecorrection_files <- extracted_files[grepl("correction", extracted_files)]

df_courbecorrection <- load_data_from_db_files(df_courbecorrection_files)

df_courbe_files <- extracted_files[grepl("_pointcourbe", extracted_files)]

df_courbe <- load_data_from_db_files(df_courbe_files)

date_fmt <- "%m/%d/%y %H:%M:%S"

setwd(working_folder)

considered_years <- c(1975:year(today()))

variable_names <- c(
    "start_day",
    "end_day",
    "duration",
    "minima_day",
    "minima_value",
    "minima_norm_all",
    "integral",
    "integral_norm_year",
    "integral_norm_all"
)

all_colnames <- as.vector(outer(variable_names, considered_years, paste, sep = "_"))

# variable to consider:
all_data_df <- as.data.frame(
    matrix(NA,
        nrow = length(stations_db_sel$codehydro3), ncol = length(all_colnames),
        dimnames = list(stations_db_sel$codehydro3, all_colnames)
    )
)



for (i in seq(1, nrow(stations_db_sel))) {
    print(i)
    station <- stations_db_sel[i, ]
    nosta <- stations_db_sel$nosta[i]
    region <- stations_db_sel$region[i]
    entetecourbe_nosta_region <- sel_data_from_station(
        df_entetecourbe,
        nosta,
        region
    )

    codehydro <- station$codehydro3
    name <- station$nom
    river <- station$courdo

    entetecourbe_nosta_region <- date_load_and_correction(entetecourbe_nosta_region, date_fmt, "datedebut", "cdatedeb", TRUE)

    entetecourbe_nosta_region <- date_load_and_correction(entetecourbe_nosta_region, date_fmt, "datefin", "cdatefin", FALSE)

    curve_station_region <- which((df_courbe$noct %in% entetecourbe_nosta_region$noct) & (df_courbe$region == region))

    df_courbe_sta_region <- df_courbe[curve_station_region, ]

    title <- paste(codehydro, name, river, "[", region, "]")

    # first_curve <- TRUE
    # any_curve <- FALSE
    # ylim <- c(min(df_courbe_sta_region$q), max(df_courbe_sta_region$q))
    # xlim <- c(min(df_courbe_sta_region$h) / 1000, max(df_courbe_sta_region$h) / 1000)

    # png(paste0("Plots_curves/", codehydro, ".png"))
    # n_curves <- nrow(entetecourbe_nosta_region)
    # colors <- viridis(n_curves)
    # for (j in seq_len(n_curves)) {
    #     curve_j <- entetecourbe_nosta_region[j, ]
    #     curve_sel <- which(df_courbe_sta_region$noct == curve_j$noct)
    #     start <- curve_j$datedebut
    #     end <- curve_j$datefin
    #     point_curve <- df_courbe_sta_region[curve_sel, ]
    #     if (nrow(point_curve) > 0) {
    #         if (first_curve) {
    #             sorted_Q_order <- match(sort(point_curve$q), point_curve$q)
    #             point_curve <- point_curve[sorted_Q_order, ]
    #             plot(point_curve$h / 1000, point_curve$q,
    #                 type = "l", main = title, ylab = "Discharge [m³/s]", xlab = "Height [m]",
    #                 ylim = ylim,
    #                 xlim = xlim,
    #                 col = colors[j]
    #             )
    #             first_curve <- FALSE
    #             any_curve <- TRUE
    #             legend_labels <- c(
    #                 paste(
    #                     format(start, "%Y"),
    #                     "-",
    #                     format(end, "%Y")
    #                 )
    #             )
    #             retained_colors <- c(colors[j])
    #         } else {
    #             lines(
    #                 point_curve$h / 1000,
    #                 point_curve$q,
    #                 col = colors[j]
    #             )
    #             years_label <- paste(
    #                 format(start, "%Y"),
    #                 "-",
    #                 format(end, "%Y")
    #             )
    #             if (!years_label %in% legend_labels) {
    #                 legend_labels <- c(
    #                     legend_labels,
    #                     years_label
    #                 )
    #                 retained_colors <- c(
    #                     retained_colors,
    #                     colors[j]
    #                 )
    #             }
    #         }
    #         legend("bottomright", legend = legend_labels, col = retained_colors, lwd = 2, cex = 0.8, title = "Period")
    #     }
    # }
    # dev.off()

    # print("rating_curve_done!")

    correction_nosta_region <- sel_data_from_station(
        df_courbecorrection,
        nosta,
        region
    )

    # intervals_date

    correction_nosta_region <- date_load_and_correction(correction_nosta_region, date_fmt, "dateOK", "ladate", TRUE)

    if (nrow(correction_nosta_region) == 0) next
    png(paste0("Plots_corrections/", codehydro, ".png"))
    values <- correction_nosta_region$valeur
    dateOK <- correction_nosta_region$dateOK
    plot(dateOK, values, type = "l", ylab = "Correction (mm)", xlab = "Date", main = title, ylim = c(max(values), min(values)))
    dev.off()

    write.csv(data.frame(dateOK, values), paste0("Data_correction_databases/", codehydro, ".csv"))

    # date_in_range <- which(as.numeric(format(dateOK, "%Y")) %in% considered_years)
    # if (length(date_in_range) > 0) {
    #     correction_sel_nosta_region <- correction_nosta_region[date_in_range, ]

    #     minima_for_norm <- quantile(correction_sel_nosta_region$valeur, 2.5 / 100)[[1]]

    #     for (year in considered_years) {
    #         sel_year <- which(as.numeric(format(correction_sel_nosta_region$dateOK, "%Y")) == year)
    #         correction_year <- correction_sel_nosta_region[sel_year, ]

    #         results_year <- get_data_for_specific_year(correction_year, minima_for_norm)
    #         if (is.null(results_year)) next
    #         col_to_write <- paste0(variable_names, "_", year)
    #         names(results_year) <- col_to_write
    #         all_data_df[codehydro, col_to_write] <- unlist(results_year)
    #     }
    # }


    print("Corretion plotted")
}

# setwd(working_folder)

# all_data_df <- all_data_df[rowSums(!is.na(all_data_df)) > 0, ]

# write.csv(all_data_df, paste0("data_regionalization/data_df_", 2,".csv"))

# write.csv(stations_db_sel, paste0("data_regionalization/stations_db_sel_", 2,".csv"))

# write.csv(station_kept_df, paste0("data_regionalization/station_kept_df_", 2,".csv"))

# station_kept_df_sel <- match(row.names(all_data_df), station_kept_df$CdStationHydro)

# reordered_station <- station_kept_df[station_kept_df_sel, ]

# stations_db_sel_order <- match(row.names(all_data_df), stations_db_sel$codehydro3)
# stations_db_sel_ordered <- stations_db_sel[stations_db_sel_order, ]

# merge_data <- cbind(all_data_df, reordered_station, stations_db_sel_ordered)

# write.csv(merge_data, paste0("data_regionalization/all_merged_data_", 2,".csv"))
