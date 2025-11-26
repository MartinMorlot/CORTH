source("Regionalization_work/Utility/Functions_for_regionalization.R")
source("Regionalization_work/Utility/distance_calculating_and_plotting.R")

file_to_analyze <- file_loc

where_to_plot <- plot_loc

variable <- variable

percentage_years_station <- 30 / 100
percentage_one_station <- 30 / 100

col_to_rm <- TRUE

sub_variables <- c("start", "end", "minima")

full_cluster_analysis <- function(file_to_analyze, where_to_plot, variable, col_to_rm = FALSE) {
    data_cluster <- read.csv(file_to_analyze)
    row.names(data_cluster) <- data_cluster$X
    df <- data_cluster[, -c(1)]

    if ((variable %in% sub_variables) && (col_to_rm)) {
        df <- df[, -c(ncol(df))]
    }

    non_NA_per_row <- rowSums(!is.na(df))
    rm_row <- which(non_NA_per_row <= 5)

    df <- df[-rm_row, ]

    cor_function <- cor_with_min_obs

    # for the function, above minimum number of observation is set to 5.

    corr <- cor_function(df)

    corr_clean_results <- clean_correlation(corr, df, cor_function)

    corr <- corr_clean_results$corr
    df <- corr_clean_results$df

    years_retained <- unlist(lapply(strsplit(names(df), "_"), "[", 3))

    year_to_analyze <- as.character(years_retained[1]:years_retained[length(years_retained)])

    dir.create(where_to_plot, recursive = T)

    col <- colorRampPalette(c("blue", "yellow", "red"))(100)
    corr_plot_location <- paste0(where_to_plot, "corr.png")
    png(corr_plot_location, height = 3500, width = 3500, res = 300)
    corrplot(corr,
        method = "color", col = col,
        type = "upper", tl.col = "black", tl.srt = 90,
        addCoef.col = "black", number.cex = 0.7
    )
    dev.off()

    cor_table <- which(corr > 0.7, arr.ind = T)
    cor_table_row_rm <- which(cor_table[, "row"] == cor_table[, "col"])
    cor_table <- cor_table[-cor_table_row_rm, ]
    # Remove rows where (row, col) is the reverse of any other (col, row)
    cor_table_df <- data.frame(cor_table)
    cor_table_filtered <- cor_table_df[
        !duplicated(paste(pmin(cor_table_df$row, cor_table_df$col),
            pmax(cor_table_df$row, cor_table_df$col),
            sep = ","
        )),
    ]

    df_corr_data <- data.frame(matrix(NA, nrow = 0, ncol = 12))
    colnames(df_corr_data) <- c(
        "Station_1_fullname",
        "Station_2_fullname",
        "Combined_station_fullname",
        "Station_1_name",
        "Station_2_name",
        "Combined_station_name",
        "Station_1_code",
        "Station_2_code",
        "Combined_station_code",
        "Values_station_1",
        "Values_station_2",
        "Variable_name"
    )
    for (row_index in seq_len(nrow(cor_table_filtered))) {
        station1 <- df[cor_table_filtered$col[row_index], ]
        station2 <- df[cor_table_filtered$row[row_index], ]
        station1_vals <- as.numeric(station1)
        station2_vals <- as.numeric(station2)
        combined_NOT_NAs <- (!is.na(station1_vals)) & (!is.na(station2_vals))

        retained_station1_vals <- station1_vals[combined_NOT_NAs]
        retained_station2_vals <- station2_vals[combined_NOT_NAs]

        station1_name <- row.names(station1)
        station2_name <- row.names(station2)
        combined_name <- paste(station1_name, station2_name, sep = " - ")

        split_name1 <- strsplit(station1_name, "[ ]")[[1]]
        split_name2 <- strsplit(station2_name, "[ ]")[[1]]

        code1 <- split_name1[1]
        code2 <- split_name2[1]

        name1 <- paste(split_name1[2:length(split_name1)], collapse = " ")
        name2 <- paste(split_name2[2:length(split_name2)], collapse = " ")

        retained_variables_names <- colnames(station1)[combined_NOT_NAs]

        df_row_data <- data.frame(
            Station_1_fullname = station1_name,
            Station_2_fullname = station2_name,
            Combined_station_fullname = combined_name,
            Station_1_name = name1,
            Station_2_name = name2,
            Combined_station_name = paste(name1, name2, sep = " / "),
            Station_1_code = code1,
            Station_2_code = code2,
            Combined_station_code = paste(code1, code2, sep = " / "),
            Values_station_1 = retained_station1_vals,
            Values_station_2 = retained_station2_vals,
            Variable_name = retained_variables_names
        )

        df_corr_data <- rbind(df_corr_data, df_row_data)
    }

    p1 <- ggplot(df_corr_data, aes(x = Values_station_1, y = Values_station_2, fill = Combined_station_name)) +
        geom_point(size = 3, shape = 21) + # Adjust size as needed
        scale_fill_brewer(palette = "Set1") +
        labs(
            title = "a)",
            x = paste0(variable, " day at station 1"),
            y = paste0(variable, " day at station 2"),
            color = ""
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom", # Place legend at the bottom
            legend.direction = "horizontal" # Arrange legend items horizontally
        )

    p2 <- ggplot(df_corr_data, aes(x = Values_station_1, y = Values_station_2, fill = Combined_station_name)) +
        geom_point(size = 3, shape = 21) + # Adjust size as needed
        geom_smooth(method = "lm") +
        scale_fill_brewer(palette = "Set1") +
        labs(
            title = "b)",
            x = paste0(variable, " day at station 1"),
            y = paste0(variable, " day at station 2"),
            color = ""
        ) +
        facet_wrap(. ~ Combined_station_code) +
        theme_minimal() +
        theme(
            legend.position = "none", # Place legend at the bottom
            legend.direction = "horizontal" # Arrange legend items horizontally
        )

    scatter_p <- p1 + p2
    scatter_p <- scatter_p + theme(legend.justification = "center")


    scatter_plot_correlation_loc <- paste0(where_to_plot, "high_corr_scatter.png")
    png(scatter_plot_correlation_loc, height = 2500, width = 4000, res = 300)
    print(scatter_p)
    dev.off()

    # # TODO hclust
    # hclust
    # TODO remake function for dis_calculation for correlation only
    distance <- as.dist(dist_correlation(corr))

    # TODO remake function with the color mid at 0.75
    p_dist <- fviz_dist(distance, FALSE, gradient = list(low = "#00AFBB", mid = "#FFD000D3", high = "#FC4E07"))

    dist_plot_location <- paste0(where_to_plot, "distance.png")
    png(dist_plot_location, height = 2500, width = 4000, res = 300)
    print(p_dist)
    dev.off()

    file_clustering_method <- paste0(where_to_plot, "clustering_method.csv")

    clustering_methods_considered <- c("ward", "complete")

    best_clustering_method <- distance_calc_clustering_method(distance, file_clustering_method, clustering_methods_considered)

    sorted_clustering <- sort(best_clustering_method, decreasing = TRUE)

    print(sorted_clustering[1])

    best_sel <- names(sorted_clustering)[1]

    name <- best_sel

    # for (name in best_sel) {
    dir.create(paste0(where_to_plot, name), recursive = T)

    plots <- make_silhouette_and_wss_plots(distance, hcut, (nrow(df) - 1), hc_func = "agnes", hc_method = name)

    wss_plot_location <- paste0(where_to_plot, name, "/number_cluster_fitting_wss.png")
    print(wss_plot_location)
    png(wss_plot_location, height = 1500, width = 2000, res = 300)
    print(plots[[1]])
    dev.off()

    silhouette_plot_location <- paste0(where_to_plot, name, "/number_cluster_fitting_silhouette.png")
    print(silhouette_plot_location)
    png(silhouette_plot_location, height = 1500, width = 2000, res = 300)
    print(plots[[2]])
    dev.off()

    # if(euclidean){
    #     gap_stat_results <- gap_statistic(dist)
    #     plot_gap <- make_gap_plots(gap_stat_results)
    #     gap_plot_location <- paste0(where_to_plot, name, "/number_cluster_fitting_gap.png")
    #     print(gap_plot_location)
    #     png(gap_plot_location, height = 1500, width = 2000, res = 300)
    #     print(plot_gap)
    #     dev.off()
    # }

    for (number_of_clusters in 2:(nrow(df) - 1)) {
        hc <- as.hclust(agnes(distance, method = name))
        sub_grp <- cutree(hc, k = number_of_clusters)

        corrplot(corr[hc$order, hc$order],
            method = "color", col = col,
            type = "upper", tl.col = "black", tl.srt = 90,
            addCoef.col = "black", number.cex = 0.7
        )

        main_plot <- paste(variable, name, number_of_clusters)

        nb_plot_loc <- paste0(where_to_plot, name, "/", number_of_clusters)

        dir.create(nb_plot_loc, recursive = T)

        cluster_number_plot_loc <- paste0(where_to_plot, name, "/", number_of_clusters, "/hclust.png")
        png(cluster_number_plot_loc, height = 2500, width = 2000, res = 300)
        plot(hc, cex = 0.6, main = name)
        rect.hclust(hc, k = number_of_clusters, border = 2:3)
        dev.off()

        df_with_cluster <- df %>% dplyr::mutate(cluster = sub_grp)

        cnames <- colnames(df_with_cluster)
        cluster_col <- cnames[length(cnames)]
        cluster_numbers <- unique(df_with_cluster[, cluster_col])

        cluster_col_data <- df_with_cluster[, cluster_col]

        for (cluster_number in seq_along(cluster_numbers)) {
            row_sel <- which(cluster_col_data == cluster_number)
            select_cluster <- df[row_sel, ]
            if (!variable %in% sub_variables) {
                for (sub_variable in sub_variables) {
                    cnames_sel <- colnames(select_cluster)
                    cnames_to_keep <- grepv(sub_variable, cnames_sel)
                    if (length(cnames_to_keep) > 0) {
                        sel_variable_cluster <- select_cluster[, cnames_to_keep]
                        plot_cluster(
                            sel_variable_cluster, sub_variable, cluster_number, year_to_analyze,
                            nb_plot_loc
                        )
                    }
                }
            } else {
                plot_cluster(
                    select_cluster, variable, cluster_number, year_to_analyze,
                    nb_plot_loc
                )
            }
        }
    }
    # }
    return("Everything ran correctly")
}


sub_variables <- c("start", "end", "minima")

cluster_analysis_nbclust <- function(file_to_analyze, where_to_plot, variable, percentage_years_station, percentage_one_station, col_to_rm = FALSE, dist_type) {
    data_cluster <- read.csv(file_to_analyze)
    row.names(data_cluster) <- data_cluster$X
    df <- data_cluster[, -c(1)]

    if ((variable %in% sub_variables) && (col_to_rm)) {
        df <- df[, -c(ncol(df))]
    }

    retain_crit <- colSums(!is.na(df))
    retain_crit_val <- which(retain_crit >= (percentage_years_station * nrow(df)))
    years_retained <- unlist(lapply(strsplit(names(retain_crit_val), "_"), "[", 3))

    year_to_analyze <- as.character(years_retained[1]:years_retained[length(years_retained)])

    pattern <- paste0(year_to_analyze, collapse = "|") # Creates "2019|2020|2021"
    sel_col <- grepl(pattern, colnames(df))
    df <- df[, sel_col]


    NonNAs_per_row <- rowSums(!is.na(df))
    row_to_rm <- which(NonNAs_per_row < (percentage_one_station * length(year_to_analyze)))
    df <- df[-row_to_rm, ]

    dir.create(where_to_plot, recursive = T)

    distance_list_result <- dist_calculation(df, dist_type, TRUE)
    dist <- distance_list_result[[1]]
    df <- distance_list_result[[2]]

    p_dist <- fviz_dist(dist, FALSE, gradient = list(low = "#00AFBB", high = "#FC4E07"))

    dist_plot_location <- paste0(where_to_plot, dist_type, "_distance.png")
    png(dist_plot_location, height = 4500, width = 4000, res = 300)
    print(p_dist)
    dev.off()

    file_clustering_method <- paste0(where_to_plot, dist_type, "_clustering_method.csv")

    clustering_methods_considered <- c("ward", "complete", "average")

    best_clustering_method <- distance_calc_clustering_method(dist, file_clustering_method, clustering_methods_considered)

    sorted_clustering <- sort(best_clustering_method, decreasing = TRUE)

    print(sorted_clustering[1:2])

    best_sel <- names(sorted_clustering)[1:2]

    for (name in best_sel) {
        dir.create(paste0(where_to_plot, name), recursive = T)

        best_clust_nb <- NbClust(diss = dist, min.nc = 1, max.nc = 12, method = name, index = along)

        p_clust <- fviz_nbclust(best_clust_nb)
    }
    return("Everything ran correctly")
}
