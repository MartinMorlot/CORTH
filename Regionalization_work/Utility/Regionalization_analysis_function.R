library(corrplot)
library(ggplot2)
library(patchwork)

source("Regionalization_work/Utility/Functions_for_regionalization.R")
source("Regionalization_work/Utility/distance_calculating_and_plotting.R")

sub_variables <- c("start", "end", "minima")

full_cluster_analysis <- function(file_to_analyze, where_to_plot, variable, col_to_rm = FALSE, anomaly = FALSE) {
    data_cluster <- read.csv(file_to_analyze)
    row.names(data_cluster) <- data_cluster$X
    df <- data_cluster[, -c(1)]

    if ((variable %in% sub_variables) && (col_to_rm)) {
        df <- df[, -c(ncol(df))]
    }

    if ((!(variable %in% sub_variables)) | (anomaly)) {
        df <- calculate_anomaly_per_variable(df, sub_variables)
    }

    if (anomaly) {
        path_where_to_plot <- strsplit(where_to_plot, "/")[[1]]
        path_where_to_plot[length(path_where_to_plot)] <- paste(path_where_to_plot[length(path_where_to_plot)], "anomaly", sep = "-")
        where_to_plot <- paste0("/", paste(path_where_to_plot[-1], collapse = "/"), "/")
    }

    non_NA_per_row <- rowSums(!is.na(df))
    rm_row <- which(non_NA_per_row <= 5)

    df <- df[-rm_row, ]

    cor_function <- cor_with_min_obs

    cnames_df <- colnames(df)
    variable_present <- c()
    for (sub_variable in sub_variables) {
        if (length(grep(sub_variable, cnames_df)) > 0) {
            variable_present <- c(variable_present, sub_variable)
        }
    }
    minimal_nb_obs <- length(variable_present) * 5
    init_NA_corr <- length(variable_present) * 10

    corr_matrix <- cor_function(df, minimal_nb_obs)

    corr_clean_results <- clean_correlation(corr_matrix, df, cor_function, min_obs = minimal_nb_obs, init_NA_corr)

    corr_matrix <- corr_clean_results$corr
    df <- corr_clean_results$df

    years_retained <- unlist(lapply(strsplit(names(df), "_"), "[", 3))

    year_to_analyze <- as.character(years_retained[1]:years_retained[length(years_retained)])

    dir.create(where_to_plot, recursive = T)

    col <- colorRampPalette(c("blue", "yellow", "red"))(100)
    corr_plot_location <- paste0(where_to_plot, "corr.png")
    png(corr_plot_location, height = 4000, width = 4000, res = 300)
    corrplot(corr_matrix,
        method = "color", col = col,
        type = "upper", tl.col = "black", tl.srt = 90,
        addCoef.col = "black", number.cex = 0.7
    )
    dev.off()

    if (variable %in% sub_variables) {
        scatter_p <- scatter_plot_high_corr(corr_matrix, 0.7, df, variable, anomaly)
        scatter_plot_correlation_loc <- paste0(where_to_plot, "high_corr_scatter.png")
        png(scatter_plot_correlation_loc, height = 2500, width = 4000, res = 300)
        print(scatter_p)
        dev.off()
    } else {
        for (sub_variable in sub_variables) {
            scatter_p <- scatter_plot_high_corr(corr_matrix, 0.7, df, sub_variable, anomaly)
            scatter_plot_correlation_loc <- paste0(where_to_plot, sub_variable, "_high_corr_scatter.png")
            png(scatter_plot_correlation_loc, height = 2500, width = 4000, res = 300)
            print(scatter_p)
            dev.off()
        }
    }

    # TODO remake function for dis_calculation for correlation only
    distance <- as.dist(dist_correlation(corr_matrix))

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

    best_sel <- names(sorted_clustering)[1:2]

    for (name in best_sel) {
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

        hc <- as.hclust(agnes(distance, method = name))

        corr_ordered_location <- paste0(where_to_plot, name, "/corr_ordered.png")
        png(corr_ordered_location, height = 3500, width = 3500, res = 300)
        corrplot(corr_matrix[hc$order, hc$order],
            method = "color", col = col,
            type = "upper", tl.col = "black", tl.srt = 90,
            addCoef.col = "black", number.cex = 0.7
        )
        dev.off()

        cluster_plot_loc <- paste0(where_to_plot, name, "/hclust.png")
        png(cluster_plot_loc, height = 2500, width = 2000, res = 300)
        plot(hc, cex = 0.6, main = name)
        dev.off()

        for (number_of_clusters in 2:(nrow(df) - 1)) {
            sub_grp <- cutree(hc, k = number_of_clusters)
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
    }
    return("Everything ran correctly")
}
