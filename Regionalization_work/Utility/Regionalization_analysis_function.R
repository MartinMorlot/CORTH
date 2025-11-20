source("Regionalization_work/Utility/Functions_for_regionalization.R")
source("Regionalization_work/Utility/distance_calculating_and_plotting.R")

# file_to_analyze <- file_loc

# where_to_plot <- plot_loc

# variable <- variable

# percentage_years_station <- 30 / 100
# percentage_one_station <- 30 / 100

# col_to_rm <- TRUE

sub_variables <- c("start", "end", "minima")

full_cluster_analysis <- function(file_to_analyze, where_to_plot, variable, percentage_years_station, percentage_one_station, col_to_rm = FALSE, dist_type) {
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

        plots <- make_silhouette_and_wss_plots(dist, hcut, 10, hc_func = "agnes", hc_method = name)

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

        for (number_of_clusters in 2:10) {
            hc <- as.hclust(agnes(dist, method = name))
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
