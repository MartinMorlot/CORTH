rm(list = ls())
gc()

library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(colorspace)
library(ggplot2)
library(ggdendro)
library(gridExtra)

source("Regionalization_work/Utility/Functions_for_regionalization.R")

year_to_analyze <- as.character("1986":"2025")

full_cluster_analysis <- function(file_to_analyze, where_to_plot, variable) {
    data_cluster <- read.csv(file_to_analyze)
    row.names(data_cluster) <- data_cluster$X
    df <- data_cluster[, -c(1, ncol(data_cluster))]

    dir.create(where_to_plot, recursive = T)

    cor_dist <- as.dist(1 - cor(t(df), use = "pairwise.complete.obs"))
    cor_dist[is.na(cor_dist)] <- 2

    corr_plot_location <- paste0(where_to_plot, "Correlation_distance.png")
    png(corr_plot_location, height = 1500, width = 2000, res = 300)
    fviz_dist(cor_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
    dev.off()

    best_clustering_method <- distance_calc_clustering_method(cor_dist)

    sorted_clustering <- sort(best_clustering_method, decreasing = TRUE)

    print(sorted_clustering[1:3])

    best_3 <- names(sorted_clustering)[1:3]

    for (name in best_3) {
        dir.create(paste0(where_to_plot, name), recursive = T)

        plots <- make_silhouette_and_wss_pot(cor_dist, hcut, 10, hc_func = "agnes", hc_method = name)

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

        for (number_of_clusters in 2:10) {
            hc <- as.hclust(agnes(cor_dist, method = name))
            sub_grp <- cutree(hc, k = number_of_clusters)

            main_plot <- paste(variable, name, number_of_clusters)

            nb_plot_loc <- paste0(where_to_plot, name, "/", number_of_clusters)

            dir.create(nb_plot_loc, recursive = T)

            cluster_number_plot_loc <- paste0(where_to_plot, name, "/", number_of_clusters, "/hclust.png")
            png(cluster_number_plot_loc, height = 2500, width = 2000, res = 300)
            plot(hc, cex = 0.6, main = name)
            rect.hclust(hc, k = number_of_clusters, border = 2:3)
            dev.off()

            df_with_cluster <- df %>%
                mutate(cluster = sub_grp)

            cnames <- colnames(df_with_cluster)
            cluster_col <- cnames[length(cnames)]
            cluster_numbers <- unique(df_with_cluster[, cluster_col])

            cluster_col_data <- df_with_cluster[, cluster_col]


            for (cluster_number in seq_along(cluster_numbers)) {
                row_sel <- which(cluster_col_data == cluster_number)

                select_start_cluster <- df[row_sel, ]
                plot_cluster(
                    select_start_cluster, variable, cluster_number, year_to_analyze,
                    nb_plot_loc
                )
            }
        }
    }
}

file_start <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/start_data_cluster.csv"
start_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/Start/"

file_end <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/end_data_cluster.csv"
end_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/End/"

file_minima <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/minima_data_cluster.csv"
minima_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/Minima/"



variables_to_iterate <- c("start", "end", "minima")

for (variable in variables_to_iterate) {
    print(variable)
    file_loc <- get(paste0("file_", variable))
    plot_loc <- get(paste0(variable, "_plot_loc"))
    full_cluster_analysis(file_loc, plot_loc, variable)
}
