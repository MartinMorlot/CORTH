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

where_to_plot <- "Regionalization_work/Plots/Start/"


full_cluster_analysis <- function(file_to_analyze, where_to_plot) {
    data_cluster <- read.csv(file_to_analyze)
    row.names(data_cluster) <- data_cluster$X
    df <- data_cluster[, -c(1, ncol(data_cluster))]

    cor_dist <- as.dist(1 - cor(t(df), use = "pairwise.complete.obs"))
    cor_dist[is.na(cor_dist)] <- 2

    corr_plot_location <- paste0(where_to_plot, "Correlation_distance.png")
    png(corr_plot_location, height = 1500, width = 2000, res = 300)
    fviz_dist(cor_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
    dev.off()

    best_clustering_method <- distance_calc_clustering_method(cor_dist)

    print(best_clustering_method)

    plots_best_agnes_ward <- make_silhouette_and_wss_pot(cor_dist, hcut, 10, hc_func = "agnes", hc_method = "ward")

    where_to_plot <- "Regionalization_work/Plots/Start/"


    wss_plot_location <- paste0(where_to_plot, "number_cluster_fitting_wss.png")
    png(wss_plot_location, height = 1500, width = 2000, res = 300)
    plots_best_agnes_ward[[1]]
    dev.off()

    silhouette_plot_location <- paste0(where_to_plot, "number_cluster_fitting_silhouette.png")
    png(silhouette_plot_location, height = 1500, width = 2000, res = 300)
    plots_best_agnes_ward[[2]]
    dev.off()
}

file_start <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/start_data_cluster.csv"
start_plot_loc <- "Regionalization_work/Plots/Start/"

file_end <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/end_data_cluster.csv"
end_plot_loc <- "Regionalization_work/Plots/End/"

file_minima <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/minima_data_cluster.csv"
minima_plot_loc <- "Regionalization_work/Plots/Minima/"



variables_to_iterate <- c("start", "end", "minima")

for (variable in variables_to_iterate) {
    print(variable)
    file_loc <- get(paste0("file_", variable))
}





start_data_cluster <- read.csv("/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/start_data_cluster.csv")
row.names(start_data_cluster) <- start_data_cluster$X
df_start <- start_data_cluster[, -c(1, ncol(start_data_cluster))]

# make an end table by cluster and station and year
end_data_cluster <- read.csv()
row.names(end_data_cluster) <- end_data_cluster$X
df_end <- end_data_cluster[, -c(1, ncol(end_data_cluster))]




# make an minima table by cluster and station and year
minima_data_cluster <- read.csv("/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/minima_data_cluster.csv")
row.names(minima_data_cluster) <- minima_data_cluster$X
df_minima <- minima_data_cluster[, -c(1, ncol(minima_data_cluster))]



cor_dist_minima <- as.dist(1 - cor(t(df_minima), use = "pairwise.complete.obs"))
cor_dist_minima[is.na(cor_dist_minima)] <- 2

cor_dist_end <- as.dist(1 - cor(t(df_end), use = "pairwise.complete.obs"))
cor_dist_end[is.na(cor_dist_end)] <- 2

cor_dist_start <- as.dist(1 - cor(t(df_start), use = "pairwise.complete.obs"))
cor_dist_start[is.na(cor_dist_start)] <- 2

png("Regionalization_work/Plots/Start/Correlation_distance.png", height = 1500, width = 2000, res = 300)
fviz_dist(cor_dist_start, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()

png("Regionalization_work/Plots/End/Correlation_distance.png", height = 1500, width = 2000, res = 300)
fviz_dist(cor_dist_end, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()

png("Regionalization_work/Plots/Minima/Correlation_distance.png", height = 1500, width = 2000, res = 300)
fviz_dist(cor_dist_minima, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()

distance_calc_clustering_method(cor_dist_minima)


distance_calc_clustering_method(cor_dist_end)


distance_calc_clustering_method(cor_dist_start)


k.max <- 2:10

plots_best_minima <- make_silhouette_and_wss_pot(cor_dist_minima, hcut, 10, hc_func = "agnes", hc_method = "ward")

plots_best_minima <- make_silhouette_and_wss_pot(cor_dist_minima, hcut, 10, hc_func = "agnes", hc_method = "ward")

plots_best_minima <- make_silhouette_and_wss_pot(cor_dist_minima, hcut, 10, hc_func = "agnes", hc_method = "ward")
