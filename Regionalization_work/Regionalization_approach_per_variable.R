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
source("Regionalization_work/Utility/Regionalization_analysis_function.R")

year_to_analyze <- as.character("1986":"2025")

file_start <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/start_data_cluster.csv"
start_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/Start/"

file_end <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/end_data_cluster.csv"
end_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/End/"

file_minima <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/minima_data_cluster.csv"
minima_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/Minima/"

file_mixed <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Kept_data.csv"
mixed_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/All_variables/"

variables_to_iterate <- c("start", "end", "minima", "mixed")

dist_types <- c("correlation", "euclidean")
for (variable in variables_to_iterate) {
    print(variable)
    file_loc <- get(paste0("file_", variable))
    orig_loc <- get(paste0(variable, "_plot_loc"))
    for (dist_type in dist_types) {
        euclidean_condition <- FALSE
        plot_loc <- paste0(orig_loc, dist_type, "/")
        if (dist_type == "euclidean") {
            euclidean_condition <- TRUE
        }
        variable_result <- full_cluster_analysis(file_loc, plot_loc, variable, 30 / 100, 30 / 100, TRUE, euclidean_condition)
    }
}



### debug lines

