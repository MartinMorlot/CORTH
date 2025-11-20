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

all_data_df <- read.csv("Regionalization_work/cleaned_up_data_2.csv")

names_for_row <- all_data_df[, "X"]

df <- all_data_df

rownames(df) <- names_for_row

cnames <- colnames(df)


# Start date for each year
start_col <- which(grepl("start_day", cnames))
# End date for each year
end_col <- which(grepl("end_day", cnames))

# valeur minimal date
minimal_date_col <- which(grepl("minima_day", cnames))

all_cols <- cnames[c(
    start_col,
    end_col,
    minimal_date_col
)]

data_from_df <- df[, all_cols]


df_start <- df[, c(start_col)]
file_start <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster_2/start_data_cluster_2.csv"
start_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots_2/Start/"

df_end <- df[, end_col]
file_end <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster_2/end_data_cluster_2.csv"
end_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots_2/End/"

df_minima <- df[, minimal_date_col]
file_minima <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster_2/minima_data_cluster_2.csv"
minima_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots_2/Minima/"

df_mixed <- data_from_df
file_mixed <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Per_cluster_2/Kept_data_2.csv"
mixed_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots_2/All_variables/"

dir.create("/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster_2/")

variables_to_iterate <- c("start", "end", "minima", "mixed")

dist_types <- c("correlation", "euclidean")

for (variable in variables_to_iterate) {
    print(variable)
    df_sel <- get(paste0("df_", variable))
    file_loc <- get(paste0("file_", variable))
    df_sel[] <- lapply(df_sel, as.numeric)
    write.csv(df_sel, file_loc)
    orig_loc <- get(paste0(variable, "_plot_loc"))
    percentage_one_station <- 40 / 100
    percentage_year_station <- 40 / 100
    for (dist_type in dist_types) {
        euclidean_condition <- FALSE
        plot_loc <- paste0(orig_loc, dist_type, "/")
        if (dist_type == "euclidean") {
            euclidean_condition <- TRUE
        }
        variable_result <- full_cluster_analysis(file_loc, plot_loc, variable, percentage_year_station, percentage_one_station, euclidean_condition)
    }
}

file_to_analyze <- file_start
where_to_plot <- plot_loc
