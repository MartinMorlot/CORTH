rm(list = ls())
gc()

source("R/Regionalization_analysis_function.R")

year_to_analyze <- as.character("1986":"2025")

plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots"
unlink(plot_loc, recursive = T)

file_start <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/start_data_cluster.csv"
start_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/Start/"

file_end <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/end_data_cluster.csv"
end_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/End/"

file_minima <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Results/Per_cluster/minima_data_cluster.csv"
minima_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/Minima/"

file_mixed <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Kept_data.csv"
mixed_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/All_variables/"

file_combined_start_end <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Combined_data_start_end.csv"
combined_start_end_plot_loc <- "/home/mmorlot/dev-work/CORTH/Regionalization_work/Plots/Combined_start_end/"

correction_data_loc <- "/home/mmorlot/dev-work/CORTH/Data_correction_databases/"

variables_to_iterate <- c("start", "end", "minima", "mixed", "combined_start_end")

for (variable in variables_to_iterate) {
    print(variable)
    file_loc <- get(paste0("file_", variable))
    orig_loc <- get(paste0(variable, "_plot_loc"))
    unlink(orig_loc, recursive = TRUE)
    plot_loc <- paste0(orig_loc)
    for (anomaly_to_run in c(TRUE, FALSE)) {
        variable_result <- full_cluster_analysis(file_loc, plot_loc, variable, TRUE, anomaly_to_run, correction_data_loc)
    }
}
