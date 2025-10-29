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

data_loaded <- read.csv("Regionalization_work/cleaned_up_data.csv", row.names = 1)

data_per_col <- colSums(is.na(data_loaded))

cnames <- colnames(data_loaded)

year_to_analyze <- as.character("1986":"2025")
to_be_retained_simple <- c("start_day_", "end_day_", "minima_day_")

start_kept <- paste0(to_be_retained_simple[1], year_to_analyze)
end_kept <- paste0(to_be_retained_simple[2], year_to_analyze)
minima_kept <- paste0(to_be_retained_simple[3], year_to_analyze)
to_keep <- c(start_kept, end_kept, minima_kept)

kept_data <- data_loaded[, to_keep]

df <- kept_data

write.csv(df, "Regionalization_work/Kept_data.csv")

# Dissimilarity matrix
cor_dist <- as.dist(correlation_distance_dataframe(df))
cor_dist_fast <- as.dist(1 - cor(t(df), use = "pairwise.complete.obs"))
cor_dist_fast[is.na(cor_dist_fast)] <- 2
cor_dist[is.na(cor_dist)] <- 2
png("Regionalization_work/Plots/Correlation_distance_all.png", height = 1500, width = 2000, res = 300)
fviz_dist(cor_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()

png("Regionalization_work/Plots/Correlation_distance_fast.png", height = 1500, width = 2000, res = 300)
fviz_dist(cor_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()

d <- cor_dist

m <- c("average", "single", "complete", "ward", "weighted", "gaverage")
names(m) <- c("average", "single", "complete", "ward", "weighted", "gaverage")

# function to compute coefficient
ac <- function(x) {
    agnes(d, method = x)$ac
}

results_clust_type <- map_dbl(m, ac)

d_r <- diana(d)

results_clust_type["diana"] <- d_r$dc

write.csv(results_clust_type, "clustering_type_results.csv")

# if curious we can distangle these

k.max <- 2:10

plots_best <- make_silhouette_and_wss_pot(cor_dist, hcut, 10, hc_func = "agnes", hc_method = "ward")

hc <- hclust(as.hclust(cor_dist), method = "ward")
sub_grp_2 <- cutree(hc, k = 2)
sub_grp_4 <- cutree(hc, k = 4)

plot(hc, cex = 0.6)
rect.hclust(hc, k = 2, border = 2:3)

plot(hc, cex = 0.6)
rect.hclust(hc, k = 4, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp_2))
fviz_cluster(list(data = df, cluster = sub_grp_4))

# spaghetti par groupement 2 et 4
# start
# end
# date maxima

# carto
