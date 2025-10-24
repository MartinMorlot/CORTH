rm(list = ls())
gc()

library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering visualization
library(colorspace)
library(dendextend) # for comparing two dendrograms
library(gridExtra)

data_loaded <- read.csv("Regionalization_work/cleaned_up_data_fixed_elev.csv", row.names = 1)

data_loaded[] <- lapply(data_loaded, function(x) as.numeric(as.character(x)))
colSums(!is.na(data_loaded))

cnames <- colnames(data_loaded)

cnames

year_to_analyze <- as.character("2015":"2020")
to_be_retained_simple <- c("start", "end", "duration")

# to_keep <- cnames[1:6]
to_keep <- c()

for (name in cnames) {
    for (year in year_to_analyze) {
        if (grepl(year, name)) {
            to_keep <- c(to_keep, name)
        }
    }
}
to_keep <- to_keep[1:24]

kept_data <- data_loaded[, to_keep]

df <- kept_data

df <- na.omit(df)

png("Regionalization_work/Plots/Eucledian_distance.png", height = 3000, width = 3000, res = 300)
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()

k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = df) + ggtitle("k = 5")

png("Regionalization_work/Plots/kmeans_cluster_nb_comparison.png", height = 3000, width = 3000, res = 300)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

set.seed(123)

p1 <- fviz_nbclust(df, kmeans, method = "wss")

p2 <- fviz_nbclust(df, kmeans, method = "silhouette")

gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 7, B = 50)

p3 <- fviz_gap_stat(gap_stat)

final <- kmeans(df, 5, nstart = 25)
p4 <- fviz_cluster(final, data = df)

png("Regionalization_work/Plots/kmeans_analysis.png", height = 3000, width = 3000, res = 300)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

results <- write.csv(df, "Regionalization_work/Results/Results_kmeans.csv")
