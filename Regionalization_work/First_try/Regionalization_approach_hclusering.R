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


data_loaded <- read.csv("Regionalization_work/cleaned_up_data_fixed_elev.csv", row.names = 1)


data_loaded[] <- lapply(data_loaded, function(x) as.numeric(as.character(x)))
colSums(!is.na(data_loaded))

cnames <- colnames(data_loaded)

cnames

year_to_analyze <- as.character("2015":"2025")
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

write.csv(df, "Regionalization_work/Kept_data.csv")


# Dissimilarity matrix
d <- dist(df, method = "euclidean")


# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete")

# Plot the obtained dendrogram
p1 <- plot(hc1, cex = 0.6, hang = -1)

hc2 <- agnes(df, method = "complete")
p1 <- pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes complete")

hc3 <- agnes(df, method = "ward")
p2 <- pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes ward")

# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc


# plot dendrogram
p3 <- pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")


hc5 <- hclust(d, method = "ward.D2")
p4 <- plot(hc5, cex = 0.6, hang = -1, main = "Dendrogram of ward.d2 Agnes")

# Convert each object to dendrograms
d1 <- as.dendrogram(hc2)
d2 <- as.dendrogram(hc3)
d3 <- as.dendrogram(hc5)
d4 <- as.dendrogram(hc4)

# Convert dendrograms to ggplot objects
p1 <- ggdendrogram(d1, size = 2) + ggtitle("agnes complete")
p2 <- ggdendrogram(d2, size = 2) + ggtitle("agnes ward")
p3 <- ggdendrogram(d3, size = 2) + ggtitle("agnes ward.d2")
p4 <- ggdendrogram(d4, size = 2) + ggtitle("diana")

# Save arranged plot
png("Regionalization_work/Plots/hcluster_method_comparison.png",
    height = 3000, width = 3000, res = 300
)

grid.arrange(p1, p2, p3, p4, ncol = 2)

dev.off()


# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)

df %>%
    mutate(cluster = sub_grp) %>%
    head()

png("Regionalization_work/Plots/hcluster_4_group.png",
    height = 3000, width = 3000, res = 300
)
par(mfrow = c(1, 1))
plot(hc5, cex = 0.6, hang = -1, main = "Dendrogram of ward.d2 Agnes")
rect.hclust(hc5, k = 4, border = 2:5)
dev.off()


# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)

# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 4)

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)

tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

png("Regionalization_work/Plots/hcluster_entanglement.png",
    height = 3000, width = 3000, res = 300
)
tanglegram(dend1, dend2,
    highlight_distinct_edges = FALSE, # Turn-off dashed lines
    common_subtrees_color_lines = FALSE, # Turn-off line colors
    common_subtrees_color_branches = TRUE, # Color common branches
    main = paste("entanglement =", round(entanglement(dend_list), 2))
)
dev.off()

p1 <- fviz_nbclust(df, FUN = hcut, method = "wss")

p2 <- fviz_nbclust(df, FUN = hcut, method = "silhouette")

gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
p3 <- fviz_gap_stat(gap_stat)

png("Regionalization_work/Plots/hcluster_testing.png",
    height = 3000, width = 2000, res = 300
)
grid.arrange(p1, p2, p3, nrow = 3, ncol = 1)
dev.off()

write.csv(df, "Results_clustering.csv")
