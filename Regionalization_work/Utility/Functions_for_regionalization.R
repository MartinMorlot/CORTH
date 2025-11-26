library(cluster) # clustering algorithms
library(factoextra) # clustering visualization
library(NbClust)
library(dendextend) # for comparing two dendrograms
library(colorspace)
library(ggplot2)
library(ggdendro)
library(tidyverse)
library(purrr)

make_silhouette_and_wss_plots <- function(distance, FUNcluster = NULL, k.max = 10, linecolor = "steelblue", ...) {
    v <- rep(0, k.max)
    w <- rep(0, k.max)
    for (i in 2:k.max) {
        clust <- FUNcluster(distance, i, ...)
        v[i] <- factoextra:::.get_ave_sil_width(distance, clust$cluster)
        w[i] <- factoextra:::.get_withinSS(distance, clust$cluster)
    }
    clust1 <- FUNcluster(distance, 1, ...)
    w[1] <- factoextra:::.get_withinSS(distance, clust1$cluster)
    df <- data.frame(
        clusters = as.factor(1:k.max), y = v, z = w,
        stringsAsFactors = TRUE
    )
    p_wss <- ggpubr::ggline(df,
        x = "clusters", y = "z", group = 1, color = linecolor, ylab = "Total Within Sum of Square", xlab = "Number of clusters k",
        main = "TWSS optimal number of clusters"
    )
    p_silhouette <- ggpubr::ggline(df,
        x = "clusters", y = "y", group = 1, color = linecolor, ylab = "Average silhouette width", xlab = "Number of clusters k",
        main = "Silhouette Optimal number of clusters"
    ) + ggplot2::geom_vline(
        xintercept = which.max(v), linetype = 2,
        color = linecolor
    )

    return(list(p_wss, p_silhouette))
}



plot_cluster <- function(data, word, cluster_number, year_to_analyze, location_to_write) {
    min_start <- min(data, na.rm = T)
    max_start <- max(data, na.rm = T)
    ylim <- c(min_start, max_start)
    ylab <- paste(word, "occurence day (DOY)")
    xlab <- "Year"
    main <- paste(word, "day per year, cluster number:", cluster_number)
    file_name <- paste0(location_to_write, "/", word, "_", cluster_number, ".png")
    colors <- viridis::viridis(nrow(data))
    png(file_name, res = 300, height = 1800, width = 2700)
    for (i in seq(nrow(data))) {
        print(i)
        if (i == 1) {
            plot(year_to_analyze, data[i, ], type = "o", ylab = ylab, xlab = xlab, ylim = ylim, col = colors[i], main = main)
        } else {
            lines(year_to_analyze, data[i, ], type = "o", col = colors[i])
        }
    }
    dev.off()
}

distance_calc_clustering_method <- function(distance, location_to_write = NA, methods_to_consider = c("average", "single", "complete", "ward", "weighted", "gaverage", "diana")) {
    methods <- methods_to_consider

    diana_loc <- which(methods == "diana")
    diana_loc_cond <- length(diana_loc) > 0

    if (diana_loc_cond) {
        methods <- methods[-diana_loc]
    }

    m <- methods
    names(m) <- methods


    # function to compute coefficient
    ac <- function(x) {
        agnes(distance, method = x)$ac
    }

    results_clust_type <- map_dbl(m, ac)

    if (diana_loc_cond) {
        d_r <- diana(distance)
        results_clust_type["diana"] <- d_r$dc
    }

    if (!is.na(location_to_write)) {
        dataframe_results <- data.frame(matrix(NA, 1, length(methods_to_consider)))
        colnames(dataframe_results) <- names(results_clust_type)
        dataframe_results[1, ] <- results_clust_type
        write.csv(dataframe_results, location_to_write)
    }

    return(results_clust_type)
}
