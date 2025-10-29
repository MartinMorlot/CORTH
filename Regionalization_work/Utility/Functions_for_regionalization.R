correlation_distance_two_vectors <- function(x, y) {
    dist <- NA
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (length(x) == length(y)) {
        x_na <- which(is.na(x))
        y_na <- which(is.na(y))

        x[y_na] <- NA
        y[x_na] <- NA
        x <- na.omit(x)
        y <- na.omit(y)
        x <- as.numeric(x)
        y <- as.numeric(y)
        if ((length(x) > 0) & (length(y) > 0)) dist <- 1 - cor(x, y)
    }
    return(dist)
}

correlation_distance_dataframe <- function(df_to_analyze) {
    n <- nrow(df_to_analyze)
    dist_df <- data.frame(matrix(NA, n, n), row.names = row.names(df_to_analyze))
    colnames(dist_df) <- row.names(dist_df)
    for (i in 1:n) {
        for (j in i:n) {
            d <- correlation_distance_two_vectors(df_to_analyze[i, ], df_to_analyze[j, ])
            if (j == i) {
                d <- 0
            }
            dist_df[i, j] <- dist_df[j, i] <- d
        }
    }
    return(dist_df)
}

make_silhouette_and_wss_pot <- function(distance, FUNcluster = NULL, k.max = 10, linecolor = "steelblue", ...) {
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
    ylab <- "Average silhouette width"
    p_wss <- ggpubr::ggline(df,
        x = "clusters", y = "z", group = 1, color = linecolor, ylab = "Total Within Sum of Square", xlab = "Number of clusters k",
        main = "Optimal number of clusters"
    )
    p_silhouette <- ggpubr::ggline(df,
        x = "clusters", y = "y", group = 1, color = linecolor, ylab = "Average silhouette width", xlab = "Number of clusters k",
        main = "Optimal number of clusters"
    ) + geom_vline(
        xintercept = which.max(v), linetype = 2,
        color = linecolor
    )
    return(list(p_wss, p_silhouette))
}
