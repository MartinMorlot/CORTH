library(ggplot2)


make_gap_plots_quick <- function(df, FUNcluster = hcut, k.max = 10) {
    gap_stat <- clusGap(df)
    p_fviz_gap <- fviz_gap_stat(gap_stat)
    ŕeturn(p_fviz_gap)
}


# Function to compute within-cluster dispersion (Wk) for a distance matrix
compute_Wk <- function(d, labels) {
    n <- nrow(d)
    clusters <- unique(labels)
    Wk <- 0
    for (k in clusters) {
        cluster_indices <- which(labels == k)
        if (length(cluster_indices) > 1) {
            # Sum of distances within the cluster
            Wk <- Wk + sum(d[cluster_indices, cluster_indices]) / (2 * length(cluster_indices))
        }
    }
    return(log(Wk))
}

compute_Wk <- function(d, labels) {
    n <- nrow(d)
    clusters <- unique(labels)
    Wk <- 0
    n_clusters <- length(clusters)
    if (n_clusters == 0) {
        return(NA)
    } # No clusters

    for (k in clusters) {
        cluster_indices <- which(labels == k)
        if (length(cluster_indices) > 1) {
            # Sum of distances within the cluster
            cluster_distances <- d[cluster_indices, cluster_indices]
            Wk <- Wk + sum(cluster_distances) / (2 * length(cluster_indices))
        }
    }
    # Only take log if Wk > 0
    if (Wk > 0) {
        return(log(Wk))
    } else {
        return(NA) # Or return a very small positive number, e.g., log(1e-10)
    }
}

# Function to generate reference distance matrices
generate_reference_d <- function(d, B = 100) {
    n <- nrow(d)
    ref_d <- array(0, dim = c(n, n, B))
    for (b in 1:B) {
        # Generate uniform random data within the range of the original data
        ref_data <- matrix(runif(n * ncol(x), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)),
            nrow = n
        )
        ref_d[, , b] <- as.matrix(dist(ref_data, method = "euclidean"))
    }
    return(ref_d)
}

generate_random_distance_matrix <- function(d, n, mean_d, sd_d) {
    # Initialize a matrix of zeros
    ref_d <- matrix(0, nrow = n, ncol = n)
    # Fill upper triangle with random distances
    ref_d[upper.tri(ref_d)] <- rnorm(sum(upper.tri(ref_d)), mean = mean_d, sd = sd_d)
    # Make it symmetric
    ref_d <- ref_d + t(ref_d)
    # Set diagonal to zero
    diag(ref_d) <- 0
    return(ref_d)
}

generate_reference_d <- function(d, B = 100) {
    n <- nrow(d)
    mean_d <- mean(d[upper.tri(d)])
    sd_d <- sd(d[upper.tri(d)])
    ref_d <- array(0, dim = c(n, n, B))
    for (b in 1:B) {
        ref_d[, , b] <- generate_random_distance_matrix(d, n, mean_d, sd_d)
    }
    return(ref_d)
}

# Function to compute the gap statistic
gap_statistic <- function(d, max_k = 10, B = 100) {
    # Clustering for real data
    hc <- hclust(d)
    cuts <- cutree(hc, h = 1:max_k)

    # transform
    d <- base::as.matrix(d)
    Wk <- sapply(1:max_k, function(k) compute_Wk(d, cuts[, k]))

    # Clustering for reference data
    ref_d <- generate_reference_d(d, B)
    Wkb <- matrix(NA, nrow = max_k, ncol = B)
    for (b in 1:B) {
        hc_ref <- hclust(as.dist(ref_d[, , b]))
        cuts_ref <- cutree(hc_ref, h = 1:max_k)
        Wkb[, b] <- sapply(1:max_k, function(k) compute_Wk(ref_d[, , b], cuts_ref[, k]))
    }

    # Compute gap statistic
    gap <- Wkb - matrix(rep(Wk, B), nrow = max_k, byrow = TRUE)
    gap <- rowMeans(gap)

    # Find optimal k
    optimal_k <- which.max(gap)

    return(list(gap = gap, optimal_k = optimal_k))
}

generate_reference_d <- function(x, B = 100) {
    n <- nrow(x)
    p <- ncol(x)
    ref_d <- array(0, dim = c(n, n, B))
    for (b in 1:B) {
        # Generate random data with the same dimensions as x
        ref_data <- matrix(
            runif(n * p, min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)),
            nrow = n
        )
        # Compute Euclidean distance matrix
        ref_d[, , b] <- as.matrix(dist(ref_data, method = "euclidean"))
    }
    return(ref_d)
}



make_gap_plots <- function(gap_stat_result, linecolor = "steelblue", ...) {
    # Example output from gap_statistic()
    gap <- gap_stat_result$gap # Your gap statistic values for k=1 to max_k
    k_values <- 1:length(gap) # The range of k values

    # Create a data frame for ggplot
    gap_df <- data.frame(k = k_values, gap = gap)

    # Plot
    p_gap <- ggplot(gap_df, aes(x = k, y = gap)) +
        geom_line(color = "blue", size = 1) +
        geom_point(color = "red", size = 3) +
        labs(
            title = "Gap Statistic for Optimal Number of Clusters",
            x = "Number of clusters (k)",
            y = "Gap statistic"
        ) +
        theme_minimal() +
        geom_vline(xintercept = result$optimal_k, linetype = "dashed", color = "green") +
        annotate("text",
            x = result$optimal_k, y = Inf, label = paste("Optimal k =", result$optimal_k),
            vjust = 2, hjust = 0, color = "green"
        )

    return(p_gap)
}
