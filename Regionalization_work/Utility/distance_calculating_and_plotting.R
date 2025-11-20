correlation_calc <- function(df) {
    corr <- cor(t(df), use = "pairwise.complete.obs")
    return(corr)
}

dist_absolute_correlation <- function(df) {
    corr <- correlation_calc(df)
    dist <- sqrt(2 * ((1 - corr)**2))
    return(dist)
}

dist_correlation <- function(df) {
    corr <- correlation_calc(df)
    dist <- sqrt(0.5 * (1 - corr))
    return(dist)
}



dist_euclidean <- function(df) {
    n <- nrow(df)
    d <- matrix(NA, n, n)
    for (i in 1:n) {
        for (j in 1:n) {
            # Find columns without NAs in either row i or j
            cols <- which(!is.na(df[i, ]) & !is.na(df[j, ]))
            d[i, j] <- sqrt(sum((df[i, cols] - df[j, cols])^2))
            if (length(cols) == 0) d[i, j] <- NA
        }
    }
    dist <- as.data.frame(d)
    row.names(dist) <- row.names(df)
    return(dist)
}


dist_euclidean_eq <- function(df) {
    n <- nrow(df)
    d <- matrix(NA, n, n)
    for (i in 1:n) {
        for (j in 1:n) {
            # Find columns without NAs in either row i or j
            cols <- which(!is.na(df[i, ]) & !is.na(df[j, ]))
            d[i, j] <- sqrt(sum((df[i, cols] - df[j, cols])^2)) / length(cols)
            if (length(cols) == 0) d[i, j] <- NA
        }
    }
    dist <- as.data.frame(d)
    row.names(dist) <- row.names(df)
    return(dist)
}

dist_calculation <- function(df_with_data, dist_type, clean = FALSE) {
    dist_function <- get(paste0("dist_", dist_type))
    dist_calc <- dist_function(df_with_data)
    if (clean) {
        na_vals <- is.na(dist_calc)
        col_with_na <- which(colSums(na_vals) > 0)
        row_with_na <- which(rowSums(na_vals) > 0)
        dist_fix <- dist_calc[-row_with_na, -col_with_na]
        dist <- as.dist(dist_fix)
        names_row_df <- row.names(df_with_data)
        df_row_to_be_removed <- which(!names_row_df %in% row.names(dist_fix))
        df_to_return <- df_with_data[-df_row_to_be_removed, ]
        return(list(dist, df_to_return))
    } else {
        dist <- as.dist(dist_calc)
        return(dist)
    }
}

# new_dist_plot <- function(
#     dist.obj, show_labels = TRUE, lab_size = NULL,
#     gradient = list(low = "red", mid = "white", high = "blue")) {
#     if (!inherits(dist.obj, "dist")) {
#         stop("An object of class dist is required.")
#     }

#     dist.obj_mat <- as.matrix(dist.obj)

#     rownames(dist.obj_mat) <- colnames(dist.obj_mat) <- row.names(dist.obj)

#     d <- reshape2::melt(dist.obj_mat)
#     p <- ggplot(d, aes_string(x = "Var1", y = "Var2")) +
#         ggplot2::geom_tile(aes_string(fill = "value"))
#     if (is.null(gradient$mid)) {
#         p <- p + ggplot2::scale_fill_gradient(
#             low = gradient$low,
#             high = gradient$high
#         )
#     } else {
#         p <- p + ggplot2::scale_fill_gradient2(
#             midpoint = mean(dist.obj_mat, na.rm = T),
#             low = gradient$low, mid = gradient$mid, high = gradient$high,
#             space = "Lab"
#         )
#     }
#     if (show_labels) {
#         p <- p + theme(
#             axis.title.x = element_blank(), axis.title.y = element_blank(),
#             axis.text.x = element_text(
#                 angle = 45, hjust = 1,
#                 size = lab_size
#             ), axis.text.y = element_text(size = lab_size)
#         )
#     } else {
#         p <- p + theme(
#             axis.text = element_blank(), axis.ticks = element_blank(),
#             axis.title.x = element_blank(), axis.title.y = element_blank()
#         )
#     }
#     return(p)
# }
