correlation_calc <- function(df) {
    corr <- cor(t(df), use = "pairwise.complete.obs")
    return(corr)
}

cor_with_min_obs <- function(x, min_obs = 5, transpose = TRUE) {
    if (transpose) {
        x <- t(x)
    }
    n <- ncol(x)
    cor_matrix <- matrix(NA, nrow = n, ncol = n)
    rownames(cor_matrix) <- colnames(x)
    colnames(cor_matrix) <- colnames(x)

    for (i in 1:n) {
        for (j in 1:n) {
            complete_cases <- !is.na(x[, i]) & !is.na(x[, j])
            n_obs <- sum(complete_cases)
            if (n_obs >= min_obs) {
                cor_matrix[i, j] <- cor(x[complete_cases, i], x[complete_cases, j], use = "complete.obs")
            } else {
                cor_matrix[i, j] <- NA
            }
        }
    }
    if (transpose) {
        cor_matrix <- t(cor_matrix)
    }
    return(cor_matrix)
}

clean_correlation <- function(corr, df, cor_function, min_obs, init_NA_corr = 10) {
    # check for presence of NA,
    NA_corr <- rowSums(is.na(corr))

    rm_row_corr_index <- which(row.names(df) %in% names(which(NA_corr >= init_NA_corr)))

    iteration <- 1
    while (length(rm_row_corr_index) > 0) {
        cat("Iteration: ", iteration, "\n")
        df <- df[-rm_row_corr_index, ]

        rownames_of_df <- row.names(df)

        corr <- cor_function(df, min_obs)
        NA_corr <- rowSums(is.na(corr))
        if (all(NA_corr == 0)) break
        sorted_NA_corr <- sort(NA_corr, decreasing = TRUE)
        nb_NA_corr <- sorted_NA_corr[1]
        index_nb_NA_corr <- which(rownames_of_df == names(nb_NA_corr))
        if (length(index_nb_NA_corr) > 1) {
            NA_df_row_NA_corr <- rowSums(is.na(df[index_nb_NA_corr, ]))
            name_rm_row_corr <- names(sort(NA_df_row_NA_corr, decreasing = FALSE)[1])
            rm_row_corr_index <- which(rownames_of_df == name_rm_row_corr)
        } else {
            rm_row_corr_index <- index_nb_NA_corr
        }
        iteration <- iteration + 1
    }

    corr <- cor_function(df, min_obs)

    if (any(is.na(corr))) {
        cat("This Failed!")
        return(NULL)
    }

    return(list(corr = corr, df = df))
}

dist_correlation <- function(corr) {
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
    df_with_data[] <- lapply(df_with_data, function(x) {
        as.numeric(as.character(x))
    })
    dist_calc <- dist_function(df_with_data)
    if (clean) {
        na_vals <- is.na(dist_calc)
        row_with_na <- which(rowSums(na_vals) > 0)
        dist_fix <- dist_calc[-row_with_na, ]
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

anomaly_per_vector <- function(x) {
    # if (!(class(x) == vector)) {
    #     return(NA)
    # }

    if (all(is.na(x))) {
        return(NA)
    }
    mean_x <- mean(x, na.rm = T)
    std_dev_x <- sd(x, na.rm = T)
    anomaly_x <- (x - mean_x) / std_dev_x

    return(anomaly_x)
}


calculate_anomaly_per_variable <- function(df, sub_variables) {
    cnames_df <- colnames(df)
    anomaly_df <- df
    for (variable_it in sub_variables) {
        c_retained <- grep(variable_it, cnames_df)
        if (length(c_retained) == 0) next
        for (row_it in seq_len(nrow(df))) {
            data <- as.numeric(df[row_it, ])[c_retained]
            anomaly_data <- anomaly_per_vector(data)
            anomaly_df[row_it, c_retained] <- anomaly_data
        }
    }
    return(anomaly_df)
}
