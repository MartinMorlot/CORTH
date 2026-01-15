library(cluster) # clustering algorithms
library(factoextra) # clustering visualization
library(colorspace)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(patchwork)
library(zoo)
library(lubridate)
library(readr)
library(purrr)
library(parallel)

# Fonction pour calculer la dispersion intra-cluster
compute_dispersion <- function(distance, clusters) {
    n <- nrow(distance)
    dispersion <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            if (clusters[i] == clusters[j]) {
                dispersion <- dispersion + distance[i, j]
            }
        }
    }
    return(dispersion)
}

generate_reference_dist_matrix_existing <- function(dist_matrix) {
    n <- nrow(dist_matrix)
    ref_dist_matrix <- matrix(0, nrow = n, ncol = n)

    upper_distances <- dist_matrix[upper.tri(dist_matrix, diag = FALSE)]

    permuted_distances <- sample(upper_distances)

    ref_dist_matrix[upper.tri(ref_dist_matrix, diag = FALSE)] <- permuted_distances

    ref_dist_matrix[lower.tri(ref_dist_matrix, diag = FALSE)] <- t(ref_dist_matrix)[lower.tri(ref_dist_matrix, diag = FALSE)]

    return(ref_dist_matrix)
}


generate_reference_dist_matrix_new <- function(dist_matrix, B = 100) {
    n <- nrow(dist_matrix)

    # Extraire les distances au-dessus de la diagonale
    upper_distances <- dist_matrix[upper.tri(dist_matrix, diag = FALSE)]

    reference_datasets <- list()

    for (b in 1:B) {
        # Initialiser la matrice de référence
        ref_dist_matrix <- matrix(0, nrow = n, ncol = n)

        # Générer des distances aléatoires basées sur la distribution des distances réelles
        permuted_distances <- sample(upper_distances)

        # Remplir la partie supérieure de la matrice avec les distances permutées
        ref_dist_matrix[upper.tri(ref_dist_matrix, diag = FALSE)] <- permuted_distances

        # Remplir la partie inférieure de la matrice en copiant la partie supérieure
        ref_dist_matrix[lower.tri(ref_dist_matrix, diag = FALSE)] <- t(ref_dist_matrix)[lower.tri(ref_dist_matrix, diag = FALSE)]

        reference_datasets[[b]] <- ref_dist_matrix
    }

    return(reference_datasets)
}

gap_calculation <- function(distance, k = 10, ref_data, clusters, FUNcluster = hcut, hc_func = agnes, hc_method = "ward") {
    dist_matrix <- as.matrix(distance)
    real_dispersion <- log(compute_dispersion(dist_matrix, clusters$cluster))

    B <- length(ref_data)

    ref_dispersions <- numeric(length = B)
    for (b in seq_along(B)) {
        ref_dist_matrix <- ref_data[[b]]

        ref_hc <- FUNcluster(as.dist(ref_dist_matrix), k, hc_func = hc_func, hc_method = hc_method)
        ref_dispersions[b] <- log(compute_dispersion(ref_dist_matrix, ref_hc$cluster))
    }

    se_values <- sd(ref_dispersions) * sqrt(1 + 1 / B)

    gap_value <- mean(ref_dispersions) - real_dispersion
    return(list(value = gap_value, se = se_values))
}

# Fonction pour trouver le meilleur k selon Tibshirani
find_optimal_k_tibshirani <- function(gap_values, se_values) {
    max_k <- length(gap_values)
    optimal_k <- 1

    for (k in 2:(max_k)) {
        if (gap_values[k] >= gap_values[k + 1] - se_values[k + 1]) {
            optimal_k <- k
            break
        }
    }

    return(optimal_k)
}

make_nb_fitting_plots <- function(distance, FUNcluster = hcut, k.max = 10, linecolor = "steelblue", hc_func = agnes, hc_method = "ward") {
    v <- rep(NaN, k.max)
    w <- rep(NaN, k.max)
    z <- rep(NaN, k.max)
    a <- rep(NaN, k.max)

    dist_ref_data <- generate_reference_dist_matrix_new(as.matrix(distance), 30)

    for (i in 2:k.max) {
        clust <- FUNcluster(distance, i, hc_func = hc_func, hc_method = hc_method)
        v[i] <- factoextra:::.get_ave_sil_width(distance, clust$cluster)
        w[i] <- factoextra:::.get_withinSS(distance, clust$cluster)
        gap_k_combo <- gap_calculation(distance, i, dist_ref_data, clusters = clust, hc_func = hc_func, hc_method = hc_method)
        z[i] <- gap_k_combo[["value"]]
        a[i] <- gap_k_combo[["se"]]
    }

    clust1 <- FUNcluster(distance, 1, hc_func = hc_func, hc_method = hc_method)
    w[1] <- factoextra:::.get_withinSS(distance, clust1$cluster)
    df <- data.frame(
        clusters = as.factor(1:k.max), sil = v, wss = w, gap = z, se = a,
        stringsAsFactors = TRUE
    )
    p_gap <- ggpubr::ggline(df,
        x = "clusters", y = "gap", group = 1, color = linecolor, ylab = "Gap statistique", xlab = "Number of clusters k",
        main = "gap optimal number of clusters"
    ) + geom_errorbar(aes(ymin = gap - se, ymax = gap + se), width = 0.1, color = linecolor) + ggplot2::geom_vline(
        xintercept = find_optimal_k_tibshirani(z, a), linetype = 2,
        color = "red"
    )
    p_silhouette <- ggpubr::ggline(df,
        x = "clusters", y = "sil", group = 1, color = linecolor, ylab = "Average silhouette width", xlab = "Number of clusters k",
        main = "Silhouette Optimal number of clusters"
    ) + ggplot2::geom_vline(
        xintercept = which.max(v), linetype = 2,
        color = "red"
    )

    if (hc_method == "ward") {
        p_wss <- ggpubr::ggline(df,
            x = "clusters", y = "wss", group = 1, color = linecolor, ylab = "Total Within Sum of Square", xlab = "Number of clusters k",
            main = "TWSS optimal number of clusters"
        )
        return(list(p_gap, p_silhouette, p_wss))
    } else {
        return(list(p_gap, p_silhouette))
    }
}

plot_cluster <- function(data, word, cluster_number, year_to_analyze,
                         location_to_write, write = TRUE) {
    # ensure column length match
    data <- data[, seq_along(year_to_analyze), drop = FALSE]

    df <- as.data.frame(data)
    df$series_id <- seq_len(nrow(df))

    df_long <- df |>
        pivot_longer(
            cols = -series_id,
            names_to = "col_name",
            values_to = "value"
        ) |>
        group_by(series_id) |>
        mutate(col_idx = row_number()) |>
        ungroup() |>
        mutate(year = year_to_analyze[col_idx])

    # y limits
    ylim <- range(df_long$value, na.rm = TRUE)

    # ---- trim leading empty years ----
    first_valid_col <- min(df_long$col_idx[!is.na(df_long$value)])

    df_long <- df_long |>
        filter(col_idx >= first_valid_col)

    # ---- DROP NA YEARS EXPLICITLY ----
    df_long <- df_long |>
        filter(!is.na(year))
    # ---------------------------------

    year_levels <- year_to_analyze[first_valid_col:length(year_to_analyze)]

    df_long$year <- factor(
        df_long$year,
        levels = year_levels,
        ordered = TRUE
    )

    p <- ggplot(
        df_long,
        aes(
            x = year,
            y = value,
            group = series_id,
            color = series_id
        )
    ) +
        geom_line(na.rm = TRUE) +
        geom_point(na.rm = TRUE) +
        scale_color_viridis_c(guide = "none") +
        scale_x_discrete(drop = FALSE) +
        coord_cartesian(ylim = ylim) +
        labs(
            title = paste(word, "day per year, cluster number:", cluster_number),
            x = "Year",
            y = paste(word, "occurrence day (DOY)")
        ) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (write) {
        ggsave(
            filename = file.path(
                location_to_write,
                paste0(word, "_", cluster_number, ".png")
            ),
            plot = p,
            dpi = 300,
            width = 9,
            height = 6
        )
    }

    return(p)
}

plot_cluster_combined <- function(combined_data, cluster_number, year_to_analyze, location_to_write, hc, number_of_clusters, sub_grp) {
    colors_vir <- viridis::viridis(nrow(combined_data))
    for (sub_variable in c("start", "end")) {
        cnames_sel <- colnames(combined_data)
        cnames_to_keep <- grepv(sub_variable, cnames_sel)
        if (length(cnames_to_keep) > 0) {
            sel_variable_cluster <- combined_data[, cnames_to_keep]
            base_plot <- plot_cluster(
                sel_variable_cluster, sub_variable, cluster_number, year_to_analyze,
                "", FALSE
            )

            plot_name <- paste0(sub_variable, "_plot")

            assign(plot_name, base_plot)
        }
    }

    k_colors <- ggpubr:::.get_pal("default", k = number_of_clusters)

    k_colors_sel <- k_colors[cluster_number]

    k_colors <- rep("black", number_of_clusters)

    k_colors[cluster_number] <- k_colors_sel

    k_colors <- k_colors[unique(sub_grp[hc$order])]

    color_labels <- rep("black", length(hc$labels))

    r_sel <- match(row.names(combined_data), hc$labels)

    color_labels[r_sel] <- colors_vir

    dend_plot <- fviz_dend(hc, k = number_of_clusters, rect = TRUE, color_labels_by_k = FALSE, k_colors = k_colors, cex = 0.8)

    dend_plot$layers[[2]]$aes_params$colour <- color_labels[hc$order]


    combined_plot <- (start_plot / end_plot) | dend_plot
    p <- combined_plot + plot_layout(widths = c(2, 1))
    ggsave(
        filename = file.path(
            location_to_write,
            paste0("Combined_", cluster_number, ".png")
        ),
        plot = p,
        dpi = 300,
        width = 18,
        height = 12
    )
}

interpolate_data <- function(df) {
    df <- df %>%
        arrange(date)

    # Calculate time differences between consecutive dates
    time_diffs <- as.numeric(diff(df$date), units = "days")

    # Identify gaps > 365 days
    gaps <- c(FALSE, time_diffs > 365)

    # Split data into segments where gaps > 365 days
    split_points <- which(gaps)
    segments <- split(df, cumsum(c(0, split_points) <= seq_along(df$date) - 1))

    # Interpolate within each segment
    interpolated_segments <- lapply(segments, function(seg) {
        if (nrow(seg) > 1) {
            seg$value_m <- na.approx(seg$value_m)
        }
        return(seg)
    })

    # Combine segments back
    return(bind_rows(interpolated_segments))
}

add_na_for_gaps <- function(df) {
    df <- df %>%
        arrange(date) # Sort by date

    # Calculate time differences between consecutive dates
    time_diffs <- as.numeric(diff(df$date), units = "days")

    # Identify gaps > 365 days
    gaps <- which(time_diffs > 365)

    # If no gaps, return the original data
    if (length(gaps) == 0) {
        return(df)
    }

    # Calculate midpoints for all gaps
    mid_dates <- df$date[gaps] + (df$date[gaps + 1] - df$date[gaps]) / 2

    # Create new rows for all gaps
    new_rows <- data.frame(
        date = mid_dates,
        value_m = NA
    )

    # Insert new rows into the data frame
    # Find the row indices after each gap
    insert_indices <- gaps + row_number(gaps)

    # Split the data and insert new rows
    df <- bind_rows(
        df[1:gaps[1], ],
        new_rows[1, ],
        df[(gaps[1] + 1):nrow(df), ]
    )

    # For more than one gap, loop and insert
    if (length(gaps) > 1) {
        for (i in 2:length(gaps)) {
            df <- bind_rows(
                df[1:(gaps[i] + i - 1), ],
                new_rows[i, ],
                df[(gaps[i] + i):nrow(df), ]
            )
        }
    }

    return(df)
}


load_and_plot_groupped_correction <- function(correction_loc, stations_name, year_to_analyze, location_to_write, cluster_number) {
    stations_nb <- unlist(lapply(strsplit(stations_name, " "), "[", 1))

    file_loc <- paste0(correction_loc, stations_nb, ".csv")

    data_list <- lapply(seq_along(file_loc), function(i) {
        # Read the file
        df <- fread(file_loc[i])

        # Rename and keep only the first 3 columns
        df <- setnames(df, old = names(df), new = c("row_nb", "date", "value_m"))

        # Add the station_name column based on the file name
        df$station_name <- tools::file_path_sans_ext(basename(file_loc[i]))

        # Standardize the date column to type `Date`
        df$date <- as.Date(df$date)

        # Return the data frame
        return(df)
    })

    data <- bind_rows(data_list)

    data <- data %>%
        mutate(date = as.Date(date)) %>%
        arrange(station_name, date)

    data_interpolated <- data %>%
        group_by(station_name) %>%
        group_modify(~ add_na_for_gaps(.x)) %>%
        ungroup()

    data_interpolated$station_name <- factor(
        data_interpolated$station_name,
        levels = stations_nb
    )

    today <- Sys.Date()

    data_interpolated$value_m <- as.numeric(data_interpolated$value_m)

    p <- ggplot(data_interpolated, aes(x = date, y = value_m, color = station_name)) +
        geom_line() +
        labs(
            title = "Correction Station Data (Values in Cm)",
            x = "Date",
            y = "DeltaH Value (cm)",
            color = "Station"
        ) +
        xlim(min(sort(data_interpolated$date)[5]), today) +
        scale_color_viridis_d(option = "viridis") +
        ylim(0, min(data_interpolated$value_m, na.rm = T)) +
        theme_bw()


    ggsave(
        filename = file.path(
            location_to_write,
            paste0("Correction_", cluster_number, ".png")
        ),
        plot = p,
        dpi = 300,
        width = 9,
        height = 6
    )
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

scatter_plot_high_corr <- function(corr_matrix, corr_value, df, variable_name, anomaly = FALSE) {
    cor_table <- which(corr_matrix > corr_value, arr.ind = T)
    cor_table_row_rm <- which(cor_table[, "row"] == cor_table[, "col"])
    cor_table <- cor_table[-cor_table_row_rm, ]
    # Remove rows where (row, col) is the reverse of any other (col, row)
    cor_table_df <- data.frame(cor_table)
    cor_table_filtered <- cor_table_df[
        !duplicated(paste(pmin(cor_table_df$row, cor_table_df$col),
            pmax(cor_table_df$row, cor_table_df$col),
            sep = ","
        )),
    ]

    c_names <- colnames(df)
    c_names_sel_variable <- grep(variable_name, c_names)

    df_corr_data <- data.frame(matrix(NA, nrow = 0, ncol = 12))
    colnames(df_corr_data) <- c(
        "Station_1_fullname",
        "Station_2_fullname",
        "Combined_station_fullname",
        "Station_1_name",
        "Station_2_name",
        "Combined_station_name",
        "Station_1_code",
        "Station_2_code",
        "Combined_station_code",
        "Values_station_1",
        "Values_station_2",
        "Variable_name"
    )
    for (row_index in seq_len(nrow(cor_table_filtered))) {
        station1 <- df[cor_table_filtered$col[row_index], c_names_sel_variable]
        station2 <- df[cor_table_filtered$row[row_index], c_names_sel_variable]
        station1_vals <- as.numeric(station1)
        station2_vals <- as.numeric(station2)
        combined_NOT_NAs <- (!is.na(station1_vals)) & (!is.na(station2_vals))

        retained_station1_vals <- station1_vals[combined_NOT_NAs]
        retained_station2_vals <- station2_vals[combined_NOT_NAs]

        station1_name <- row.names(station1)
        station2_name <- row.names(station2)
        combined_name <- paste(station1_name, station2_name, sep = " - ")

        split_name1 <- strsplit(station1_name, "[ ]")[[1]]
        split_name2 <- strsplit(station2_name, "[ ]")[[1]]

        code1 <- split_name1[1]
        code2 <- split_name2[1]

        name1 <- paste(split_name1[2:length(split_name1)], collapse = " ")
        name2 <- paste(split_name2[2:length(split_name2)], collapse = " ")

        retained_variables_names <- colnames(station1)[combined_NOT_NAs]

        df_row_data <- data.frame(
            Station_1_fullname = station1_name,
            Station_2_fullname = station2_name,
            Combined_station_fullname = combined_name,
            Station_1_name = name1,
            Station_2_name = name2,
            Combined_station_name = paste(name1, name2, sep = " / "),
            Station_1_code = code1,
            Station_2_code = code2,
            Combined_station_code = paste(code1, code2, sep = " / "),
            Values_station_1 = retained_station1_vals,
            Values_station_2 = retained_station2_vals,
            Variable_name = retained_variables_names
        )

        df_corr_data <- rbind(df_corr_data, df_row_data)
    }

    variable_name_composed <- paste(variable_name, "day")

    if (anomaly) variable_name_composed <- paste(variable_name_composed, "anomaly")

    # Extract colors from Set1, Set2, and Set3
    set1_colors <- brewer.pal(8, "Set1")
    set2_colors <- brewer.pal(8, "Set2")
    set3_colors <- brewer.pal(12, "Set3")

    # Combine the colors (adjust the number of colors as needed)
    combined_colors <- c(set1_colors, set2_colors, set3_colors)

    p1 <- ggplot(df_corr_data, aes(x = Values_station_1, y = Values_station_2, fill = Combined_station_name)) +
        geom_point(size = 3, shape = 21) + # Adjust size as needed
        scale_fill_manual(values = combined_colors) +
        labs(
            title = "a)",
            x = paste0(variable_name_composed, " at station 1"),
            y = paste0(variable_name_composed, " at station 2"),
            color = ""
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom", # Place legend at the bottom
            legend.direction = "horizontal" # Arrange legend items horizontally
        )

    p2 <- ggplot(df_corr_data, aes(x = Values_station_1, y = Values_station_2, fill = Combined_station_name)) +
        geom_point(size = 3, shape = 21) + # Adjust size as needed
        geom_smooth(method = "lm") +
        scale_fill_manual(values = combined_colors) +
        labs(
            title = "b)",
            x = paste0(variable_name_composed, " at station 1"),
            y = paste0(variable_name_composed, " at station 2"),
            color = ""
        ) +
        facet_wrap(. ~ Combined_station_code) +
        theme_minimal() +
        theme(
            legend.position = "none", # Place legend at the bottom
            legend.direction = "horizontal" # Arrange legend items horizontally
        )

    scatter_p <- p1 + p2
    scatter_p <- scatter_p + theme(legend.justification = "right")

    return(scatter_p)
}
