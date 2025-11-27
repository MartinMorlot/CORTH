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



    p1 <- ggplot(df_corr_data, aes(x = Values_station_1, y = Values_station_2, fill = Combined_station_name)) +
        geom_point(size = 3, shape = 21) + # Adjust size as needed
        scale_fill_brewer(palette = "Set3") +
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
        scale_fill_brewer(palette = "Set3") +
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
