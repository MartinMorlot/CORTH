library(terra)
library(dplyr)
library(lubridate)

merge_spatvectors <- function(vec_list, all_cols = NULL) {
    # If no columns specified, detect all unique names
    if (is.null(all_cols)) {
        all_cols <- unique(unlist(lapply(vec_list, names)))
    }

    # Harmonize each SpatVector
    vec_list_aligned <- lapply(vec_list, function(v) {
        missing_cols <- setdiff(all_cols, names(v))
        if (length(missing_cols) > 0) {
            for (col in missing_cols) {
                v[[col]] <- NA
            }
        }
        v <- v[[all_cols]]
        return(v)
    })

    # Merge them
    merged_vec <- do.call(rbind, vec_list_aligned)
    return(merged_vec)
}


load_data_from_db_files <- function(
    file_list) {
    i <- 0
    for (file_name in file_list) {
        region <- unlist(lapply(strsplit(file_name, "_"), "[[", 2))
        file_content <- read.csv(file_name)
        nb_rows <- nrow(file_content)
        file_content$region <- rep(region, nb_rows)
        if (nb_rows > 0) {
            if (i == 0) {
                resulting_data <- file_content
            } else {
                resulting_data <- bind_rows(resulting_data, file_content)
            }
            i <- i + 1
        }
    }
    if (exists("resulting_data")) {
        return(resulting_data)
    }
}

load_stations <- function(files_station) {
    merged_content <- NULL

    for (station_file in files_station) {
        content <- read.csv(station_file)
        region <- unlist(lapply(strsplit(station_file, "_"), "[[", 2))
        content$region <- region
        names_of_columns <- colnames(content)
        if (station_file == files_station[1]) {
            default_columns <- names_of_columns
            merged_content <- content
        }

        not_there <- which(!names_of_columns %in% default_columns)
        if (length(not_there) > 0) {
            content <- content[, -not_there]
        }

        if (station_file != files_station[1]) {
            default_columns <- names_of_columns
            merged_content <- bind_rows(merged_content, content)
        }
    }
    return(merged_content)
}

sel_data_from_station <- function(df, nosta, region) {
    sel <- which((df$nosta == nosta) & (df$region == region))

    df <- df[sel, ]
    return(df)
}

date_load_and_correction <- function(df, date_format, col_to_add, col_to_transform, reorder = FALSE) {
    df[[col_to_add]] <- as.POSIXct(df[[col_to_transform]], tz = "UTC", format = date_format)

    loc_of_years_to_be_potentially_corrected <- which(as.numeric(format(df[[col_to_add]], "%Y")) > 2025)


    if (length(loc_of_years_to_be_potentially_corrected) > 0) {
        years_2digit_in_question <- unlist(lapply(strsplit(unlist(lapply(strsplit(df[[col_to_transform]][loc_of_years_to_be_potentially_corrected], " "), "[[", 1)), "[/]"), "[[", 3))
        for (i in seq_along(loc_of_years_to_be_potentially_corrected)) {
            digit_year <- as.numeric(years_2digit_in_question[i])
            if (digit_year <= 70) {
                loc_to_fix <- loc_of_years_to_be_potentially_corrected[i]
                df[loc_to_fix, col_to_add] <- df[loc_to_fix, col_to_add] - years(100)
            }
        }
    }

    if (reorder) {
        order <- match(sort(df[[col_to_add]]), df[[col_to_add]])
        df <- df[order, ]
    } else {
        if (nrow(df) > 1) {
            year_last <- as.numeric(format(df[nrow(df), col_to_add], "%Y"))
            year_second_to_last <- as.numeric(format(df[nrow(df) - 1, col_to_add], "%Y"))

            if (year_second_to_last > year_last) {
                df[nrow(df), col_to_add] <- df[nrow(df), col_to_add] + years(100)
            }
        }
    }

    return(df)
}

filter_intervals <- function(intervals_matrix, values_vector, remove_na = TRUE, remove_zero = TRUE) {
    # Keep only intervals with no NAs in between
    if (remove_na) {
        keep <- apply(intervals_matrix, 1, function(row_mat) all(!is.na(values_vector[(row_mat[1] + 1):(row_mat[2] - 1)])))
        intervals_matrix <- intervals_matrix[keep, , drop = FALSE]
    }

    # Keep only intervals with no zeros in between
    if (remove_zero && nrow(intervals_matrix) > 0) {
        keep <- apply(intervals_matrix, 1, function(row_mat) all(values_vector[(row_mat[1] + 1):(row_mat[2] - 1)] != 0))
        intervals_matrix <- intervals_matrix[keep, , drop = FALSE]
    }

    return(intervals_matrix)
}



get_data_for_specific_year <- function(df, minima_all, value_col = "valeur", date_col = "dateOK") {
    # Extract vectors
    values <- as.numeric(df[[value_col]])
    dates <- as.POSIXct(df[[date_col]])

    values[values > 0] <- NA

    # Indices of zeros
    zero_idx <- which(values == 0)
    if (length(zero_idx) < 2) {
        return(NULL)
    } # not enough zeros to form an interval

    # Build all consecutive zero intervals
    intervals <- cbind(head(zero_idx, -1), tail(zero_idx, -1))

    intervals <- filter_intervals(intervals, values)

    if (nrow(intervals) == 0) {
        return(NULL)
    }

    # Compute durations and pick the longest
    starts <- as.integer(intervals[, 1])
    ends <- as.integer(intervals[, 2])

    # explicit difftime in days
    durations <- as.numeric(difftime(dates[ends], dates[starts], units = "days"))

    longest <- intervals[which.max(durations), , drop = FALSE]

    # Extract data for the interval
    subdf <- df[longest[1]:longest[2], ]

    # Minima
    min_idx <- which.min(subdf[[value_col]])
    minima_day <- as.numeric(format(subdf[[date_col]][min_idx], "%j"))
    minima_value <- subdf[[value_col]][min_idx]

    start_day <- as.numeric(format(subdf[[date_col]][1], "%j"))
    end_day <- as.numeric(format(subdf[[date_col]][nrow(subdf)], "%j"))

    # Create daily sequence
    x_daily <- seq(min(subdf[[date_col]]), max(subdf[[date_col]]), by = "day")

    # Linear interpolation at daily resolution
    y_daily <- approx(subdf[[date_col]], subdf[[value_col]], xout = x_daily)$y

    # Integrate again via trapezoidal rule (same math, but now evenly spaced)
    integral <- sum(diff(as.numeric(x_daily)) * (head(y_daily, -1) + tail(y_daily, -1)) / 2)
    # Return result as a list

    results_list <- list(
        start_day = start_day,
        end_day = end_day,
        duration = end_day - start_day,
        minima_day = minima_day,
        minima_value = minima_value,
        minima_norm_all = minima_value / minima_all,
        integral = integral,
        integral_norm_year = integral / minima_value,
        integral_norm_all = integral / minima_all
    )

    return(results_list)
}
