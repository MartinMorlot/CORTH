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
    file_list
) {
    i <- 0
    for(file_name in file_list){
        region <-unlist(lapply(strsplit(file_name, "_"), "[[", 2))
        file_content <- read.csv(file_name)
        nb_rows <- nrow(file_content)
        file_content$region <- rep(region, nb_rows)
        if(nb_rows > 0){
            if( i == 0){
                resulting_data <- file_content
            } else {

                resulting_data <- bind_rows(resulting_data, file_content)
            }
            i <- i+1
        }
    }
    if(exists("resulting_data")) {
        return(resulting_data)
    }
}

load_stations <- function(files_station) {
    merged_content <- NULL

    for(station_file in files_station){
        content <- read.csv(station_file)
        region <-unlist(lapply(strsplit(station_file, "_"), "[[", 2))
        content$region <- region 
        names_of_columns = colnames(content)
        if(station_file == files_station[1]){
            default_columns = names_of_columns
            merged_content = content
        }

        not_there=which(!names_of_columns %in% default_columns)
        if(length(not_there) > 0){
            content <- content[,-not_there]
        }

        if(station_file != files_station[1]){
            default_columns = names_of_columns
            merged_content = bind_rows(merged_content, content)
        }
    }
    return(merged_content)
}

sel_data_from_station <- function(df, nosta, region) {

    sel <- which((df$nosta == nosta) & (df$region == region))

     df <- df[ sel,]
     return(df)
}


date_load_and_correction <- function(df, date_format, col_to_add, col_to_transform) {

    print(df[,col_to_transform])

    df[,col_to_add] <- as.POSIXct(df[,col_to_transform], tz="UTC", format=date_format)

    loc_of_years_to_be_poitentially_corrected <- which(as.numeric(format(df[,col_to_add], "%Y")) > 2025)

    
    if(length(loc_of_years_to_be_poitentially_corrected) > 0){
        years_2digit_in_question <- unlist(lapply(strsplit(unlist(lapply(strsplit(df[,col_to_transform][loc_of_years_to_be_poitentially_corrected], " "), "[[",1)), "[/]"), "[[", 3))
            for(i in seq_len(loc_of_years_to_be_poitentially_corrected)){
                digit_year <- as.numeric(years_2digit_in_question[i])
                if(digit_year <= 70){
                    loc_to_fix <- loc_of_years_to_be_poitentially_corrected[i]
                    df[loc_to_fix,col_to_fix] <-  df[loc_to_fix,col_to_fix] - years(100)
                }
        }
    }
    return(df)
}
