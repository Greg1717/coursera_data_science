

pollutantmean <- function(directory = "specdata", pollutant = "nitrate", id = 1:332) {
        # determine file names based on 'id'
        file_paths <- create_filename_based_on_id(directory, id)
        # import data
        df <- data.table::rbindlist(lapply(file_paths, read.csv))
        # calc mean
        x <- mean(df[[pollutant]], na.rm = TRUE)
        # explicit return
        return(x)
}

create_filename_based_on_id <- function(directory, v) {
        v <- as.character(v)
        v[nchar(v) == 1] <- paste0("00", v[nchar(v) == 1])
        v[nchar(v) == 2] <- paste0("0", v[nchar(v) == 2])
        v <- as.list(paste0(directory, "/", v, ".csv"))
        # return list of file names
        return(v)
}
