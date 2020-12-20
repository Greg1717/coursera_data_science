complete <- function(directory, id = 1:332) {
        # determine file names based on 'id'
        file_paths <- create_filename_based_on_id(directory, id)
        # import data
        df <- data.table::rbindlist(lapply(file_paths, read.csv))
        # keep only complete cases
        df <- df[complete.cases(df)]
        # count complete cases
        aggr <- df[, .N, by = .(ID)]
        # rename cols
        aggr <- aggr[, .(id = ID, nobs = N)]
        return(aggr)

}

create_filename_based_on_id <- function(directory, v) {
        v <- as.character(v)
        v[nchar(v) == 1] <- paste0("00", v[nchar(v) == 1])
        v[nchar(v) == 2] <- paste0("0", v[nchar(v) == 2])
        v <- as.list(paste0(directory, "/", v, ".csv"))
        # return list of file names
        return(v)
}
