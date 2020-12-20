
corr <- function(directory = "specdata", threshold = 0) {
        # get complete nobs
        aggr <- complete("specdata")

        # get 'id' of items which pass threshold
        passed_threshold <- aggr[aggr$nobs > threshold, ]

        if (nrow(passed_threshold) == 0) {
                return(NULL)
        }

        id <- passed_threshold$id

        # determine file names based on 'id'
        file_paths <- create_filename_based_on_id(directory, id)

        # import data
        df <- data.table::rbindlist(lapply(file_paths, read.csv))
        # filter on complete cases
        df <- df[complete.cases(df)]

        # calc cor
        dfcor <- df[, stats::cor(x = sulfate, y = nitrate),
                    by = .(ID)]
        # explicit return
        return(dfcor$V1)
}
