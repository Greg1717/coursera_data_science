# notes

# test lexical vs dynamic scoping
y <- 10

f <- function(x) {
  y <- 2
  y ^ 2 + g(x)
}

g <- function(x) {
  x * y
}

f(3)
# 4 + (3*10)
#
# How to submit
# Copy the token below and run the submission script included in the assignment download. When prompted, use your email address gergely_horvath@icloud.com.
#
# 3yFB5i5qb6QsJJgd

library(swirl)



cube <- function(x, n) {
        x ^ 3
}

cube(3)

x <- 1:10

if (x > 5) {
        x <- 0
}

f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}

z <- 10
# 3 + 3 + 4
f(3)



x <- 5
y <- if(x < 3) {
        NA
} else {
        10
}

# Assignment ===================================================================
# Part 1 =======================================================================
#
# Write a function named 'pollutantmean' that calculates the mean of a pollutant
# (sulfate or nitrate) across a specified list of monitors.
#
# The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
#
# Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows


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

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

# Part 2 =======================================================================

# Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows


# You can see some example output from this function below. The function that you write should be able to match this output. Please save your code to a file named complete.R. To run the submit script for this part, make sure your working directory has the file complete.R in it.

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
        # explicit return
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


complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


cr <- corr("specdata")
cr <- sort(cr)
RNGversion("3.5.1")
set.seed(868)
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("specdata", 129)
cr <- sort(cr)
n <- length(cr)
RNGversion("3.5.1")
set.seed(197)
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("specdata", 2000)
n <- length(cr)
cr <- corr("specdata", 1000)
cr <- sort(cr)
print(c(n, round(cr, 4)))

# Part 3

# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold.

# The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows

# For this function you will need to use the 'cor' function in R which calculates the correlation between two vectors. Please read the help page for this function via '?cor' and make sure that you know how to use it.

# You can see some example output from this function below. The function that you write should be able to approximately match this output. Note that because of how R rounds and presents floating point numbers, the output you generate may differ slightly from the example output. Please save your code to a file named corr.R. To run the submit script for this part, make sure your working directory has the file corr.R in it.


print(R.version.string)

source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313
cr <- corr("specdata", 400)
head(cr)
## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313
cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##
length(cr)
## [1] 0
cr <- corr("specdata")
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000
length(cr)
## [1] 323





