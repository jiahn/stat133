# Complete the implementation of the function "readRemove". This function should
# take the following arguments:
#
# <file.name>: a string giving the name of the file from which the data should
# be read
# <max.na>: the maximum number of variables that may be NA for a given
# observation (default=0)
# <sep>: the field separator character (eg. comma, white space, tab)
# <header>: a logical indicating whether or not first line of the file contains
# variable names (default=',')
#
# You function should return the following:
# <file.data>: a data frame containing the observation in the file given by
# the variable <file.name>. Any observations that have more NA variables than
# <max.na> should be removed from this data frame.

load('ex1_tests.Rda')

readRemove <- function(file.name, max.na=0, sep=',', header=F) {

    data = read.table(file.name, sep = sep, header = header)
    sumRowNA = apply(data, 1, function(x) { sum(is.na(x)) } )
    rowsToRemove = which(sumRowNA > max.na)
    length = length(rowsToRemove)
    if (length == 0) {
      data = data
    }
    else
    data = data[-rowsToRemove,]
    
}

library(RUnit)
errMsg <- function(err) print(paste("ERROR:", err))
tryCatch(checkEquals(sum(is.na(readRemove('test.data', max.na=1, sep=';'))), sum(is.na(test.1))),
         error=function(err) errMsg(err))
tryCatch(checkIdentical(readRemove('babies.csv', sep=',', header=T), test.2),
         error=function(err) errMsg(err))




