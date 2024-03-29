library(RUnit)
errMsg <- function(err) print(err)
load('ex1-tests.rda')

# Suppose that you are given some dataset where all variables are
# numeric. Further, assume that you consider a given variable for some
# observation to be an outlier if it is more than 1.5 IQRs from that variable's
# median value. Implement the function "outlierCutoff" that determines the
# min and max value that is not considered an outlier for each variable. your
# function should take the following arguments:
#
# <data>: a data frame consisting of only numeric variables
#
# Your function should return the following:
#
# <outlier.cutoffs>: a 2xnumber.variables matrix giving the lower and upper
# bound for non-outlier values. The first row should be the lower bound and the
# second the upper bound

outlierCutoff <- function(data) {
  
    i = 0
    n = ncol(data)
    lowerBound = rep(NA, n)
    upperBound = rep(NA, n)
    names = colnames(data)
    
    for (i in 1:n) {
      
      coldata = data[,i]
      median.col = median(coldata)
      iqr.col = IQR(coldata) * 1.5
      minOutlier = median.col - iqr.col
      maxOutlier = median.col + iqr.col
      lowerBound[i] = minOutlier
      upperBound[i] = maxOutlier
      i = i + 1
      
    }
    
    outlier.cutoffs = matrix(c(lowerBound, upperBound), nrow = 2, ncol = n, byrow = TRUE,)
    colnames(outlier.cutoffs) = colnames(data)
    return(outlier.cutoffs)
    
}
    

tryCatch(checkIdentical(outlier.cutoff.t, outlierCutoff(ex1.test)),
         error=function(err) errMsg(err))
      

# Again, suppose that you are given some dataset where all variables are numeric
# Further, assume that you are interested in removing outliers as defined in the
# previous part
# Implement a function "removeOutliers" that
# 1) caclulates the number of variables for each observation in the dataset that
# are considered outliers
# 2) removes any observation with more than some specified fraction of its
# variables as outliers. Your function should take the following arguments:
#
# <data>: a data frame where each variable is numeric
# <max.outlier.rate>: a numeric between 0 and 1 specifying the maximum allowable
# fraction of outliers (#outlier.variables / #variables)
#
# Your function should return the follwing:
#

# <subset.data>: a data frame with numeric variables where observations with
# unacceptably high rates of outliers (i.e. greater than <max.outlier.rates>) have
# been removed.

removeOutliers <- function(data, max.outlier.rate) {

    stopifnot(max.outlier.rate>=0 & max.outlier.rate<=1)
    
    cutoffs = outlierCutoff(data)
    n = ncol(cutoffs)
    #n = ncol(data)
    #r = nrow(data)
    TFmatrix = (data == TRUE)
#     
#     for (i in 1:r) {
#       numMin = data[i,] < cutoffs[1,]
#       numMax = data[i,] > cutoffs[2,]
#       numOutliers = length(which(numMin == TRUE)) + length(which(numMax == TRUE))
#       outlierrate = numOutliers / r
#       if (outlierrate > max.outlier.rate) {
#         data = data[-i,]
#       }
#       i = i + 1
#     }

    outVariables = ncol(data) * max.outlier.rate
    
    #for (n in 1:n) {
#     lower = as.numeric(cutoffs[1,])
#     higher = as.numeric(cutoffs[2,])
#     TFmatrix = apply(data, 2, function(data) (data < lower | data > higher))


  for (n in 1:n) {
    lower = which(data[,n] < cutoffs[1,n])
    higher = which(data[,n] > cutoffs[2,n])
    
    TFmatrix[lower,n] = TRUE
    TFmatrix[higher,n] = TRUE
  }
    
     rowstoremove = which(rowSums(TFmatrix == TRUE) > outVariables)
     subset.data = data[-rowstoremove,]

    return(subset.data)
}

tryCatch(checkIdentical(remove.outlier.t, removeOutliers(ex1.test, 0.25)),
         error=function(err) errMsg(err))
