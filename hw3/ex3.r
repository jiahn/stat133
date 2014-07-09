library(RUnit)
errMsg <- function(err) print(err)
load('ex3-tests.rda')

#done

sumNA <- function(data.matrix) {
    
    i = nrow(data.matrix)
    j = ncol(data.matrix)
    rows = c()
    cols = c()
    
    for (i in 1:i) {
      rowsum = sum(is.na(data.matrix[i,]))
      rows = c(rows, rowsum)
    }
    
    for (j in 1:j) {
      rowsum = sum(is.na(data.matrix[,j]))
      cols = c(cols, rowsum)
    }
    
    numNAs = list(rows, cols, sum(is.na(data.matrix)))
    return(numNAs)
}

tryCatch(checkEquals(sum.na.t, sumNA(ex3.test1)), error=function(err)
         errMsg(err)) 

    
# Implement the function "simulateNormals". Your function should take the
# following arguments:
#
# <n>: a numeric constant giving the number of normal variables drawn in
#   each simulation (constant across simulations)
# <sim.mean>: a numeric constant giving the mean of the normal variables
#   (constant across simulations)
# <sim.var>: a numeric constant giving the variance of the normal variables
#   (constant across simulations)
# <k> the total number of simulations
#
# Your function should return the following:
#
# <simulations>: a <n> x <k> matrix of simulated normal variables

simulateNormals <- function(n, sim.mean=0, sim.var=1, k=10) {

    simulations = matrix(nrow = n, ncol = k)
    sd = sqrt(sim.var)
    
    for (k in 1:k) {
      simulations[,k] = rnorm(n, sim.mean, sd)
    }

    return (simulations)
}

set.seed(47)
tryCatch(checkEquals(simulate.normals.t, simulateNormals(100, 5, 4, 5)),
         error=function(err) errMsg(err))


# Implement the function "listLengths". Your function should take the
# follwoing arguments:
#
# <data.list>: a list whose elements are vectors of varying length
#
# Your function should return the following:
#
# <element.lengths>: a numeric vector whose entries are the lengths of each
#   element of <data.list>

listLengths <- function(data.list) {

    # your code here *
    element.lengths = lapply(data.list, function(x) length(x))
    element.lengths = unlist(element.lengths)
    return(element.lengths)
}

tryCatch(checkEquals(list.lengths.t, listLengths(ex3.test2)),
         error=function(err) errMsg(err))


# Implement the function "matrixListMeans". Your function should take the
# following arguments:
#
# <matrix.list>: a list of square matrices all with the same dimensions
#
# Your function should return:
#
# <matrix.row.means>: a nxk matrix (where n is the dimension of the
#   matrices and k is the length of the list). The jth column of this
#   matrix should correspond to the row means of the jth list element.

matrixListMeans <- function(matrix.list) {

    r = dim(matrix.list[[1]])
    c = length(matrix.list)
    
    means = lapply(matrix.list, function(x) rowMeans(x))
    matrix.list = matrix(unlist(means), ncol = c, byrow = FALSE)
    return(matrix.list)
    
}

tryCatch(checkEquals(matrix.list.means.t, matrixListMeans(ex3.test3)),
         error=function(err) errMsg(err))


# Implement the function "standMatrixVariables". Your function should take
# the folowing arguments:
#
# <data.matrix>: a numeric matrix whose columns correspond to variables
#
# Your function should return the following:
#
# <standardized.matrix>: an nxn matrix (where n is the number of variables
#   i.e. columns of <data.matrix). Entry (i,j) of this matrix should contain
#   the following value:
#
#      (mean(col.i) - mean(col.j)) / sd(col.i, col.j)
#
# where sd(col.i, col.j) is the standard deviation of all values from both
# column i and j.

standMatrixVariables <- function(data.matrix) {

    n = ncol(data.matrix)
    r = ncol(data.matrix)
    emptymatrix = matrix(nrow = n, ncol = n)
    
    value = function(i, j) {
      (mean(data.matrix[,i]) - mean(data.matrix[,j])) / (sd(c(data.matrix[,i], data.matrix[,j])))
    }
    
    for (n in 1:n) {
      for (r in 1:r) {
      emptymatrix[n,r] = value(r,n)
      }
    }
    
    standardized.matrix = emptymatrix
    return(standardized.matrix)
}

tryCatch(checkEquals(stand.matrix.variables.t,
                     standMatrixVariables(ex3.test4)),
         error=function(err) errMsg(err))
