library(RUnit)
errMsg <- function(err) print(err)
load('ex2-tests.rda')

# Implement the function "colSorter". Your function should take the
# following arguemnts:
#
# <data.matrix>: a numeric matrix with 2 columns and any number of rows
#
# Your function should return the following:
#
# <sorted.matrix>: <data.matrix> sorted in increasing order by the values
#   in the first column. Any ties should be broken by values in the second
#   column. The row pairs should be maintained in this matrix

colSorter <- function(data.matrix) {

    sorted.matrix = data.matrix[order(data.matrix[,1],data.matrix[,2]),]  
  
    return(sorted.matrix)
    
}

tryCatch(checkEquals(col.sorter.t, colSorter(ex2.test1)),
         error=function(err) errMsg(err))


# Implement the function "rowSorter". Your function should take the
# following arguments:
#
# <data.matrix>: a numeric matrix of any dimensions
#
# Your function should return the following arguments
#
# <sorted.matrix>: a matrix for which each of the rows of <data.matrix> has
#   been sorted in decreasing order (the columns will no longer
#   match). This matrix should be the same dimensions as <data.matrix> (you
#   might have to make adjustments).

rowSorter <- function(data.matrix) {

    i = nrow(data.matrix)
    for (i in 1:i) {
      newrow = sort(data.matrix[i,], decreasing = TRUE)
      data.matrix[i,] = newrow
    }
    
    sorted.matrix = data.matrix
    return(sorted.matrix)
}

tryCatch(checkEquals(row.sorter.t, rowSorter(ex2.test2)),
         error=function(err) errMsg(err))


# Implement the function "factorSorter". Your function should take the
# following arguments:
#
# <data>: a data frame where one of the variables gives a factor level for
#   each observation. The remaining observations are numeric.
# <sort.name>: a character string giveing the name of the variable to
#   sort by
#
# Your function should return the following:
#
# <sorted.factors>: an object of **by class**. The elements of this class
#   should be data frames that are subsets of <data> whose rows have been
#   sorted in increasing order according to the variable given by
#   <sort.name>

factorSorter <- function(data, sort.name) {

    colvals = data[[sort.name]]
    
    names = colnames(data)
    factor = sapply(names, function(var) {class(data[[var]]) == "factor"})
    factorname = names(which(factor == TRUE))
    factorcol = unname(which(factor == TRUE))
    varcol = which(names == sort.name)
    factor.variable = data[[factorname]]
    levelnames = levels(factor.variable)
    #data = data[order(colvals),]
    #with(data, data[order(Species, Sepal.Length),])
    
    data.sort = function(facname) {
        newdat = data[order(data[,sort.name],data[which(data[[factorname]] == facname),factorcol]),]
    }
    
    
    sorted.factors = by(data, factor.variable, function(x) data[order(data[,varcol],data[,factorcol]),])
    #sorted.factors = by(data, factor.variable, function(x) data.sort(x))
    return (sorted.factors)
}

tryCatch(checkEquals(factor.sorter.t, factorSorter(iris, 'Sepal.Length')),
         error=function(err) errMsg(err))
