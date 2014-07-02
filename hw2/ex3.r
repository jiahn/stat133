library(RUnit)
errMsg <- function(err) print(err)
load('ex3-tests.rda')

# Suppose you are given a data frame where all but one of the variables are
# numeric. The final variable (though not necessarily final in position) is a
# factor associated with different levels of your observations. Implement the
# function "meanByLevel" that returns the mean value for each of the numeric
# variables by the levels given from the factor variable. Your function should
# take the following arguments:
#
# <data>: a data frame where all but one of the variables are numeric. The final
#   variable is a factor giving the different levels of the observations. **The
#   factor variable is not necessarily the final variable in position.**
#
# Your function should return:
#
# <level.means>: the means of each of the variables broken down by each of the
#   levels (this should be a num.factors x num.numeric.variables matrix).

meanByLevel <- function(data) {

    names = colnames(data)
    numCol = ncol(data)
    numVars = ncol(data) - 1
    meandata = 1:j
    factornames = names(table(data[numCol]))
    i = length(factornames)
    level.means = vector("list", i)
    
    for (i in 1:i) {
      rows = which(data[lastCol] == factornames[i])
      newdata = data[rows,-numCol]
      level.means[[i]] = colMeans(newdata)
    }
    
    level.means = do.call(rbind, level.means)
    rownames(level.means) = factornames
    return(level.means)
}  


tryCatch(checkIdentical(mean.by.level.t, meanByLevel(iris)), error=function(err)
         errMsg(err))

# Suppose you are given a data frame with the same structure as in the previous
# part of the question. You are interested in identifying the difference between
# the overall average for a given variable and the factor level average for that
# variable. You want this difference to be standardized by the overall standard
# deviation for that variable. Implement the function "stdLevelDiff" that does
# this for each of the numeric variables in your data frame. Your function
# should take the following arguments:
#
# <data>: a data frame where all but one of the variables are numeric. The final
#   variable is a factor giving the different levels of the observations. **The
#   factor variable is not necessarily the final variable in position**
#
# Your function should return: 
#
# <level.diff> the difference between mean by factor level and overal
#   mean for each variable divided by the overall standard deviation for each
#   variable. This should be a num.factors x num.numeric.variables matrix.
#   NOTE: you may need to use R's transpose function to make sure that the
#   dimensions of your return value are correct.

stdLevelDiff <- function(data) {

    meanMatrix = meanByLevel(data)
    cols = colnames(meanMatrix)
    lengthCol = length(cols)
    
    factornames = names(table(data[lengthCol + 1]))
    j = length(factornames)
    
    sepdata = c(data[cols[1:lengthCol]])
    overallMeans = sapply(sepdata, mean)
    overallSD = sapply(sepdata, sd)
    
    indivMeans = vector("list", j)
    
    for (j in 1:j) {
      n = which(data[lengthCol + 1] == factornames[j])
      sepdata = c(data[n,cols[1:lengthCol]])
      factorMeans = sapply(sepdata,mean)
      indivMeans[[j]] = factorMeans
    }
    
    for (j in 1:j) {
      varName = factornames[j]
      DFC = (as.numeric(indivMeans[[j]]) - overallMeans) / (overallSD)
      if (j == 1)
      level.diff = rbind(DFC)
      else
      level.diff = rbind(level.diff, DFC)
    }
    
    rownames(level.diff) = factornames
    level.diff = abs(level.diff)
    return(level.diff)
}

tryCatch(checkIdentical(std.level.diff.t, abs(stdLevelDiff(iris))),
         error=function(err) errMsg(err))
