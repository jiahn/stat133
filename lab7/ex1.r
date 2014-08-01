library(RUnit)
errMsg <- function(err) print(err)
load('lab7-tests.rda')

# Implement the function dataGenerator. Your function should take the following
# arguments:
#
# <X>: an n x p matrix of predictor variables (or vector if p=1)
# <betas>: the "true" coefficients of the regression equation
# <var>: the variance of the error term epsilon in the regression equation
#   (sigma^2)
#
# Your function should return (Y) a length <n> vector of responses: Y=X<beta> +
# epsilon. If length(<betas>) is not equal to ncol(<X>), your function should
# throw the error: "length betas does not match p"

dataGenerator <- function(X, betas, var) {

    X = as.matrix(X)
    if ((length(betas) != ncol(X)) == TRUE) {
      stop("length betas does not match p")
    }
    else 
      
    Y = c()
    i = nrow(X)
    epsilon = rnorm(i, mean = 0, sd = sqrt(var))
    
    
    for (i in 1:i) {
      val = (X[i] * betas) + epsilon[i]
      Y = rbind(Y, val)
    }
    
    Y = as.numeric(Y)
    Y = as.matrix(Y)
    return(Y)
    
}

set.seed(47)
tryCatch(checkEquals(lab7$dataGenerator.t, dataGenerator(lab7$predictors, 4, 4)),
         error=function(err) errMsg(err))


# Implement the function betaEstimator. Your function should take the following
# arguments:
#
# <X>: an n x p matrix of predictor variables (or vector if p=1)
# <betas>: the "true" coefficients of the regression equation
# <var>: the variance of the error term epsilon in the regression equation
#
# Your function should generate response values using your dataGenerator
# function. Using this data, your function should run a linear regression and
# return an estimate of the values of <betas>.

betaEstimator <- function(X, betas, var) {

  set.seed(47)
  values = dataGenerator(X, betas, var)
  XYE = lsfit(X, y = values, wt = NULL, intercept = TRUE)
  return(XYE$coefficient[2])

}

set.seed(47)
tryCatch(checkEquals(lab7$betaEstimator.t, betaEstimator(lab7$predictors, 4, 4)),
         error=function(err) errMsg(err))


# It turns out that under certain assumptions, our estimates of beta follow a
# normal distribution with mean beta and variance (X^T X) ^ -1*sigma^2
# *sigma^2. Implement the function betaVariance. Your function should take the
# following arguments:
#
# <X>: an n x p matrix of predictor variables (or vector if p=1)
# <var>: the variance of the error term epsilon in the regression equation
#
# Your function should calculate and return a vector giving the variance of beta
# hat.

betaVariance <- function(X, var) {
  
  betas = rnorm(1, mean = 4, sd = sqrt(var))
  return(betaEstimator(X, betas, var))

}

tryCatch(checkEquals(lab7$betaVariance.t, betaVariance(lab7$predictors, 4)),
         error=function(err) errMsg(err))


# Generate a 100 x 1 vector of predictor variables X by sampling from a normal
# distribution with mean 0 and variance 1. Using these predictors, run 1000
# simulations where you generate response variables Y according to the
# regression equation with beta=3 and sigma^2=2. For each of your simulations,
# estimate beta hat and store this value as beta.hats. Calculate the proportion
# of beta.hats that are within 2 sd of beta over your 1000 simulations. Store
# this value as prop.2sd.
# ***make sure to set your seed to 47 before running your simulations***

predictors = rnorm(100, mean = 0 , sd = sqrt(1))
p.beta = 3
p.var = 2
p.simulate = simulate(dataGenerator(predictors, p.beta, p.var), nsim = 1000, seed = 47)
# beta.hats <- your code here
# prop.2sd <- your code here

