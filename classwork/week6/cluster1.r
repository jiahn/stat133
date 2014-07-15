library(MASS)

healthy.mean = rnorm(30, mean=10, sd=5)
sigma = diag(30)

sick.mean = healthy.mean
important.variables = c(3, 9, 23)
mean.diff = c(20, -10, 3)
sick.mean[important.variables] = sick.mean[important.variables] - mean.diff



healthy.subjects = mvrnorm(100, healthy.mean, sigma)
sick.subjects = mvrnorm(100, sick.mean, sigma)

group = rbind(healthy.subjects, sick.subjects)

## Exercise
#
# 1. Perform a principal components analysis on group.

pcaResult = princomp(group)

# 2. Write a helper function called plotPCA that takes
# a data matrix with 2 columns and plots the first column
# against the second.  You should assume that the columns are
# from the PCA (you may wish to review gene-eda4.r). Color
# the "healthy" subjects red and the "sick" subjects blue. 
# Your helper function should also label the plot
# "Simulated  MVG (n= 200, p=30)" and put a legend in the
# top right corner.

plotPCA = function(data) {
  
  #pcaResult = prcomp(data)
  plot(data, col = c('black', 'red'), main = "Simulated  MVG (n= 200, p=30)")
  legend("topright", "Black = Sick, Red = Healthy")
  
}

# 3. Produce four figures in one window by calling your
# helper function on the:
#   a. 1st vs 2nd PC
plotPCA(pcaResult$x[,1:2])

#   b. 2nd vs 3rd PC 
plotPCA(pcaResult$x[,2:3])

#   c. 1st vs 3rd PC 
plotPCA(pcaResult$x[,1:3])

#   d. 3rd vs 4th PC 
plotPCA(pcaResult$x[,3:4])
