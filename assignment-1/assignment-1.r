# --------------------------------------------------------------
# Assignment 1
# --------------------------------------------------------------

# Complete the function bodies as indicated by the comments under the function
# definition. Note that in our comments, we indicate variables by
# "<variable.name>". You should try to pass all the provided tests to ensure
# that your implementation conforms to the requirements as much as possible.
# Before pushing to github, make sure that your script is a **syntactically
# valid** R script. To run the tests, type in the terminal "Rscript
# assignment-1.r" and press enter. Rscript is a version of the R interpreter
# that allows you to execute the R code contained in a given file.

# The tests for this lab use the R suite for unit tests RUnit, which we import
# using the R way to load packages:

library(RUnit)
errmsg = function(err) print(paste("ERROR:  ",err))

# The test will be indicated after the function body you'll need
# to implement.


# The data for this lab come from Phil Spector's class webstie. We are loading
# it in for you and storing it as the variable <temperature.data>.  The other
# data used for this lab are included in the file "assignment-1-3.Rda", which we
# are loading here.

temperature.data <-
  read.table('http://www.stat.berkeley.edu/classes/s133/data/january.tab',
             header=T)

load('assignment-1-3.Rda')

# Good luck!

# --------------------------------------------------------------
# Problem 1 - a
# --------------------------------------------------------------  
toCelcius <- function(temp.far) {
    # This function should take a vector of Farenheit temperature values
    # <temp.far> and return the values of each entry in Celcius

    #your code here
	celsius = (temp.far - 32) * (5/9)
	return(celsius)
}


tryCatch(
     checkEquals(toCelcius(c(32, 100, 210)), c(0, 340/9, 890/9)),
     error = function(err) errmsg(err)
)



# --------------------------------------------------------------
# Problem 1 - b
# --------------------------------------------------------------
calculateS <- function(data, selected.year, selected.day) {
    # Calculate S, as defined in question 1b of the problem set, on
    # <selected.day> of <selected.year>

    # These lines make sure that the dataset you call this function on uses
    # the same variables as <temperature.data>
    
    if (!all.equal(names(data), names(temperature.data)))
        stop('unrecognized dataset supplied')

    # These lines make sure your supplied dates are in the correct range
    if ((selected.year < 2005 | selected.year > 2011) | (selected.day < 1 |
                                                         selected.day > 31))
        stop('invalid date')
  subset.year = data[data$year == selected.year,]
  subset.daymax = subset.year$max[subset.year$day == selected.day]
  subset.daymin = subset.year$min[subset.year$day == selected.day]
  subset.dayavg = subset.year$mean[subset.year$day == selected.day]
  result = (subset.daymax - subset.daymin)/subset.dayavg
	return(result)
}


tryCatch(
    checkEquals(calculateS(temperature.data, 2009, 28), 0.4807155,
                tolerance=.Machine$double.eps^0.3), error = function(err) errmsg(err)
)

tryCatch(
    checkEquals(calculateS(temperature.data, 2007, 1), 0.4095435,
                tolerance=.Machine$double.eps^0.4), error = function(err) errmsg(err)
)



# --------------------------------------------------------------
# Problem 2 - a
#--------------------------------------------------------------
#Subset the data to include only observations from the year 2010. Use this
#subset to examine the spread of temperatures for these days.  Find the maximum
#spread and the day on which it occured.  Store these as the variables:
#<subset.2010>, <temp.differences>, <max.difference>, and <max.difference.day>.

subset.2010 = temperature.data[temperature.data$year == 2010,]
temp.differences = subset.2010$max - subset.2010$min
max.differences = max(temp.differences)
max.differences.day = subset.2010$day[temp.differences == max.differences]


# --------------------------------------------------------------
# Problem 2 - b
# --------------------------------------------------------------  
# Find average min temperature for the following subsets: (1)days with daily
# high temperatures greater than the 65th percentile (2)days with daily high
# temperatures below the 65th percentile. Use strict inequalities when
# determining these subsets
    
greater = temperature.data$max
cutoff = quantile(greater,c(0.65))
greaterdays = temperature.data$day[temperature.data$max > cutoff]
lowerdays = temperature.data$day[temperature.data$max < cutoff]
mean.low.above = mean(temperature.data$min[greaterdays])
mean.low.below = mean(temperature.data$min[lowerdays])

# --------------------------------------------------------------
# Problem 3
# -------------------------------------------------------------- 
# Use the variabiables from "assignment-1-3.Rda" (<observed.animals> and
# <animal.key>) to create two new vectors. Both should contain one entry for
# each of the entries in <observed.animals>. The entries of the first should
# correspond to the diet of observed.animals (as given by <animal.key>) while
# the entries of the second should correspond to the type of <observed.animals>
# (as given by <animal.key>).  Store these vectors as <observed.diets> and
# <observed.types> respectively.

#your code here
typenum = match(observed.animals, animal.key$animal)
observed.diets = animal.key$diet[typenum]
observed.types = animal.key$type[typenum]
# Use your newly created vectors to calculate the total number of observed
# animals that are both carnivores and mammals.  Store this variable as
# <carnivore.mammals>

mammals = (observed.types == "mammal")
carnivores = (observed.types == "carnivore")
n.carnivore.mammals = sum((carnivores == mammals) == TRUE)

    
