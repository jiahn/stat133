load('ex2.rda')

# We have loaded the data frame "grades" into your workspace. It contains the
# following variables:
#
# > names(grades)
# [1] "hw"    "labs"  "final"
#
# The three variables represent 100 student's scores for homework, labs, and final.

# (2 points)
# Please fit two linear models. One that predicts a student's final grade from
# their homework grade and another that predicts a student's lab grade from
# their homework grade. Store these as the variables <fit.final> and <fit.labs>
# respectively

fit.final = lm(grades$final ~ grades$hw)
fit.labs = lm(grades$labs ~ grades$hw)


# (2 points)
# Please use the diagnostic methods discussed in class to determine which of the
# models satisfies the assumption of constant variance. Store this as the
# variable <contant.var.model>. Your answer should be one of the following
# strings:
#
# "final" or "labs"

contant.var.model = final


# (3 points)
# What is the slope of the regression line in your <fit.final> model? Store this
# as the variable <final.slope>. What is the intercept of the of the regression
# line in the <fit.labs> model? Store this as the variable
# <labs.intercept>. What is the r-squared value for the <fit.final> model (this
# will need to be accurate up to 5 decimal places)? Store
# this as the variable <final.r.sq>

final.slope = unname(fit.final$coefficients[2])
labs.intercept = unname(fit.labs$coefficients[1])
final.r.sq = unname(unlist(summary(fit.final)["r.squared"]))

# (2 points)
# Consider a model that predicts an individual's final score using the following
# formula: y.hat = beta*hw, where
#
# y.hat = individual's predicted final score
# beta = 2*<final.slope>
# hw = individual's actual hw score
#
# Please compute the squared residuals for this model (this should be a length
# 300 numeric vector). Store this as the variable <sq.residuals>.

sq.residuals = as.numeric(fit.final$residuals)^2


# (3 points)
# Using your <fit.final> model, generate a 90 percent prediction interval for
# each of the fitted values (this should be a 100 x 2 matrix). Store this as the
# variable <final.pi>. What fraction of the final scores fall within their
# respective prediction interval?  Store this as the variable <prop.within>.

newdata = data.frame(fit.final$fitted.values)
final.pi = predict(fit.final, newdata, interval="predict", level=0.90)
final.pi = as.matrix(final.pi)

i = nrow(final.pi)
vec = c()

for( i in 1:i) {
  modelval = fit.final$model[i,1]
  TF1 = (modelval > final.pi[i,2])
  TF2 = (modelval < final.pi[i,3])
  
  if (TF1 == TRUE & TF2 == TRUE) {
    vec = c(vec, modelval)
  }

}

prop.within = length(vec) / i
