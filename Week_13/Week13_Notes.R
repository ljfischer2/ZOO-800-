# Maximum Likelihood and MLE

# Three ways to fit a linear regression:
#Anylitical v. Numerical estimation of parameters
##Anylitical:
###an equation:
### only possible for simple problems
### often faster than numerical
### nice to reframe model
## Numerical:
### try lots of different solutions, and see "how well they work"
### objective function (Minimize SSE, maximize likelihood, minimize "cost, "distance")

# yi = sum(from j=1 to n) of Xij *Bj

#y = XB

#Bhat = (X^T * X)^-1 * X^T*y

# Matricies: is it a number (scalar), a vector, or a matrix?

#b <- solve(t(X)%*%X %*% (t(X) %*% y))    #%*% is a matrix multiplication
# when we multiply x by the B vector, it produces a vector


#Least squares solution:
#minimize sum of square errors

#Sum(i=1 to m) of (yi - yhati)^2

#Find values of B that returns SSE

#Brute force it:
##loop ranve of plausible values of intercept and slope)
##save SSE for each pair in a matrix
##find cell with lowest Negative Log likelihood, extract corresponding value
##repeat to refine.

# OR

#optim()
  #takes objective function, data, params as arguments
  #tries different values of each param and finds those that minimize obj. function
  #many algorithms run simultaneously
  #returns estimates of params, and their uncertainties

#sample data (as df)
#objective function (minimisze RSS for linear model)
#min_RSS <- function(par, data) {
  #with (data, sum((par[1] + par[2] *x-y)^2)
#}

#initial param guess needed:
#init_param <- c(0,1)

#run optim
#result <- optim(par = init_param, fn = min_RSS, data = dat)

# Maximum likelihood estimation is the same as minimizing log likelihood
#finde int. and slope that maximize likelihood of data (minimize neg. log. likelihood)

#normal: lnL = (-n/2)ln(2pitheta^@)... and a bunch of other stuff

#is it the true mountain, or one in a range?
#did you find a peak, or a ridgeline
