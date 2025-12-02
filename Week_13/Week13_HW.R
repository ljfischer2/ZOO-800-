# HW13    Lucas Fischer
###### Importing Data #######

dgndf <- read.csv("Week_13/dragon_data.csv")
dgn <- data.frame(x = dgndf$size,
                 y = dgndf$acres_on_fire) #make it x and y for ease

ggplot(dgn, aes(x = x, y = y)) +
  geom_point() # just to visualize

## Objective 1 #########
#convert to matrices
x <- as.matrix(cbind(1, dgn$x))   
y <- as.matrix(cbind(dgn$y))

# Matrix math
coes <- solve(t(x)%*% x) %*% (t(x)%*% y)

## Objective 2 #########

slope <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)
int <- c(-1, -1.1, -1.2, -1.3, -1.4, -1.5)

par_est <- matrix(nrow =6, ncol = 6)

#These will be necessary for objective 3
par_mu <- matrix(nrow =6, ncol = 6)
par_sigma <- matrix(nrow =6, ncol = 6)

# grid search
for (i in 1:length(int)) {
  for (o in 1:length(int)) {
      yhat <- slope[i]*dgn$x + int[o]
      SSE <- sum(dgn$y - yhat)^2
      par_mu[o,i] <- mean((dgn$y - yhat)^2)
      par_sigma[o,i] <- var((dgn$y - yhat)^2)
      par_est[o,i] <- SSE
  }
}

par_est #our SSE was the lowest when i =4 and o = 1
# this correlates to the 4th value of slope, 1st value of the intercept
# slope = 1.3, intercept = -1

#this is contrasting with our data from objective 1.  Probably the way it was
# entered into the matrix?

# Objective function (minimize RSS for a linear model)
min_RSS <- function(par, data) {
  with(data, sum((par[1] + par[2] * x - y)^2))
}

# Initial parameter guess
initial_params <- c(0, 1)

#test function
min_RSS(initial_params, dgn)

# Run optim
result <- optim(par = initial_params,
                fn = min_RSS,
                data = dgn)

#testing with other values of starting params
params2 <- c(-20, 20)
result2 <- optim(par = params2,
                fn = min_RSS,
                data = dgn) #these parameters converge on a different intercept

params3 <- c(20,-20)
result3 <- optim(par = params3,
                fn = min_RSS,
                data = dgn) #However, this one converges the same as #1


###### Objective 3 ##############

par_est2 <- matrix(nrow =6, ncol = 6)
n <- 50

#I don't think this is how you do a grid search for MLE, but I wasn't sure
# where to fit in changing variables elsewhere.  there's no place for
# slope and intercept in the equation below, so I think I am missing a bit of 
# context for how to do this grid search in the same way.
for (i in 1:6) {
  for (o in 1:6) {
    lnL <- (-n/2)*log(2*pi*(par_sigma[o,i]^2)) - (1/(2*(par_sigma[o,i]^2)))*sum(dgn$x - par_mu[o,i])^2
    par_est2[o,i] <- lnL
  }
}
    #we ended up with the same matrix location as in part 2, so slope = 1.4, int = -1

#add variance of residuals from obj. 2 to objective 3

MLE <- function(par, data) {
  with(data, (-par[1]/2)*log(2*pi*(par[3]^2)) - (1/(2*(par[3]^2)))*sum(data - par[2])^2)
}
pars <- c(50, par_mu[1,4], par_sigma[1,4])

result <- optim(par = pars,
                fn = MLE,
                data = dgn) #yeah this shit aint right.

pars2 <- c(50, 0, 1)
result2 <- optim(par = pars2,
                fn = MLE,
                data = dgn)

pars3 <- c(50, 1, 2)
result3 <- optim(par = pars3,
                fn = MLE,
                data = dgn)
#I am missing something about the last part of this objective, maybe I am not
# supposed to use this equation, or I am solving for the wrong output?  Either
# way, the grid search yielded the same value as the least squares.


###### Objective 4 ###########
#Our first two methods yielded similar estimates for slope and intercept,
# However using log likelihood we did not come to the same estimates because 
# I believe I am misusing the optim function in the final step.  I am sure it 
# is something simple that would be better if it was just answered, rather than
# me beating my head against a wall for it.

