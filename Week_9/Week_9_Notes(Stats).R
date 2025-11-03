#Central Limit Theorem
##Distribution of sample means converges to a normal distribution as 
## Sample size gets larger

# accum. (sum) of many small random errors drawn from any distribution is norm. dis.

#Other types of non-normal data:
##counts(quadrats, line transects, etc.)
##Proportions(# of ind. w/ a characteristic / total # of ind. examined)
##Categorical(Discrete)

#Counts of events w/in a given sampling unit(distance, area, vol., time)
##are Poisson distributed if the rate is constant, and independent
#Discrete probability distribution
##integer values from 0 - inf.
##single parameter: mean = var = lamda
##Clumped fata gives rise to overdispersion: var > mean
##Negative binomial can be a better option (two parameters - lamda varies in time/space)

#Binomial
##total # of successes in a set of n indendent trials - observations are binary
##Discrete probability dist.
###values from 0 to n
###Parameters n and p
###Variance changes with mean: mean = n*p, var. = n*p(1-p)

#beta distribution
##dist p can ve defined by the results of n independent trials
##Continuous
###values 0 to 1
### Param alpha and beta
### Dist. of p is beta with alpha-1 'successes' and beta-1 'failures
#

#Exponentials:
##Normal, poisson, binomial, gamma, inverse gaussian
##expressed by two parameters
### location
### scale

#IN R:
#rnorm, dnorm, pnorm, (qnorm)
##rnorm - random vars from specified dist.
##dnorm - probability density at any point
##pnorm- tail probs to left or right of a point
## set.seed() - sets a starting point for every time

#Use random num gen to select random subset of obs
##create new col
#fill w/ random num
#sort by first column
#select first x rows

#Fitting distributions and dist. tests
##MASS and fitdistrplus packages
##denscomp() plot function

install.packages('MASS')
install.packages('fitdistrplus')

library(fitdistrplus)
#always same values
set.seed(12)

#find our non-normal population
pop <- rchisq(500,10)
mean(pop) #9.798
sd(pop) #4.371

#taking random samples from pop
n = 100 #do this 100 times
df = data.frame(pop = pop,
                x = seq(1:500))
y = matrix(nrow = 10, ncol = 100)
for (i in 1:n) {
  df$x <- rnorm(500, 50, 10)
  df <- df %>%
    arrange(x)
    y[,i] <- head(pop)
  
}


df$x <- rnorm(500, 50, 10)
df <- df %>%
  arrange(x)
y[i] <- head(pop)
