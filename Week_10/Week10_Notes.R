

######################## CLass Notes ######################

# Galton's peas were based on his research that extreme parents had more
# mediocre offspring.  "regressiion to the mean."  First regression in 1877.

#Pearson's "product-moment" correlation for normal distribution
#strength of two variables can be defined by product momont - product of each
# observations deviations from its mean:
#NOT the same as slope
#r^2 is the familiar "precent of variation explained" by the regression


#Spearman rank correlation for non-normal data
#Pearson correlation of observation ranks (highest = 1, next = 2, etc.)
#cor.test(), w/ method = "spearman" argument
#When relationship is linear, data is normal, Pearson is more powerful.

#Formal regression model: Y = B0 + B1X + E
#B0, B1 are parameters
# X is the predictor, or independent variable
#Deviations E are independent, ~N(0,std.dev)

#Linear Regression: analytical solution
# yi = sum(from j=1 to n) Xij * Bj
# i represents one of m observations and j one of n+1 covariates (+1 for the
# intercept)
#Y = XB
#Y = matrix from y1 to ym cols,
#X = matrix from 1 to Xncols, 1 to Xm rows
#B = matrix from B1 to Bn

#Linearity - The relationshp between the response and predictor is linear
#Errors are independent & identically distributed ~N(o, stddev)
##Normally distributed
##Independent(no autocorrelation)
##Homoscedastic(variance is the same)

#Syntax example: car_mod2 <- lm(mtcars$mpg ~ hp, data = mtcars)

#ggfortify::autoplot allows us to view and check assumptions
########        USE THIS FOR YOUR PROJECT!!!!
# assumption checking: hist(car_mod2$residuals)

#Syntax example: predict(object, newdata, interval = "prediction")
## object is lm object
## newdata is df with X values for all predictions
## interval controls hw uncertainty in the predictions is portrayed
## prediction interval is where 95% of obs are.
## confidence band is wherwe the predicted mean should be.

######################### Exercise ##########
install.packages('ggfortify')
library(ggfortify)

#create our data frame that will serve to continually populate the plots
df <- data_frame(x = vector(length = 50L),
                 y = vector(length = 50L))

# our intercept and slope:
a <-  2.459
b <- -2.837

#and using a seed so that the values are the same every time
set.seed(16)
df$x <- rnorm(50, 5, 5)
e1 <- rnorm(50, 0, 20)
e2 <- rnorm(50, 0, 1)
e2 <- abs(e2)
#we populate our y column using the equation below, including our random term
for (i in seq(1:50)) {
  df$y[i] <- a +b*df$x[i]*e2[i] + e1[i]
}

plot1 <- ggplot(df, aes(x = x, y = y)) +
  geom_point()
plot1
mod1 <- lm(y~x, data = df)
predict(mod1, df, interval = 'prediction')

#plot(mod1$residuals)
#autoplot(mod1)



#### Model 2###
e2.2 <- abs(rnorm(50, 0, 5))
e2 <- rnorm(100, 0, 10)
for (i in seq(1:50)) {
  df$y[i] <- a +b*df$x[i]*e2.2[i] + e2[i]
}

plot2 <- ggplot(df, aes(x = x, y = y)) +
  geom_point()
plot2
mod2 <- lm(y~x, data = df)
plot(mod2$residuals)
autoplot(mod2)
