#HW Week 9 - probability and distributions

#Use the equation for a simple linear regression to generate 100 observations of
#x and y.  Consistent with the assumptions of linear regression, x should be 
#measured without error, but the observed values of y should include normally 
#distributed error with mean zero

set.seed(123) # for reproducibility

X = runif(100, min=0, max=10) # independent variable x

#set slope (beta) and intercept (alpha)
beta = 2.5
alpha = 5

#generate normally distributed errors
errors = rnorm(100, mean=0, sd=1)

#calculate dependent variable y with error
Y = alpha + beta * X + errors

#Create a scatter plot of x and y with ggplot2

library(ggplot2)
data = data.frame(X, Y)

ggplot(data, aes(x=X, y=Y)) +
  geom_point() +
  labs(title="Scatter plot of X vs Y with normally distributed errors",
       x="Independent variable X",
       y="Dependent variable Y") +
  theme_minimal()

#Repeat the simulation above, but this time generate errors with standard deviation = 10
errors_sd10 = rnorm(100, mean=0, sd=10)
Y_sd10 = alpha + beta * X + errors_sd10

#add these data to the existing data frame "data" and create a new column indicating the standard deviation
data$Y_10 = Y_sd10

#Repeat the simulation above, but this time generate errors with standard deviation = 25
errors_sd25 = rnorm(100, mean=0, sd=25)
Y_sd25 = alpha + beta * X + errors_sd25

#add these data to the existing data frame "data" and create a new column indicating the standard deviation
data$Y_25 = Y_sd25

#Reshape the data frame for ggplot2
library(tidyr)
data_long = pivot_longer(data, cols = c(Y, Y_10, Y_25), 
                         names_to = "Error_SD", values_to = "Y_value")

#Create scatter plots for each error standard deviation using facet wrap in ggplot2
ggplot(data_long, aes(x=X, y=Y_value)) +
  geom_point() +
  facet_wrap(~ Error_SD, ncol=3) +
  labs(title="Scatter plots of X vs Y with different normally distributed errors",
       x="Independent variable X",
       y="Dependent variable Y") +
  theme_minimal()

#Objective 2: how many coin flips are required to be able to consistently determine whether the coin is 
#unfair (p(heads)>0.5) for different degrees of unfairness. 

#Using simulations of coin flips (Bernoulli trials), plot the probability (number of times out of 100) 
#that you determine the coin is significantly unfair (alpha < 0.05) for 1 to 20 coin flips when p = 0.55

set.seed(456) # for reproducibility
p_unfair = 0.55
num_flips = 1:20
results_55 = numeric(length(num_flips))
for (i in num_flips) {
  significant_count = 0
  for (j in 1:100) {
    flips = rbinom(i, 1, p_unfair)
    test = binom.test(sum(flips), i, p=0.5, alternative="greater")
    if (test$p.value < 0.05) {
      significant_count = significant_count + 1
    }
  }
  results_55[i] = significant_count
}

#Plot results for p = 0.55 using ggplot2
results_df_55 = data.frame(Num_Flips = num_flips, Significant_Count = results_55)
ggplot(results_df_55, aes(x=Num_Flips, y=Significant_Count)) +
  geom_line() +
  geom_point() +
  labs(title="Significant Unfairness Detection (p=0.55)",
       x="Number of Coin Flips",
       y="Number of Significant Detections out of 100") +
  theme_minimal()

#Repeat the simulation for p = 0.6 and p = 0.65
p_values = c(0.6, 0.65)
results_list = list()
for (p_unfair in p_values) {
  results = numeric(length(num_flips))
  for (i in num_flips) {
    significant_count = 0
    for (j in 1:100) {
      flips = rbinom(i, 1, p_unfair)
      test = binom.test(sum(flips), i, p=0.5, alternative="greater") #alternative to using beta distribution
      if (test$p.value < 0.05) {
        significant_count = significant_count + 1
      }
    }
    results[i] = significant_count
  }
  results_list[[as.character(p_unfair)]] = results
}
#Combine results with all three p values into a single data frame for plotting
results_df_all = data.frame(Num_Flips = rep(num_flips, times=3),
                             Significant_Count = c(results_55, results_list[["0.6"]], results_list[["0.65"]]),
                             P_Unfair = rep(c(0.55, 0.6, 0.65), each=length(num_flips)))

#Plot results for all p values using ggplot2
ggplot(results_df_all, aes(x=Num_Flips, y=Significant_Count, color=factor(P_Unfair))) +
  geom_line() +
  geom_point() +
  labs(title="Significant Unfairness Detection for Different p Values",
       x="Number of Coin Flips",
       y="Number of Significant Detections out of 100",
       color="p(Unfair)") +
  theme_minimal()


