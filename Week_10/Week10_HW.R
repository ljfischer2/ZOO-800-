library(tidyverse)
library(ggfortify)
library(patchwork)
data <- read.csv("Week_10/Single sheet May2023 Data.csv")
data_ito <- data %>%
  filter(Common == "Ito")
mod1 <- lm(Respiration_g_g_day ~ Avg_temp, data = data_ito)  
summary(mod1)
 ggplot(data_ito, aes(x = Avg_temp, y = Respiration_g_g_day)) + 
   geom_point() + 
   geom_abline(slope = 0.000133, intercept = 0.00546)

autoplot(mod1)
hist(mod1$residuals)
#Since our data starts and ends above our quantiles (based on our Q-Q plot),
# this indicates that our data is likely not linear, but our model
# may still be useful for modelling the center of our data.

#There may be some Independence issues based off our Fitted Values, since the
# right side of our residual plot is tighter compared to the rest of the plot.

#Our residual plot indicates that our data may not be normally distributed,
# and that it is slightly left-skewed.

median(data_ito$Respiration_g_g_day) #0.0076
quantile(data_ito$Respiration_g_g_day, 0.95) #0.01087

predict(mod1, newdata = list(Avg_temp = c(0.0076, 0.01087)),
        interval = 'prediction')
#There doesn't seem to be much difference in the prediction intervals.  The
# fitted point and the lower bound seem to be more spread than the upper
# bound, 


#### Objective 2 ########################################################
#

#setup of our original data
x <- rnorm(100, 10, 3)
y<- rnorm(100, 20, 3)

#creating a matrix as an easier way to store our coefficients from every iteration
term_mat <- matrix(nrow = 100, ncol = 2)

#and an model list
mods <- list()

#and a dataframe for operating withing the loop to create our models using
# y values with our randomness added to them.
df_loop <- data.frame(x = x)

#and our for loop:
for (i in 1:100) { 
  ey <- rlnorm(100, i*0.05, 0.4) #THIS SHOULD BE ON DEV, NOT MEAN
  yran <- y + ey
  df_loop$yran <- yran
  mods[[i]] <- lm(yran ~ x, data = df_loop)
  term_mat[i,1] <- summary(mods[[i]])$coefficients[1,1]
  term_mat[i,2] <- summary(mods[[i]])$coefficients[2,1]
}

#Combine all of our data into one data frame, keeping only true y and x, along
# with the slope and intercept estimates
df <- data.frame(x = x,y = y, int = term_mat[,1],
           slope = term_mat[,2])
true_mod <- lm(y~x, data = df)

#checking for fit using a plot
plot2 <- ggplot(data = df, aes(x = x, y = y)) + 
  geom_point()
plot2

#creating a list of plots to be used
plots <- vector('list', 100)

#create our plots for every estimation
for (i in 1:100) {
  plots[[i]] <- plot2 +
    geom_abline(slope = df$slope[i], intercept = df$int[i]) +
    ggtitle(paste("Plot", i))
}

#compare our first predictions which have low randomness with our last ones
# with high randomness
wrap_plots(plots[1:10], ncol = 5, nrow = 2)
wrap_plots(plots[91:100], ncol = 5, nrow = 2)


#generating our list of models
x_list <- list(x)
predict_list <- list()
for (i in 1:100){
predict_list[[i]] <- predict(mods[[i]], newdata = x_list,
        interval = 'prediction')
}

#Uncertainty in our response tends to scale a lot faster than uncertainty in our
# predictor.  Our predictions started ranging very tightly to our response, but
# slowly began increasing as our random variable increased.
true_df <- data.frame(x = x)
true_predict <- predict(true_mod, true_df, interval = 'prediction')

#each iteration of our y values slowly has a lower fraction of y values in the
# prediction range.  however, I think this is due to my own mistake in 
# calculating an uncertainty.  The uncertainty slowly grew out of proportion 
# of the original slope and intercept.

#I don't think I understand what the HW prompt was asking for in terms of setup,
# since I was ending up with lists 4mb in size.

