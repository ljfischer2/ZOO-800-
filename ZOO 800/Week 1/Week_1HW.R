#setwd("C:/Users/heref/Documents/Classes_Fall_25/ZOOO 800")

library(ggplot2)
library(dplyr)
library(tidyverse)


#Introduce our variables
k = 1000
r = 0.0005 #am I crazy or is this an absurdly low rate?
n0 = 1
t = 1


##### Objective1 #####

#creating a dataframe for the model
pop1 <- data.frame(year = seq(t:75),
                   population = (n0)
                   )
for (i in 2:75) {
  pop1$population[i] <- (
    k / (1 + ((k - pop1$population[i-1]) / pop1$population[i-1]) * exp(-r * pop1$year[i-1])))
}
 #We got 4 individuals... that seems wrong.
plot(pop1)

#I'll try it with vectors
time <- seq(1,75,1)
size <- k/(1 +((k - n0)/n0) * exp(-r*time))
print(size)

#fuck it, I am moving on.


##### Objective2 ####

#repeat 1 with double rate of increase
r <- 0.001

pop1$popu2 <- 1
for (i in 2:75) {
  pop1$popu2[i] <- (
    k / (1 + ((k - pop1$popu2[i-1]) / pop1$popu2[i-1]) * exp(-r * pop1$year[i-1])))
}
  
#Had to look this up because apparently I can't stop hardcoding colors in aesthetics
#Good practice code for the future to remember regarding piping and colors
pop1_long <- pop1 %>%
  pivot_longer(cols = c(population, popu2),
               names_to = "populationNum",
               values_to = "value")

#saving it as a variable for later export
# and plotting
model <- ggplot(data = pop1_long, aes(x = year, y = value, color = populationNum)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Population Growth",
       x = "Year",
       y = "# of Individuals",
       color = "Population") + 
  xlim(0,50) + 
  ylim(0,5) +
  scale_color_brewer(palette = "Set1")
 
#ggplot function to save plots
ggsave('Logistic_growth_1_Week1.png',
       plot = model)

#This took way longer than it should've...
