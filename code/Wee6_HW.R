#Homework 6 #########
#Lucas Fischer


library(tidyverse)
#Objective 1#####

#A#_#_#_#_#_#_#_#_#_#_

#create a dataframe to be poulated by the equation
hell_pop <- data.frame(year = vector('numeric', 50L),
                      pop_size = vector('numeric',50L))

hell_pop$year <- seq(1:50)
hell_pop$pop_size[1] <- 50

#add variables
r = 0.2
k = 1000


#and run a for loop to popluate
for (i in 2:50){ # notice the 2:50: this is because of the year 0
  hell_pop$pop_size[i] <- hell_pop$pop_size[i-1] +
    r * hell_pop$pop_size[i-1] * (1 - hell_pop$pop_size[i-1] / k)
}

#B#_#_#_#_#_#_#_#_

# I chose a green line because green looks good.
plot1 <- ggplot(hell_pop, aes(x = year, y = pop_size)) +
  geom_line(color = 'forestgreen', linewidth = 1.5) + 
  xlim(1,15) + 
  ylim(0,500)

plot1



# Objective 2 ##############

#A #_#_#_#_#_#_#_#


rvec <- rnorm(0.2,0.03, n = 50)

#Lonnie showed me how to create a matrix and populate it.
#Super helpful for this format, would've taken me hours
sim_mat <- matrix(NA, nrow = 51, ncol = 50)
sim_mat[1,] <- 50

#I liked Lonnie's loop, but I kept the rnorm outside and used the outside
# loop to run through each r made in the vector
for (o in 1:50){
  
  for (i in 2:50){
    sim_mat[i, o] <- sim_mat[i-1, o] +
      rvec[o] * sim_mat[i-1, o] * (1 - sim_mat[i-1,o] / k)
  }
  
}

#This part was also a huge help from Lonnie, using the pivot.longer function
#Definitely would've needed some outside help for this part.
sim_df <- as.data.frame(sim_mat)
sim_df$year <- 0:50
sim_df_long <- pivot_longer(sim_df, cols = sim_df$year, names_to = "simnum",
                            values_to = "pop_size")
#this line is just to even the simulation with the given first line
sim_df_long$year <- sim_df_long$year + 1

#I simulated out the 50 years, figuring it would be easier to cut data than
# create it later.
sim_df_long_10 <- sim_df_long %>%
  filter(year %in% 1:10)


#Adding the simulations to the first plot.
plot2 <- plot1 + 
  geom_line(data = sim_df_long_10, aes(x = year, 
                                       y = pop_size, 
                                       group = simnum),
            linetype = "dashed")

plot2


#Objective 3 ###########

#same as before, filtering out the last 25 years.
sim_df_long_25 <- sim_df_long %>%
  filter(year %in% 1:25)

#adding it to the plot
plot3 <- plot2 + 
  geom_line(data = sim_df_long_25, aes(x = year, 
                                       y = pop_size,
                                       group = simnum),
            linetype = 'dashed') +
  geom_hline(yintercept = 800,
             linetype = "dashed",
             linewidth = 1.5) + 
  xlim(1,25) + 
  ylim(0,900)

plot3


sim_year_25 <- sim_df_long %>%
  filter(year == 25)

sim_hist <- ggplot(sim_year_25, aes(x = pop_size)) + 
  geom_histogram() + 
  geom_vline(xintercept = 800,
             color = 'red',
             linetype = 'dashed',
             linewidth = 1.5)

sim_hist                     

simtot <- sum(sim_year_25$pop_size > 800)

#I am not sure if this is the most efficient way of doing this, but whatever.
sim_hist2 <- sim_hist + 
  annotate("text",
           x = 790, y = 5, 
           label = paste("Count >", 800, ":", simtot, "/ 50"))
sim_hist2

#Sorry if this was a more tame commenting, It is early enough in the day
# That I have not lost my mind.
