library(tidyverse)



#Objective 1#####

#A#_#_#_#_#_#_#_#_#_#_

hell_pop <- data.frame(year = vector('numeric', 50L),
                      pop_size = vector('numeric',50L))

hell_pop$year <- seq(1:50)
hell_pop$pop_size[1] <- 50

r = 0.2
k = 1000

for (i in 2:50){
  hell_pop$pop_size[i] <- hell_pop$pop_size[i-1] +
    r * hell_pop$pop_size[i-1] * (1 - hell_pop$pop_size[i-1] / k)
}

#B#_#_#_#_#_#_#_#_

ggplot(hell_pop, aes(x = year, y = pop_size)) +
  geom_line(color = 'forestgreen', linewidth = 1.5)


# Objective 2 ##############

rvec <- rnorm(0.2,0.03, n = 50)

sim_mat <- matrix(NA, nrow = 51, ncol = 50)
sim_mat[1,] <- 50

for (o in 1:50){
  
  for (i in 2:50){
    sim_mat[i, o] <- sim_mat[i-1, o] +
      rvec[o] * sim_mat[i-1, o] * (1 - sim_mat[i-1,o] / k)
  }
  
}

sim_df <- as.data.frame(sim_mat)
sim_df$year <- 0:50
sim_df_long <- pivot_longer(sim_df, cols = sim_df$year, names_to = "simnum",
                            values_to = "pop_size")

sim_df_long_10 <- sim_df_long %>%
  filter(year %in% 1:10)



sim_plot <- ggplot(sim_df_long_10, aes(x = year, y = pop_size, group = simnum)) + 
  geom_line()

sim_plot
#Objective 3 ###########

sim_df_long_25 <- sim_df_long %>%
  filter(year %in% 1:25)

sim_plot2 <- ggplot(sim_df_long_25, aes(x = year, y = pop_size, group = simnum)) + 
  geom_line() +
  geom_hline(yintercept = 800,
             linetype = "dashed")

sim_plot2


sim_year_25 <- sim_df_long %>%
  filter(year == 25)

sim_hist <- ggplot(sim_year_25, aes(x = pop_size)) + 
  geom_histogram() + 
  geom_vline(xintercept = 800)

sim_hist                     

simtot <- sum(sim_year_25$pop_size > 800)

sim_hist <- sim_hist + 
  annotate("text",
           x = 790, y = 3, 
           label = paste("Count >", 800, ":", simtot, "/ 50"))
sim_hist
