#Lucas Fischer
# Week 9 Homework


install.packages('patchwork') # I didn't feel like pivoting my y values below 
# into their own column, so I found this package using chatGPT that allows you
# to stitch uncorrelated plots together.  Cool stuff.
library(patchwork)
library(tidyverse)

## Objective A: ###############################

#create our data frame that will serve to continually populate the plots
df <- data_frame(x = vector(length = 100L),
                 y = vector(length = 100L))

# our intercept and slope:
a <-  7.459
b <- 2.837

#and using a seed so that the values are the same every time
set.seed(16)
df$x <- runif(100, 0, 10)
e1 <- rnorm(100, 0, 1)

#we populate our y column using the equation below, including our random term
for (i in seq(1:100)) {
  df$y[i] <- a +b*x[i] + e1[i]
}
plot1 <- ggplot(df, aes(x = x, y = y)) +
  geom_point() + 
  geom_smooth()


#for e2:
e2 <- rnorm(100, 0, 10)

for (i in seq(1:100)) {
  df$y2[i] <- a +b*x[i] + e2[i]
}

plot2 <- ggplot(df, aes(x = x, y = y2)) +
  geom_point() + 
  geom_smooth()
plot2

#for e3:
e3 <- rnorm(100, 0, 25)

for (i in seq(1:100)) {
  df$y3[i] <- a +b*x[i] + e3[i]
}

plot3 <- ggplot(df, aes(x = x, y = y3)) +
  geom_point() + 
  geom_smooth()
plot3

#patchwork enters here:
full_plot <- plot1 + plot2 + plot3
full_plot

## Objective B: ##############################

#here we have the histogram representing that in 100 trials, what is the likely
# probability of our two outcomes. values ~ 1 are high alphas, values ~ 0 are
# high betas
n = 100


hist(rbeta(100, 21, 1))
hist(rbeta(100, 1, 21))

#The code below was taken from Olaf's code "CLT_Demo.R", since it is
# applicable here.  It determines probability.  0.5 = an even coin "50/50", 
# the code below represents the probability that if you would get 5 heads, and
# 1 tails, that the true probability is less than 0.5.

#Calculate the probability density to the left of 0.5; 1 minus this probability 
#is the probability that p(heads) > 0.5
pbeta(0.55, 5, 1)



set.seed(81) #using seed 81 because.  Also to make same graph every time.



probs <- matrix(nrow = 100, ncol = 20) #Making a matrix seems like the best option


# Using a for loop to simulate # of coins flipped.
for (i in 1:20) {
  probs[, i] <- replicate(100, {
    heads <- rbinom(1, i, 0.6)
    binom.test(heads, i, 0.5)$p.value
  })
}

#convert it to a data frame so we can pivot the table.
probsdf <- as.data.frame(probs)
colnames(probsdf) <- paste0(1:20)
probsdf$sim <- 1:100 #Also adding what sim #.

probs_longer <- probsdf %>%
  pivot_longer(cols = !'sim', 
               names_to = "num_of_flips", values_to = "probability")


# At this point, I got a little lost and needed some help with AI.
probs_longer$num_of_flips <- as.numeric(gsub("n_", "", probs_longer$num_of_flips))

#Grouping by number of flips, then summarizing.  I got stuck here
# as I was visualizing the problem, since I was going to try and plot all of
# the results, rather than summarizing them.  I think this is porbably the 
# more efficient way of doing this.
results_summary <- probs_longer %>%
  group_by(num_of_flips) %>%
  summarise(prop_significant = mean(probability < 0.05))

ggplot(results_summary, aes(x = num_of_flips, y = prop_significant * 100)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(

    x = "# of Coin Flips",
    y = "% detection)"
  ) +
  theme_minimal(base_size = 14)

# Rinse and repeat for the next two probabilities as well.
#I probably could do this either in a function or in a loop, looking at it now,
# but for 3 probabilities this was the easiest option

probs2 <- matrix(nrow = 100, ncol = 20)

for (i in 1:20) {
  probs2[, i] <- replicate(100, {
    heads <- rbinom(1, i, 0.6)
    binom.test(heads, i, 0.5)$p.value
  })
}

probs2df <- as.data.frame(probs2)
colnames(probs2df) <- paste0(1:20)
probs2df$sim <- 1:100

probs2_longer <- probs2df %>%
  pivot_longer(cols = !sim, 
               names_to = "num_of_flips", values_to = "probability")

probs2_longer$num_of_flips <- as.numeric(gsub("n_", "", probs2_longer$num_of_flips))

results2_summary <- probs2_longer %>%
  group_by(num_of_flips) %>%
  summarise(prop_significant = mean(probability < 0.05))




probs3 <- matrix(nrow = 100, ncol = 20)

for (i in 1:20) {
  probs3[, i] <- replicate(100, {
    heads <- rbinom(1, i, 0.65)
    binom.test(heads, i, 0.5)$p.value
  })
}

probs3df <- as.data.frame(probs3)

colnames(probs3df) <- paste0(1:20)
probs3df$sim <- 1:100

probs3_longer <- probs3df %>%
  pivot_longer(cols = !sim, 
               names_to = "num_of_flips", values_to = "probability")

probs3_longer$num_of_flips <- as.numeric(gsub("n_", "", probs3_longer$num_of_flips))

results3_summary <- probs3_longer %>%
  group_by(num_of_flips) %>%
  summarise(prop_significant = mean(probability < 0.05))

#At this point, I added the results of the higher probabilities to the 
# original results to make it easier to graph.
results_summary$test2 <- results2_summary$prop_significant
results_summary$test3 <- results3_summary$prop_significant


#Good stoplight colors seemed appropriate here.
ggplot(results_summary, aes(x = num_of_flips, y = prop_significant * 100,
                            color = "Test1")) +
  geom_line(linewidth = 1) +
  geom_line(aes(x = num_of_flips, y = test2 * 100,  color = "Test2"),
            linewidth = 1) + 
  geom_line(aes(x = num_of_flips, y = test3 * 100, color = "Test3"),
           linewidth = 1) +
  labs(x = "# of Coin Flips",
    y = "% detection") +
  scale_color_manual(
    name = "Legend Title",
    values = c("Test 1" = "green", "Test 2" = "yellow", "Test 3" = "red")) +
  theme_minimal(base_size = 14)





# Extra Code Bits that I was working with ############

#sig_results <- numeric(length(1:20))

#for (i in 1:20) {
#  pvals <- replicate(100, {
#    heads <- rbinom(1, i, 0.55)
#    no_diff <- binom.test(heads, i, 0.5)
#    no_diff$p.value
#  })
  
  
#  sig_results[i] <- mean(pvals < 0.05)
#}

#ggplot()











#for (i in seq(1:20)) { 
#  heads <- rbinom(100, i, 0.55)
#  no_diff <- 1 - pbinom(heads, i, 0.55)
#  probs[,i] <- no_diff
#}

