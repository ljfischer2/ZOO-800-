#week notes
library(tidyverse)
#example code:
sloth_speed <- sloth_df %>%
               select(id, age, speed)

#Part 4####
#Question 1
squares <- c(1:10)

for  (value in squares) {
  print(squares[value]^2)
}



#Question 2
#we borrow squares from part 1 
n0 <- 10
r = 0.3
t <- c(0:10)
pop <- vector('numeric', 10L)
for (value in squares) {
  n1 <- n0 * (exp(r*t[value]))
  pop[value] <- n1
  n0 <- n1
  print(pop[value])
}

#Question 3
#populating the lakes
Mendota <- c(27,18,23,33)
Monona <- c(29,15,26,36)
Wingra <- c(13,7,12,13)
Waubesa <- c(10,15,17,19)
Kegonsa <- c(16,12,22,25)

#creating a list
phosphorus <- list('Mendota' = as.numeric(Mendota),
                   'Wingra' = as.numeric(Wingra),
                   'Monona' = as.numeric(Monona),
                   'Waubesa' = as.numeric(Waubesa),
                   'Kegonsa' = as.numeric(Kegonsa))
phosphorus[[1]]
phosphorus[1]

#add an index for the for loop
element <- c(1:4)
lake_means <- c()
lake_names <- c('Mendota', 'Wingra', 'Monona', 'Waubesa', 'Kegonsa')


#Question 4
#and now iteratively going through the list and calculating the mean
for (i in element) {
  lake_means[i] <- mean(phosphorus[[i]])
  print(paste0(lake_names[i], " mean phosphorus is = ", lake_means[i], "μg/L"))
}

#Question 5
print(lake_means)





#example list of multiple data types
#test_list <- list(Mendota, data.frame(d = (c(1:3)),
#                                      x = (c(4:6)),
#                                      y = (c(7:9))),
#                                      Waubesa)
