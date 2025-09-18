#Part 4####
#Question 1
squares <- c(1:10)

for  (value in squares) {
  print(value^2)
}



#Question 2
#we borrow squares from part 1 
n0 <- 10
r = 0.3
t <- c(0:10)
pop <- c(0:10)
for (value in squares) {
  n1 <- n0 * (exp(r*t[value]))
  pop[value] <- n1
  n0 <- n1
  print(pop[value])
}


