
#####
# random example I found on how to plot in 3D
# not really helpful, but super fun to look at and play with


# Library
install.packages("plotly")
library(plotly)

# Data: volcano is provided by plotly

# Plot
p <- plot_ly(z = volcano, type = "surface")
p 




#Actual notes

#example function
any.function = function(arg1, arg2, arg3...){
  whatever instructions
}

function(x, decreasing = FALSE, ...) {
  if (!is.logical(decreasing) || length(decreasing) != 1L)
    stop("'decreasing' must be a length 1 logical vector")
  
}



# von betalanffy curve
# lt = linf * (1 - exp(-k * (t - t0)))

# t is age
# lt is length at age t
# k is growth rate
# t0 is length at age 0

#cod
linf <- 100
k <- 0.4
t0 <- -0.3
t <- 0:20

lt = linf * (1 - exp(-k * (t - t0)))
lt

# if you are doing the same thing multiple times, using a function saves time.

size.at.age <- function(linf, k, t0 = 0, t){
  lt = linf * (1 - exp(-k * (t - t0)))
  print(lt)
  plot(lt)
}

#do the same thing again
# cod
linf <- 100
k <- 0.4
t0 <- 0.3
t <- 0:20

size.at.age(linf, k, t0, t)

#herring
linf = 20
k = 0.5
t0 = 0
t = 0:6


# using the function we created
size.at.age(linf, k, t, t0)

linf <- c(20, 70, 100)
k <- c(0.4, 0.7, 0.5)
t0 <- c(-0.3, -0.2, 0)
t <- list(0:6, 0:10, 0:20)

# what if we created a function that plots, and runs through multiple species?
multi.size.at.age <- function(linf, k, t0, t){
  
  lt <-  linf[1] * (1 - exp(-k[1] * (t[[1]] - t0[1])))
  plot(t[[1]], lt, type = "l", xlim = c(0, max(unlist(t))),
       ylim = c(0, max(linf + 10)))
  
  
  for (i in 2:length(linf)) {
    lt <-  linf[i] * (1 - exp(-k[i] * (t[[i]] - t0[i])))
    
    lines(t[[i]],lt)
  }
}


multi.size.at.age(linf, k, t0, t)

# good advice: split into multiple scripts, keep scripts short (<200 lines)
# have 1 script to import raw data
# script 2 starts the analysis, pulls from raw data
# script 3 is figures, pulls from analysis
# you can create a script for only the functions you need.








