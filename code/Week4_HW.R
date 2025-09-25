install.packages("palmerpenguins")
library("palmerpenguins")


#Objective 1#####

#creating data.frame from penguins
peng <- penguins
peng$size_class <- vector('character', 344L)
max(peng$body_mass_g, na.rm = TRUE)
min()
#finging the mean, so that we can split the penguins in half.
m <- mean(x = peng$body_mass_g, na.rm = TRUE)

#creating a function that checks for body mass
bodymass.size <- function(column1, column2, m) {
  column2 <- ifelse(column1 > m, "large", "small")
  print(column2)
}
#running it or penguins
peng$size_class <- bodymass.size(peng$body_mass_g, peng$size_class, m)


#Objective 2#####
# we need to add vectors for breakpoints
#assign breakpoints from largest to smallest 
#NOTE: laregest break must be > max size
bp <- c(7500, 4500, 3500)
# assign a vector of breakpoint sizes as characters
bp_size <- c('large', 'medium', 'small')

#creating a new function to form new breakpoints
bodymass.size2 <- function(column1, column2, breakpoint, breakpoint_size){
  for (o in 1:length(breakpoint)){
    for (i in 1:length(column1)) {
      
      if(is.na(column1[i])){
        column2[i] = NA
      } else
        
        if (column1[i] < breakpoint[o]) {
          column2[i] <- breakpoint_size[o]
          
        } else{}
    }
  }
  #print(column2)
}

peng$size_class <- bodymass.size2(peng$body_mass_g, peng$size_class,
                                  bp, bp_size)

#i=1
#o=1

#test = vector()

#for (o in 1:length(bp)){
#  for (i in 1:length(peng$body_mass_g)) {
#    
#    if(is.na(peng$body_mass_g[i])){
#      test[i] = NA
#    } else
#    
##    if (peng$body_mass_g[i] < bp[o]) {
#      test[i] <- bp_size[o]
#      
#    } else{}
#  }
#}

test
