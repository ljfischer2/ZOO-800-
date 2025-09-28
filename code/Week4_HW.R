install.packages("palmerpenguins")
library("palmerpenguins")
library(dplyr)
rm(list = ls())
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
bp <- quantile(x = peng$body_mass_g, probs = c(1, 0.67, 0.33), 
               na.rm = TRUE)
bp[1] <- bp[1] + 500

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
  print(column2)
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

#Objective 3, 4, 5 (I actually sort of did part 5without meaning to, the only
# thing missing is the species list, but that is probably fine.  Whoever uses
# this function can figure it out.)


#We need to determine a breakpoint for each spp. before anything
# also need to add to each max so every penguin is included
#Adelie
#bp_adel <- quantile(x = peng$body_mass_g[peng$species == 'Adelie'],
#            probs = c(1, 0.67, 0.33), na.rm = TRUE)
#bp_adel[1] <- bp_adel[1] + 1000

#Gentoo
#bp_gen <- quantile(x = peng$body_mass_g[peng$species == 'Gentoo'],
#            probs = c(1, 0.67, 0.33), na.rm = TRUE)
#bp_gen[1] <- bp_gen[1] + 1000

#Chinstrap
#bp_chin <- quantile(x = peng$body_mass_g[peng$species == 'Chinstrap'],
#         probs = c(1, 0.67, 0.33), na.rm = TRUE)
#bp_chin[1] <- bp_chin[1] + 1000


#The best way I can think of is to create an external loop that specifies the
# species, and uses that species' breakpoints

#We could probably determine breakpoints in the function for each spp
# in the loop itself.  We would still need to specify bp_size

peng_spp_list <- c('Adelie', 'Gentoo', 'Chinstrap')

#we could also generate the list internally
#unique(peng$species)
#sp_list <- unique(peng$species)


#and our function

bodymass.size3 <- function(col1, col2, col_spp, breakpoint_size){
  sp_list <- unique(col_spp)
  
  for (v in 1:length(sp_list)) {
    #first determine the breakpoints for each spp
    breakpoint <- quantile(x = col1[col_spp == sp_list[v]],
                           probs = c(1, 0.67, 0.33), na.rm = TRUE)
    breakpoint[1] <- breakpoint[1] + 500  
    
    #for each breakpoint, run through.  same as #2, but checking for matching spp.
    for (o in 1:length(breakpoint)) {
      
      for (i in 1:length(col1)) {
        
        if (col_spp[i] == sp_list[v]) {
          
          if(is.na(col1[i])){
            col2[i] = NA
          } else {
            
            if (col1[i] < breakpoint[o]) {
              col2[i] <- breakpoint_size[o]
            } else{
            }
            
          } 
        } 
        
      }  
      
    }  
    #print(col2)
  }
  # Create a data frame within the function to plot it
  plot_data <- data.frame(species = col_spp,
                          mass <- col1,
                          size_class <- col2)
  
  visual <- ggplot(data = plot_data, aes(x = species, y = mass,
                          grouping = size_class,
                          fill = size_class)) +
    geom_boxplot() +
    labs(x = 'Species',
         y = 'Body Size(g)',
         fill = 'Size Class') +
    scale_fill_brewer(palette = 'Dark2')
  
  #and return the plot and size_class
  print(visual)
  return(col2)
  
}


peng$size_class <- bodymass.size3(col1 = peng$body_mass_g,
            col2 = peng$size_class, col_spp = peng$species,
            breakpoint_size = bp_size)



#ggplot(data = NULL, aes(x = peng$species, y = peng$body_mass_g,
#                        grouping = peng$size_class,
#                        fill = peng$size_class)) +
#  geom_boxplot() +
 # labs(x = 'Species',
#       y = 'Body Size(g)',
#       fill = 'Size Class') +
#  scale_fill_brewer(palette = 'Dark2')
  



#for (v in 1:length(peng_spp_list)) {
  #first determine the breakpoints for each spp
#  breakpoint <- quantile(x = peng$body_mass_g[peng$species == peng_spp_list[v]],
#                         probs = c(1, 0.67, 0.33), na.rm = TRUE)
#  breakpoint[1] <- breakpoint[1] + 500  
  
  #for each breakpoint, run through.
#  for (o in 1:length(breakpoint)) {
#    for (i in 1:length(peng$body_mass_g)) {
#      if (peng$species[i] == peng_spp_list[v]) {
        
#        if(is.na(peng$body_mass_g[i])){
#          peng$size_class[i] = NA
#        } else {
#          
#          if (peng$body_mass_g[i] < breakpoint[o]) {
#            peng$size_class[i] <- bp_size[o]
#            
#          } else{
#          }
#          
#        } 
#      } 
#      
#    }  
#    
#  }  
#  print(peng$size_class)
#}#

