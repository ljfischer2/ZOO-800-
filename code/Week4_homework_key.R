#
# Zoo800 Homework Week 4: Creating your own functions
#

# load in required package
library(palmerpenguins)

# the data set is auomatically populated into the environment 
# (much like functions from a package)
# you can store it as your own object if you like
View(penguins)
df = penguins

########################################################
# Objective 1

# function to convert body mass to categorical variable
convert_to_binary <- function(size, break.point) {
  
  binary_variable <- ifelse(size > break.point, "large", "small") # if number is bigger than threshold, call it "large" otherwise call it "small"
  
  return(binary_variable)
}


# create/format data that matches the arguments the function requires 
# you can just provide the specific body size column as input data here, 
# but see below where it can be useful to provide the entire data frame 
size = penguins$body_mass_g 
break.point = 4000


# see if it worked!
convert_to_binary(size = size, 
                    break.point = break.point)



########################################################
# Objective 2

# function to convert body mass to categorical variable
# (dplyr solution for the tidyverse fans)
convert_to_discrete <- function(data, breaks, labels) {
  
  require(dplyr)
  
  categorized_data <- data %>%
    mutate(
      discrete_variable = case_when(     # case_when is useful for labeling based on conditional statements
        body_mass_g < breaks[1] ~ labels[1],
        body_mass_g >= breaks[1] & body_mass_g < breaks[2] ~ labels[2],
        body_mass_g >= breaks[2] ~ labels[3]
      )
    )
  
  print(categorized_data$discrete_variable)
  
}


# create/format data
data = penguins
breaks = c(3500, 4000)
labels = c("small", "medium", "large")


# see if it worked!
convert_to_discrete(data = data, 
                    breaks = breaks, 
                    labels = labels)




########################################################
# Objective 3

# function to convert body mass to categorical variable
# conditional on species 
convert_to_discrete_spp <- function(data, 
                                    spp, 
                                    breaks, 
                                    labels) {

  cats = vector()

    for(i in 1:length(data)){

        cats[i] <- paste(cut(data[i],
                             breaks = c(-Inf, breaks[[spp[i]]], Inf),
                             labels = unlist(labels[spp[i]]),
                             include.lowest = TRUE,
                             right = TRUE))
    
    
  }  
  
  return(cats)
}


# create / format data
# note: breaks and labels must be lists for the function to work
# it is nice to have these as lists so that different species can have a different number of breaks
data = penguins$body_mass_g

spp = penguins$species

breaks = list(Adelie = c(3500, 4000), 
              Chinstrap = c(3500, 4000), 
              Gentoo = c(4000))

labels = list(Adelie = c("small", "medium", "large"), 
              Chinstrap = c("small", "medium", "large"), 
              Gentoo = c("small", "large"))


# see if it worked!
convert_to_discrete_spp(data = data,
                        spp= spp,
                        breaks = breaks,
                        labels = labels)




########################################################
# Objective 4

# function to convert body mass to categorical variable
# conditional on species 
# with automated break points
convert_to_discrete_spp_auto_bp <- function(data, 
                                            labels) {
  
  require(dplyr) # require is nice inside a function, so that a user doesn't have to remember to load the package

  df = data %>%  # find quantiles by species
    group_by(species) %>%  # this could easily be done in base R, but group_by() in dplyr is very nice, it's the one tidy thing I routinely use
    reframe(breaks = quantile(body_mass_g, probs = c(0.25, 0.75), na.rm = T))
  
  breaks = split(df, df$species) # split() converts a data frame into a list, nice for indexing
  
  cats = vector()
  
  for(i in 1:length(data$species)){
    
    cats[i] <- paste(cut(data$body_mass_g[i],
                         breaks = c(-Inf, breaks[[data$species[i]]]$breaks, Inf),
                         labels = unlist(labels[data$species[i]]),
                         include.lowest = TRUE,
                         right = TRUE))
    
    
  }  
  
  return(cats)
}


# create / format data
data = penguins

# for this example, technically only need one set of "small, medium, large" 
# but could generalize the function to have a different number of breaks for each species 
# so would need species-specific labels
labels = list(Adelie = c("small", "medium", "large"), 
              Chinstrap = c("small", "medium", "large"), 
              Gentoo = c("small", "medium", "large"))

# see if it works!
convert_to_discrete_spp_auto_bp(data, labels)



########################################################
# Objective 5

# function to convert body mass to categorical variable
# conditional on species 
# with automated break points
# and plot results
convert_to_discrete_spp_auto_bp_plot <- function(data, 
                                                 labels) {

# call required functions within the function    
  require(dplyr)
  require(ggplot2)

# group data by species and calculate size quantiles for each  
  df = data %>%
    group_by(species) %>%
    reframe(breaks = quantile(body_mass_g, probs = c(0.25, 0.75), na.rm = T))

# split data into one data frame for each species for looping purposes    
  breaks = split(df, df$species)

# create storage vector    
  cats = vector()
  
  for(i in 1:length(data$species)){ # loop round each species and discretize body size
    
    cats[i] <- paste(cut(data$body_mass_g[i],
                         breaks = c(-Inf, breaks[[data$species[i]]]$breaks, Inf),
                         labels = unlist(labels[data$species[i]]),
                         include.lowest = TRUE,
                         right = TRUE))
    
    
  }  

# append categories to the original data frame and plot  
  data$cats = cats

  ggplot(data, aes(species, body_mass_g, fill = cats)) +
    geom_boxplot() + theme_classic()
    
}


# input data
data = penguins
# now we see the value of providing the full data frame to the function
# can append a new column and then use regular plot funtions easily 

labels = list(Adelie = c("small", "medium", "large"), 
              Chinstrap = c("small", "medium", "large"), 
              Gentoo = c("small", "medium", "large"))

# run function
convert_to_discrete_spp_auto_bp_plot(data, labels)
