#Part 1####
# Hi, Lucas here.  I think these lines are beyond the need of comments here, as
# it all makes sense and is obvious to me.
# still, I am going to double check all of the code chunks and make sure that 
# even a spiny water flea could figure this stuff out.  This was not my part, 
# but I kind of like commenting in code.  It makes it feel personal.

# Also, Sunday scaries hit hard, and I am in that mind-state of "fuck it, we're 
# in grad school.  PIs and Profs like to have fun too."  give them some fun code
# to read through.  Also I am doing this to ignore actual responsibility.  Lots 
# of grading for BIO 151 to do.



#storing variables, and math.
temp_C <- 18.5
temp_F = temp_C * (9/5)+ 32

#Print lines, saving a paste as a variable
temp_state <- paste("The water temperature is", temp_C, "°C (", temp_F," °F)") 
print(temp_state)


#Part 2####
#populate the vector, give them names
species_counts <- c(Bluegill = 12, Bass = 7, Sunfish = 21, Carp = 3)

#Math and math functions
fish_counted <- sum(species_counts)
fish_counted

highest_count <- max(species_counts)
highest_count_species <- names(which.max(species_counts))


# creating a matrix.  Haven't worked with matrices in a long time.
# I miss math class from being a kid.  Such a simple time.
chlorophyll_conc <- matrix(c(21, 22, 23, 24, 25, 26, 27, 28, 29), nrow = 3, ncol = 3)
rownames(chlorophyll_conc) <- c("Surface", "Mid", "Bottom")
colnames(chlorophyll_conc) <- c("Day 1", "Day 2", "Day 3")
print(chlorophyll_conc)

depth_averages <- (rowMeans(chlorophyll_conc))
depth_averages

#####Part 3#####


##############################
#########    HW3   ########### 
##### Part 3: Data Frames ####
#####      Frank          ####
##############################


#*********************************************************************************
#You sampled dissolved oxygen (mg/L) and temperature (°C) in 5 lakes.Lake Temp_C DO_mgL 
#Mendota 22.4 8.3 
#Wingra 25.1 6.7 
#Monona 23.7 7.5 
#Waubesa 24.6 7.9 
#Kegonsa 26.0 6.2 
#1. Enter this data into a data frame called lakes. 
#2. Calculate the mean temperature and mean dissolved oxygen across all lakes. 
#3. Add a new column called Temp_F with values converted to Fahrenheit. 
#4. [BONUS] install package <LakeMetabolizer>. Add new column for the equilibrium
# concentration of oxygen in water. Add a second new column of dissolved oxygen % 
# saturation. Sort the dataframe in order of DO % saturation using the order() function. 
#*********************************************************************************
#####
#1. make the date frame
Lake <- c("Mendota", "Wingra", "Monona", "Waubesa","Kegonsa " )
Temp_C <- c(22.4, 25.1, 23.7, 24.6, 26.0)
DO_mgL <- c (8.3, 6.7, 7.5, 7.9, 6.2)
lakes <- data.frame(Lake, Temp_C, DO_mgL)

#2. Calculate the mean temperature and mean dissolved oxygen
mean_temp <- mean(lakes$Temp_C)
mean_temp

# Calculate the dissolved oxygen
mean_DO <- mean(lakes$DO_mgL)
mean_DO

#3. Add a new column called Temp_F with values converted to Fahrenheit.
lakes$Temp_F <- lakes$Temp_C *9/5+32

#4. #4. [BONUS] install package <LakeMetabolizer>. Add new column for the 
# equilibrium concentration of oxygen in water. Add a second new column of dissolved oxygen % 
# saturation. Sort the dataframe in order of DO % saturation using the order() function. 

#install.packages("LakeMetabolizer")

library(LakeMetabolizer)

#Hi, Lucas again.  Not really sure wtf this function is, but looks fun!
# Also, I made the data neater here.
lakes$DO_sat <- o2.at.sat.base (temp = lakes$Temp_C,
                                baro=1000, 
                                altitude = 0, 
                                salinity = rep(0, length(Temp_C)),  
                                model = "garcia-benson") 
# I assume this predicts O2 at a given temp, pressure, altitude, and salinity.


#Part 4####
#Question 1
# make a sequence for the loop to follow
squares <- c(1:10)

for  (value in squares) {
  print(squares[value]^2)
}



#Question 2
# we borrow squares from part 1 
# add all of the necessary variables
n0 <- 10
r = 0.3
t <- c(0:10)

#neat trick I learned for making an empty vector.
pop <- vector('numeric', 10L)

# my original vector:
# pop <- vector(1:10)

# and run the steps in the for loop
for (value in squares) {
  n1 <- n0 * (exp(r*t[value]))
  pop[value] <- n1
  n0 <- n1
  print(pop[value])
}

#Question 3
#populating the lakes.  I just picked numbers on vibes.  def could use RNG here.
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

# This was me practicing indeing on lists.
#phosphorus[[1]]
#phosphorus[1]

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

#Question 5, not really sure why this is a separate question but whatevs.
print(lake_means)





# example list of multiple data types.  Used to show how lists can contain 
# multiple data types, rather than function as a glorified spreadsheet.
# test_list <- list(Mendota, data.frame(d = (c(1:3)),
#                                      x = (c(4:6)),
#                                      y = (c(7:9))),
#                                      Waubesa)


#Part 5####

#Part 5

#mean chlorophyll concentrations (across 3 days) for each depth
means_depths <- apply(chlorophyll_conc, 1, mean)
means_depths

#mean chlorophyll concentrations (across 3 depths) for each day
means_days <- apply(chlorophyll_conc, 2, mean)
means_days




#2. Revisit your lakes data frame. Use apply() to calculate the range (max – min) 
# of each  numeric column.
numeric_cols <- sapply(lakes, is.numeric)
ranges <- apply(lakes[, numeric_cols],2,function(x) rev(range(x)))
ranges


#3. In Lucas's opinion, for loops seem cleaner and easier.  Given that I did not
# use the apply statements, as Frank worked on part 3, and the subsequent 
# question in part 5, I am working solely off of visuals.  I am also more used 
# to for loops, so I may just be more familiar with them already.  I like the 
# concept of having rows for each iterative step in a for loop, and it makes
# more sense in my head to see the amount of iterations, and exactly in what
# order the steps of the loop occur.