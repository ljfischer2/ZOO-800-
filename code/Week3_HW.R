#Part 1####

temp_C <- 18.5
temp_F = temp_C * (9/5)+ 32

temp_state <- paste("The water temperature is", temp_C, "°C (", temp_F," °F)") 
print(temp_state)


#Part 2####

species_counts <- c(Bluegill = 12, Bass = 7, Sunfish = 21, Carp = 3)

fish_counted <- sum(species_counts)
fish_counted

highest_count <- max(species_counts)
highest_count_species <- names(which.max(species_counts))

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
#4. [BONUS] install package <LakeMetabolizer>. Add new column for the equilibrium concentration of oxygen in water. Add a second new column of dissolved oxygen % 
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

#4. #4. [BONUS] install package <LakeMetabolizer>. Add new column for the equilibrium concentration of oxygen in water. Add a second new column of dissolved oxygen % 
# saturation. Sort the dataframe in order of DO % saturation using the order() function. 

install.packages("LakeMetabolizer")

library(LakeMetabolizer)


lakes$DO_sat <- o2.at.sat.base (temp = lakes$Temp_C,baro=1000, altitude = 0, salinity = rep(0, length(Temp_C)),  model = "garcia-benson") 


# Part5 
#2. Revisit your lakes data frame. Use apply() to calculate the range (max – min) of each  numeric column.
numeric_cols <- sapply(lakes, is.numeric)
ranges <- apply(lakes[, numeric_cols],2,function(x) rev(range(x)))
ranges
          



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


#Part 5####

#Part 5

#mean chlorophyll concentrations (across 3 days) for each depth
means_depths <- apply(chlorophyll_conc, 1, mean)
means_depths

#mean chlorophyll concentrations (across 3 depths) for each day
means_days <- apply(chlorophyll_conc, 2, mean)
means_days



# Part5 
#2. Revisit your lakes data frame. Use apply() to calculate the range (max – min) of each  numeric column.
numeric_cols <- sapply(lakes, is.numeric)
ranges <- apply(lakes[, numeric_cols],2,function(x) rev(range(x)))
ranges
