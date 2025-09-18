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
