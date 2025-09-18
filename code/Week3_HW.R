#Part 1

temp_C <- 18.5
temp_F = temp_C * (9/5)+ 32

temp_state <- paste("The water temperature is", temp_C, "°C (", temp_F," °F)") 
print(temp_state)


#Part 2

species_counts <- c(Bluegill = 12, Bass = 7, Sunfish = 21, Carp = 3)

#sum to find total number of fish counted
fish_counted <- sum(species_counts)
fish_counted

#find species with highest count 
highest_count <- max(species_counts)
highest_count_species <- names(which.max(species_counts))

#set chlorophyll concentration values
chlorophyll_conc <- matrix(c(21, 22, 23, 24, 25, 26, 27, 28, 29), nrow = 3, ncol = 3)
#name rows: depths
rownames(chlorophyll_conc) <- c("Surface", "Mid", "Bottom")
#name columns: days 
colnames(chlorophyll_conc) <- c("Day 1", "Day 2", "Day 3")
print(chlorophyll_conc)

#average the values for each row (depth)
depth_averages <- (rowMeans(chlorophyll_conc))
depth_averages

#Part 5

#mean chlorophyll concentrations (across 3 days) for each depth
means_depths <- apply(chlorophyll_conc, 1, mean)
means_depths

#mean chlorophyll concentrations (across 3 depths) for each day
means_days <- apply(chlorophyll_conc, 2, mean)
means_days
                  