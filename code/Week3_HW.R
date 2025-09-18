#Part 1

temp_C <- 18.5
temp_F = temp_C * (9/5)+ 32

temp_state <- paste("The water temperature is", temp_C, "°C (", temp_F," °F)") 
print(temp_state)


#Part 2

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

                  