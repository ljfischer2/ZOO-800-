library(sf)
library(maps)
library(sp)

#Sarufutsu agricultural data, essentially not helpful
sf <- 'Data/agri202001511.shp'
sf_jpn <- st_read(sf)

plot(sf_jpn)

#Sarufutsu
sf2 <- 'Data/r2kb01511.shp'
sf2_jpn <- st_read(sf2)
plot(sf2_jpn)

#Hamatonbetsu
sf3 <- 'Data/r2kb01512.shp'
sf3_jpn <- st_read(sf3)
plot(sf3_jpn)

#Surprisingly, I was able to understand enough about from the Japan
# GIS database to gather some sort of map.  The only thing I could find was
# this shapefile of Sarufutsu village and the census of it. I am just going to
# make a map based on the population density of people in the village

ggplot(sf2_jpn) + 
  geom_sf(aes(fill = JINKO)) + 
  scale_fill_gradient(
    low = "white",
    high = "red",
    name = "Population Density",
    limits = c(0, 400),
    breaks = c(0, 25, 50, 75, 100, 200, 400),
    labels = c("0", "25", "50", "75", "100", "200", "400")) +
  theme_minimal() + 
  theme(legend.key.size = unit(1, "cm"),
    legend.title = element_text(size = 13))



-# Extra bits and bobs I found when trying to find any useful Japan data
# TIF File ################
#install.packages(c("terra", "raster", "tmap", "ggplot2"))
#library(terra)    
#library(tmap)     
#library(ggplot2)   

# Read the TIF file (GeoTIFF)
#tif_path <- "Data/GSJ_MAP_G200_NL5411_1981_F1_geotiff.tif"
#raster_data <- rast(tif_path)

# Check the raster
#print(raster_data)

#plot(raster_data)

# KMZ File #######################################################

#install.packages(c("sf", "xml2"))

# Path to your .kmz file
#kmz_file <- "Data/GSJ_MAP_G200_NL5411_1981.kmz"

# Unzip to a temporary directory
#unzip(kmz_file, exdir = "Data")

# Look for the .kml file inside
#list.files("Data", pattern = "\\.kml$")


#library(sf)

# Read the .kml file (assuming it's called "doc.kml")
#kml_path <- "Data/G200k_Esashi_map.kml"  # adjust name if different

#kml_data <- st_read(kml_path)

# View the structure
#print(kml_data)
#plot(kml_data)
