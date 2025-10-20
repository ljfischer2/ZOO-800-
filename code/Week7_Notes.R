# Three classes of spatial data:
#point, where you generally only have one point for each data
#raster data, gridded representation of a spatially contini=uous process
# -> often layers overlayed
#lattice data, regular and irr. lattices, information conveyed by polygons
#

library(XML)
library(ggmap)
library(tidyverse)
library(RCurl)

url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate_(2012)"
#citiesCR <- readHTMLTable(GetURL(url), which = 1)  #outdated
library(rvest)


#geocode location
library(tidygeocoder)
latlon <- geocode(paste(citiesCRclean$City, citiesCRclean$State, sep = ','))

library(maps)
us_map <- map_data("state")

#plot
ggplot() +
  geom_polygon(data = us_map, aes())




#Excercise 1:######

#install.packages('sf')
#install.packages('sp')
#install.packages('maps')
library(sf)
library(maps)
library(sp)
sf_path <- 'Data/Philly3/Philly3.shp'
philly <- st_read(sf_path)
names(philly)

plot(philly)

philly_sp <- as(philly, "Spatial")
# Now use spplot
spplot(philly_sp)

spplot(philly_sp, "HOMIC_R")
# or
spplot(philly_sp, c("HOMIC_R", "PCT_COL"))



#This sections is for creating a color palette, pretty useful later


# Load the library
library(RColorBrewer)

# to explore, display all sequential color schemes now available
display.brewer.all(type = "seq")

# now let's use one of them, called OrRd
pal <- brewer.pal(5, "OrRd")  # we select 5 colors from the palette
pal <- brewer.pal(5, 'YlOrRd')
spplot(philly_sp, "HOMIC_R", col.regions = pal, cuts = 4)





library(classInt)

# determine the breaks
breaks.qt <- classIntervals(philly$HOMIC_R, n = 5, style = "quantile")

# add a very small value to the top breakpoint, and subtract from the bottom
# for symmetry
br <- breaks.qt$brks
offs <- 1e-07
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs

# plot
spplot(philly_sp, "HOMIC_R", col.regions = pal, at = br, main = "Philadelphia homicide rate per 100,000")
