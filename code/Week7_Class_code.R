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

# the code wants us to use an sp file type, so just use that,
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
#pal <- brewer.pal(5, "OrRd")  # we select 5 colors from the palette
# I didn't like that palette
pal <- brewer.pal(5, 'BuPu')
spplot(philly_sp, "HOMIC_R", col.regions = pal, cuts = 4)




library(classInt)
# Part III, where we change the class intervals and make it more meaningful
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
