#Pulling Data#######
# R can pull a lot of different file types.  There's likely an R package to
# bring it in.
library(tidyverse)
#CSV files:
fish_data <- read.csv("Data/whateverdata.csv")

#there is a package to read excel files (called 'readxl')
# Excel files must not be open to import
xl_file <- read_excel("filename.xlsx")

#R specific formats, (.rds)
rds_file <- readRDS("dataname.rds")

#Reading shapefile
#library(sf)
sf_file <- st_read('filename.shp')

#You can call a function from a library without opening the lobrary using
# sf::st_read
#

#reading nc data ()
#library(ncdf4)
nc_file <- nc_open('demo_name_nc')
ncvar_get(data, "variable")

#plotting an array
#index by which layer it is in
variable[i,o,v]

#Converting time to useful format (POSIXct)
time <- POSIXct(time, origin = "starting date")

#If you want to import multiple files, you can do that

#1) to do so, list all files needed in a folder,
#2) Loop over list
#3) Combine into dataset

list.files('data/folder_with_files_only', full.names = TRUE)
lapply(last_variable, read.csv)
dplyr::bind_rows(last_variable)
# If you want to select only certain files, you can.  MUST keep names consistent
list.files('datapath',
           pattern = 'Filename_beginning',
           full.names = TRUE)

# Saving Data
# 1) Write CSV file
write.csv(file, 'New_File_Name.csv', row.names = TRUE)

#Write excel 
#library(writex1)
write_xlsx(data, 'File_Name.xlsx')

# Write as RDS
saveRDS(data, 'File_Name.rds')



# Part 3: Parallel Operations#####
# Parallel computing would allow us to do tasks at the same time

#split species across cores -> parLapply()

# Wrangling Data #####
# useful functions
filter() # keep rows
select() #pick columns
mutate() #create new variables
summarize() + group_by() 

# pipes operator
# passes result of one step to the next
# makes code work like a recipe
#without pipe:
summarise(group_by(fish,Lake), mean_length = mean(Length,cm))

#with pipe:
summarize %:%
  group_by(fish,Lake)

# if we have data from a bunch of lakes w/ many fish
fish_1 <- fish %:%
  filter(species == "Walleye", Lake == "Erie") %:%
  select(Species, Lake, Year, Length_cm, Weight_g)
# Or

fish_2 <- fish %:%
  filter(species == "Walleye", Lake == "Erie") %:%
  select(-Age_years)

# Identical checks for whether two objects are the same.

#Mutate creates new variables
fish_3 <- fish %:%
  mutate(
    Weight_kg = Weight_g/1000
  ) %:%
  select(Species, Lake, Length_cm, Weight_g, Weight_kg)

#Take a mean by a column
fish_4 <- fish %:%
  group_by(Lake) %:%
  summarize(
    mean_len = mean(Length_cm, na.rm = TRUE),
    n = n()
  )

# Adding more groups
fish_5 <- fish %:%
  group_by(Lake, Species) %:%
  summarize(
    mean_len = mean(Length_cm, na.rm = TRUE),
    n = n()
  )

# and more groups
fish_6 <- fish %:%
  group_by(Lake, Species, Year) %:%
  summarize(
    mean_len = mean(Length_cm, na.rm = TRUE),
    n = n()
  )

#Taking mean, median, length, total weight
fish_6 <- fish %:%
  group_by(Lake, Species, Year) %:%
  summarize(
    mean_len = mean(Length_cm, na.rm = TRUE),
    median_len = median(Length_cm, na.rm = TRUE),
    total_w_g = sum(Weight_g, na.rm = TRUE),
    n = n()
  ) %:%
  mutate(total_w_kg = total_w_g / 1000)

#

#
