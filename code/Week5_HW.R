#-------------------------------------------------------------------------------
#-----Name: Lucas Fischer -----


#install.packages('openxlsx')
#install.packages('writexl')
#install.packages('tidyverse')

#rm(list = ls())

#First we need to install all of the packages we will be using
library(readxl)
library(writexl)
library(tidyverse)
#library(openxlsx)

#Problem 1######
fishcsv <- read.csv("Data/fish.csv")
head(fishcsv)

fishxl<- read_excel('Data/fish.xlsx')
head(fishxl)

fishrds <- readRDS('Data/fish.rds')
head(fishrds)

#Problem 2######

#Once we have read all of the files, we need to rewrite them and export them
# out of R.  If I am honest, my naming convention here was terrible.
# But whatever, it should be alright.
write.csv(x = fishcsv, 'Data/Output/fishcsvout.csv' )
write_xlsx(fishcsv, 'Data/Output/fishcsvout.xlsx')
saveRDS(fishcsv, 'Data/Output/fishcsvout.rds')
#The comment line below is for the package 'openxlsx'.  Not necessary
#saveWorkbook(fishcsv, 'Data/Output/fishcsvout.csv')

file.info('Data/Output/fishcsvout.csv', 
          'Data/Output/fishcsvout.xlsx', 
          'Data/Output/fishcsvout.rds')

#I think there is something odd about the sizes listed in R compared to 
# what is reported in the actual file viewer, but based on this I would
# think the .csv file is the most convenient in general, while the .rds file
# is the most convenient for sending, as long as the receiver has R.
# Otherwise I would likely still elect to use the .csv file. For File 
# storage, a .rds format would be the best option.
#.csv size: ~14 kb (~11 kb in file viewer)
#.rds size: ~3 kb 
#.xlsx size: ~14 kb (~18 kb in file viewer)


#Problem 3######

fish_output <- fishcsv %>%
  filter(Species %in% c('Walleye', 'Yellow Perch', 'Smallmouth Bass'),
         Lake %in% c('Erie', 'Michigan')) %>%
  select(-Age_years) %>% #Easier than selecting all the ones you DO need
  mutate(Length_mm = Length_cm * 10) %>%
  mutate(Length_group = cut(Length_mm,      # I wish i understood cut function
                            breaks = c(0, 200, 400, 600, Inf), #          more
                            labels = c('<200mm', '200-400mm',
                                       '400-600mm','>600mm'),
                            right = TRUE) 
  ) %>%
  group_by(Species, Length_group) %>%
  mutate(n = n()) %>%
  group_by(Species, Lake) %>%
  mutate(
    mean_w = mean(Weight_g, na.rm = TRUE),
    median_w = median(Weight_g, na.rm = TRUE),
    total_w_g = sum(Weight_g, na.rm = TRUE),
  )

ggplot(fish_output, aes(Year,mean_w,
                        group = Species,
                        color = Species)) +
geom_smooth()     # I tried this with geom_line, and it looked terrible.
                  # Even if geom_smooth is not ideal, it looks much nicer.

write.csv(x = fish_output, "Data/Output/fish_output.csv")

#Problem 4######


#Name is a bit long, but is definitely explanatory
fish_year_files <- list.files("Data", 
                        pattern = 'fish_',
                        full.names = TRUE)
# fish_year_files
fish_year_files_csv <- lapply(fish_year_files, read.csv)

#In my own code, I would probably have used rbind function.  This works better.
all_fish_all_year <- dplyr::bind_rows(fish_year_files_csv)



#Problem 5 (Parallel Computing Code provided by Luoliang.  If you are the
# one who grades these, I hope that your day is going well!  If it is Olaf,
# or someone else, I hope your day is also going well!).  I left all of the 
# original comments since they would explain it a lot better than I could.
##################

# --- Setup: load base parallel, read data --------------------------------

library(parallel)                          # built-in; no install needed

fish <- read.csv("Data/fish_bootstrap_parallel_computing.csv")     # adjust path if needed
species <- unique(fish$Species)            # list of species we'll loop over


# --- A tiny bootstrap function (no pipes, base R only) --------------------

boot_mean <- function(species_name, n_boot = 500, sample_size = 100) {
  # Pull the Length_cm vector for just this species
  x <- fish$Length_cm[fish$Species == species_name]
  
  # Do n_boot resamples WITH replacement; compute the mean each time
  # replicate(...) returns a numeric vector of bootstrap means
  means <- replicate(n_boot, mean(sample(x, size = sample_size, replace = TRUE)))
  
  # Return the average of those bootstrap means (a stable estimate)
  mean(means)
}


# --- SERIAL version: one core, one species after another ------------------

t_serial <- system.time({                   # time the whole serial run
  res_serial <- lapply(                     # loop over species in the main R process
    species,                                # input: vector of species names
    boot_mean,                              # function to apply
    n_boot = 10000,                           # number of bootstrap resamples per species
    sample_size = 200                       # bootstrap sample size
  )
})

# head(res_serial)


# --- PARALLEL version: many cores using a PSOCK cluster (portable) --------

n_cores <- max(1, detectCores() - 1)        # use all but one core (be nice to your laptop)
cl <- makeCluster(n_cores)                  # start worker processes

clusterSetRNGStream(cl, iseed = 123)        # make random numbers reproducible across workers

# Send needed objects to workers (function + data + species vector)
clusterExport(cl, varlist = c("fish", "boot_mean", "species"), envir = environment())

t_parallel <- system.time({                 # time the parallel run
  res_parallel <- parLapply(                # same API as lapply(), but across workers
    cl,                                     # the cluster
    species,                                # each worker gets one species (or more)
    boot_mean,                              # function to run
    n_boot = 10000,                           # same bootstrap settings as serial
    sample_size = 200
  )
})

stopCluster(cl)                             # always stop the cluster when done


# --- Compare runtimes & show speedup --------------------------------------

# Extract elapsed (wall) time and compute speedup = serial / parallel
elapsed_serial   <- unname(t_serial["elapsed"])
elapsed_parallel <- unname(t_parallel["elapsed"])
speedup <- elapsed_serial / elapsed_parallel

cat("Serial elapsed (s):   ", round(elapsed_serial, 3), "\n")
cat("Parallel elapsed (s): ", round(elapsed_parallel, 3), " using ", n_cores, " cores\n", sep = "")
cat("Speedup:               ", round(speedup, 2), "x\n", sep = "")


#Serial elapsed (s):    1.51 
#Parallel elapsed (s): 0.42 using 11 cores
#Speedup:               3.6x

#The parallel ended up being about 3.6 times faster than in series.  There
# was a funny moment where the series was faster due to a typo.  For a second
# I was very confused, then laughed because of the improbability of it.
