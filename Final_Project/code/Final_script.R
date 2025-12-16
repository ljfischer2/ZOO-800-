###### Final Code - ZOO 800 ######
# Lucas Fischer
#
#
##### Required Packages ######
library(respR)
library(tidyverse)



##### Import  & Clean Data #####
fish <- read.csv("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo/Fish_Data_mass_vol.csv",header=T)

Trial1 <- read.csv("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo/Trial_1_Comp.csv",header=T)
fish_T1 <- fish[1:4,]
#data1 <- read.csv("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo/Trial_1_Comp.csv",header=T)
Trial2 <- read.csv("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo/Trial_2_Comp.csv",header=T)
fish_T2 <- fish[5:12,]
Trial3 <- read.csv("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo/Trial_3_Comp.csv",header=T)
fish_T3 <- fish[13:20,]
Trial4 <- read.csv("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo/Trial_4_Comp.csv",header=T)
fish_T4 <- fish[21:28,]




Trial2 <- Trial2[-c(42751:42937), ]
Trial3 <- Trial3[-c(42751:43226), ]
Trial4 <- Trial4[-c(42751:43215), ]

Raw_Data <- list(Trial1, Trial2,  Trial3, Trial4)

###### Just calculating via raw data ######
start <- vector('integer', length = 95L)
end <- seq(450, 42750, by = 450)
for (i in 1:length(end)){
  start[i] <- end[i] - 75
}


extract <- function(data, output, numchamber) {
  
  for (i in 1:numchamber) {
    df <- data.frame(time = data[, 1],
                      fish = data[, i+4])
    output[[i]] <- df
  }
  return(output)
}

fishlist1 <- list()
fishlist1 <- extract(Trial1, fishlist1, 4)
fishlist2 <- list()
fishlist2 <- extract(Trial2, fishlist2, 8)
fishlist3 <- list()
fishlist3 <- extract(Trial3, fishlist3, 8)
fishlist4 <- list()
fishlist4 <- extract(Trial4, fishlist4, 8)

length(fishlist2)

rate_extract <- function(data, resp_rates, fishstat, start, end){
  output <- matrix(nrow = length(end), ncol = length(data))
  for (i in 1:length(data)) {
    for (o in 1:length(end)) {
      start_val <- data[[i]][[resp_rates]][start[o]]
      end_val <- data[[i]][[resp_rates]][end[o]]
      output[o,i] <- start_val - end_val
      output[o,i] <- output[o,i] * fishstat[i,2] * 12
      output[o,i] <- output[o,i] / fishstat[i,1]
    }
  }
  return(output)
}

rates_fl1 <- rate_extract(fishlist1, 'fish', fish_T1, start, end)
rates_fl2 <- rate_extract(fishlist2, 'fish', fish_T2, start, end)
rates_fl3 <- rate_extract(fishlist3, 'fish', fish_T3, start, end)
rates_fl4 <- rate_extract(fishlist4, 'fish', fish_T4, start, end)










warnings()

i <- 2
v <- 1
fishlist <- list()
for (i in 2:4) {
  for (v in 1:8) {
    df <- data.frame(time <- Raw_Data[[i]][[1]],
                     ch <- Raw_Data[[i]][[v + 4]])
    shortlist[[i]] <- df
  }
}







##### Use RespR functions to calculate resp. rates ######
chamber2.1<-inspect(Trial2, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
inspected <- list()
i <- 2
#i <- 4
for (i in 2:4) {
  for (v in 1:8) {
    inspected[[i]][[v]] <- inspect(Raw_Data[[i]], time = 1, oxygen = v + 4)
  }
}

for (i in 2:4) {
  for (v in 1:8) {
    fishlist <- calc_rate.int(inspected[[i]][[v]],
                              starts = 450,
                              wait = 375,
                              measure = 75,
                              by = "row")
    convert_fishlist <- convert_rate(fishlist[[i]][[v]],
                                  oxy.unit = "mg/L",       # oxygen units of the original raw data
                                  time.unit = "secs",      # time units of the original raw data
                                  output.unit = "mg/d/g",  # desired output unit
                                  volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                                  mass = 0.0056)            # mass of the specimen in kg (fish wet mass: kg)
    
  }
}






fishlist <- list()
Masu2 <- calc_rate.int(chamber2.1,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

#for (i in 2:4) {
  for (v in 1:8) {
    fishlist <- calc_rate.int(inspected[[i]][[v]],
                              starts = 450,
                              wait = 375,
                              measure = 75,
                              by = "row")
  }
#}