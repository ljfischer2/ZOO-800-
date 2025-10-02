install.packages('openxlsx')
install.packages('writexl')
install.packages('tidyverse')
library(readxl)
library(writexl)
library(tidyverse)
#library(openxlsx)

fishcsv <- read.csv("Data/fish.csv")
head(fishcsv)

fishxl<- read_excel('Data/fish.xlsx')
head(fishxl)

fishrds <- readRDS('Data/fish.rds')
head(fishrds)


write.csv(x = fishcsv, 'Data/Output/fishcsvout.csv' )
write_xlsx(fishcsv, 'Data/Output/fishcsvout.xlsx')
saveRDS(fishcsv, 'Data/Output/fishcsvout.rds')
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


fish_output <- fishcsv %>%
  filter(Species %in% c('Walleye', 'Yellow Perch', 'Smallmouth Bass'),
         Lake %in% c('Erie', 'Michigan')) %>%
  select(-Age_years) %>%
  mutate(Length_mm = Length_cm * 10) %>%
  mutate(Length_group = cut(Length_mm,
                            breaks = c(0, 200, 400, 600, Inf),
                            labels = c('<200mm', '200-400mm',
                                       '400-600mm','>600mm'),
                            right = TRUE) 
  ) %>%
  group_by(Species, Length_group) %>%
  mutate(n = n()) %>%
  group_by(Species, Lake) %>%
  summarize(
    mean_w = mean(Weight_g, na.rm = TRUE),
    median_w = median(Weight_g, na.rm = TRUE),
    total_w_g = sum(Weight_g, na.rm = TRUE),
  )
  
           
?cut
?count
