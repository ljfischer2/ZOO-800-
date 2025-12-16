library(tidyverse)
library(lmtest)
library(ggfortify)
library(insight)
library(knitr)
data <- read.csv("Final_Project/data/CTM_data_analysis.csv")
data <- data[-c(69:72),]

###### Creation of models & asumption checking #######

mod1 <- glm(Temp_LOE ~ Species + Fork_Length_mm + Weight_g, data = data)
mod2 <- glm(Temp_LOE ~ Species * Fork_Length_mm + Weight_g, data = data)
mod3 <- glm(Temp_LOE ~ Species + Fork_Length_mm, data = data)
mod4 <- glm(Temp_LOE ~ Species, data = data)
mod5 <- glm(Temp_LOE ~ Species * Fork_Length_mm, data = data)   

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)   #All good
summary(mod5)

NLL_mod1 <- logLik(mod1)
NLL_mod2 <- logLik(mod2)
NLL_mod3 <- logLik(mod3)
NLL_mod4 <- logLik(mod4)
NLL_mod5 <- logLik(mod5)

lrtest(mod1, mod2, mod3, mod4, mod5)

autoplot(mod1)
autoplot(mod2)
autoplot(mod3)
autoplot(mod4)
autoplot(mod5)    # All plots look good and normal and stuff

mod1_AIC = 2*(-NLL_mod1 - 5)
mod2_AIC = 2*(-NLL_mod2 - 5)
mod3_AIC = 2*(-NLL_mod3 - 4)
mod4_AIC = 2*(-NLL_mod4 - 3)
mod5_AIC = 2*(-NLL_mod4 - 4)

AIC_tbl <- data.frame(
                      NLL = c(NLL_mod1,NLL_mod2,NLL_mod3, NLL_mod4, NLL_mod5),
                      AIC = c(mod1_AIC, mod3_AIC, mod3_AIC, mod4_AIC, mod5_AIC))
AIC_tbl$delta <- AIC_tbl[,2] - mod1_AIC 

print(AIC_tbl)
write.csv(AIC_tbl, file = 'Final_Project/figures/AIC_Table.csv')  # a lot here is redundant
export_table(AIC_tbl)     #for printing
kable(AIC_tbl, format = 'latex')


data$level[47:68] <- as.factor(c(1))    #Also redundant?  idk, if it aint broke...
data$level[1:50] <- as.factor(c(3))

###### Generating Predictions and Plotting ######

newdata <- data.frame(Species = rep(c("Masu", "Ito"), length.out = 100),
                          Fork_Length_mm = seq(min(ito_data$Fork_Length_mm),
                                               max(ito_data$Fork_Length_mm),
                                               length.out = 100),
                          Weight_g = seq(min(ito_data$Weight_g),
                                         max(ito_data$Weight_g),
                                         length.out = 100))
# I looked up methods of predicting data and this seemed to be a reasonable one
newdata_mod2 <- newdata
newdata$Temp_LOE <- predict(mod1, newdata, type = 'response')
newdata_mod2$Temp_LOE <- predict(mod2, newdata, type = 'response')

ggplot(data, aes(x = Fork_Length_mm, y = Temp_LOE, color = Species)) + # Model 1
  geom_point(size = 3.5) + 
  geom_line(data = newdata, aes(x = Fork_Length_mm, y = Temp_LOE)) + 
  scale_color_brewer(palette = 'Dark2') +
  theme_classic() +
  xlab(label = "Fork Length") + 
  ylab(label = 'Temperature(C)') +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

ggplot(data, aes(x = Fork_Length_mm, y = Temp_LOE, color = Species)) + # Model 2
  geom_point(size = 3.5) + 
  geom_line(data = newdata_mod2, aes(x = Fork_Length_mm, y = Temp_LOE)) + 
  scale_color_brewer(palette = 'Dark2') +
  theme_classic() +
  xlab(label = "Fork Length") + 
  ylab(label = 'Temperature(C)') +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

###### Plotting the species models vs. the data ######

ito_data <- data[data$Species == 'Ito',] #Tried making models for each ind. spp
ito_data <- ito_data[-c(1),]
ito_mod1 <- glm(Temp_LOE ~ Weight_g + Fork_Length_mm, data = ito_data)
masu_data <- data[data$Species == 'Masu',]
masu_mod1 <- glm(Temp_LOE ~ Weight_g + Fork_Length_mm, data = data)

autoplot(ito_mod1)
autoplot(masu_mod1)

ito_NLL <- logLik(ito_mod1)
masu_NLL <- logLik(masu_mod1)

ito_AIC <- 2*(-ito_NLL - 4)
masu_AIC <- 2*(-masu_NLL - 4)


# plotting said spp. models
ggplot(ito_data, aes(x = Fork_Length_mm, y = Temp_LOE)) + 
  geom_point() +
  geom_smooth(method = 'glm', se = F) +
  ylab(label = "Temp(C)") +
  xlab(label = "Fork Length") +
  theme_classic() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
  

ggplot(masu_data, aes(x = Fork_Length_mm, y = Temp_LOE)) + 
  geom_point() + 
  geom_smooth(method = 'glm', se = F) +
  ylab(label = "Temp(C)") +
  xlab(label = "Fork Length") +
  theme_classic() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
  
# Making dataframe for prediction data

ito_newdata <- data.frame(Species = 'Ito',
                          Fork_Length_mm = seq(min(ito_data$Fork_Length_mm),
                            max(ito_data$Fork_Length_mm),
                            length.out = 100),
                          Weight_g = seq(min(ito_data$Weight_g),
                                     max(ito_data$Weight_g),
                                     length.out = 100))

masu_newdata <- data.frame(Species = 'Masu',
                           Fork_Length_mm = seq(min(masu_data$Fork_Length_mm),
                                               max(masu_data$Fork_Length_mm),
                                               length.out = 100),
                          Weight_g = seq(min(masu_data$Weight_g),
                                         max(masu_data$Weight_g),
                                         length.out = 100))

ito_newdata$Temp_LOE <- predict(ito_mod1, ito_newdata, type = 'response')
masu_newdata$Temp_LOE <- predict(masu_mod1, masu_newdata, type = 'response')

ggplot(data, aes(x = Fork_Length_mm, y = Temp_LOE, color = Species)) +
  geom_point(size = 2.5) + 
  geom_line(data = ito_newdata, aes(x = Fork_Length_mm, y = Temp_LOE)) + 
  geom_line(data = masu_newdata, aes(x = Fork_Length_mm, y = Temp_LOE)) + 
  scale_color_brewer(palette = 'Dark2')



##### Combine & compare ######
# Potentially unnecessary now, but is no longer extrapolating data, so idk.
data_edit <- data %>%
  select(-c('Trial', "Time_LOE"))

newdata$level <- 1

masu <- seq(69, 168, 2)
ito <- seq(70, 168, 2)
obs_pred <- rbind(data_edit, newdata)
obs_pred$level[1:46] <- 3
obs_pred$level[47:68] <- 1
obs_pred$level[ito] <- 2
obs_pred$level[masu] <- 4
obs_pred$level <- factor(obs_pred$level,
                         labels = c( 'Ito Obs', 'Ito Pred',
                                    'Masu Obs', 'Masu Pred'))

ggplot(obs_pred, aes(x = level, y = Temp_LOE,
                      group = level, fill = level)) + 
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.3), 
             alpha = 0.5) +
  scale_fill_brewer(palette = 'Paired') + 
  #xlab(label = 'Obs V. Pred. for Masu & Ito') + 
  ylab(label = "Temp(C)") +
  theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

###### Original CTMax code used for t test and analysis
#library(dplyr)
#library(ggplot2)


setwd("C:/Users/heref/Documents/Project stuff/LucasProject")
ctm_data <- read.csv("CTM_data_analysis.csv")
?t.test
Masu_ctm <- ctm_data$Temp_LOE[ctm_data$Species == "Masu"]
Ito_ctm <- ctm_data$Temp_LOE[ctm_data$Species == "Ito"]
t.test(Masu_ctm, Ito_ctm)

#

ctmnochar <- ctm_data[-c(ctm_data$Species == "Char"),]
ctmnochar <- subset(ctm_data, ctm_data$Species == "Masu" | 
                      ctm_data$Species == "Ito" | ctm_data$Species == "Rainbow"|
                      ctm_data$Species == "Brown")

#if (ctmnochar$Species == "Masu") {
#  ctmnochar$value <- 1}




?boxplot
boxplot(ctmnochar$Temp_LOE ~ ctmnochar$Species)
ggplot(data = ctm_data) + 
  geom_point(mapping = aes(x = Trial, y = Temp_LOE,
                           color = Species, size = 2))


mean(ctmnochar$Temp_LOE[ctmnochar$Species == 'Ito'])
sd(ctmnochar$Temp_LOE[ctmnochar$Species == 'Ito'])
mean(ctmnochar$Temp_LOE[ctmnochar$Species == 'Masu'])
sd(ctmnochar$Temp_LOE[ctmnochar$Species == 'Masu'])



# A really basic boxplot.




# to reorder the species
ctmnochar$Species <- factor(ctmnochar$Species, levels = c("Ito", "Masu", 'Rainbow', "Brown"))


ggplot(ctmnochar, aes(x=Species, y=Temp_LOE, fill = Species)) + 
  geom_boxplot() + 
  labs(#title = 'Critical Thermal Maxima',
    x = 'Species',
    y = 'Temperature(Celsius)') +
  theme_minimal() +
  theme(text = element_text(size = 20),
        #axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) + 
  theme(legend.position = "none") 
#scale_fill_brewer(palette = 'Dark2')
#xlab("Species") + ylab("Temperature(Celsius)") +
#title('Critical Thermal Maxima')








###### Predicting weights and new data #######
# This is the code used for extrapolating for larger-bodied fish.
# I modified it to represent fish closer in size to what we tested, 
# but it is still defunct code.

ggplot(data, aes(x = Fork_Length_mm, y = Weight_g, color = Species)) + 
  geom_point(size = 3, alpha = 0.8) +
  scale_color_brewer(palette = 'Dark2') + 
  ylab(label = "Weight(g)") +
  xlab(label = 'Length(mm)') +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))


lengthmod1 <- glm(Fork_Length_mm ~ Weight_g + Species, data = data)
pred_length <- data.frame(Species = 'Masu',
                          Fork_Length_mm = NaN,
                          Weight_g = rnorm(100,100,20))


pred_length$Species[51:100] <- 'Ito'
pred_length$Fork_Length_mm <- predict(lengthmod1, pred_length)

pred_data <- pred_length
pred_data$Temp_LOE <- NaN


pred_data$Temp_LOE  <- predict(mod1, pred_data)

ggplot(pred_data, aes(x = Species, y = Temp_LOE,
                      group = Species, fill = Species)) + 
  
  geom_boxplot() +
  geom_point(position = 'jitter', alpha = 0.5) +
  scale_fill_brewer(palette = 'Dark2') + 
  ylab(label = "Temp(C)") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

ggplot(data, aes(x = Species, y = Temp_LOE,
                 group = Species, fill = Species)) + 
  geom_boxplot() +
  geom_point(position = 'jitter', alpha = 0.5) +
  scale_fill_brewer(palette = 'Dark2') + 
  ylab(label = "Temp(C)") +
  theme_classic() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())


pred_data$level[1:50] <- 4
pred_data$level[51:100] <- 2



