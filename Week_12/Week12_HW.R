# HW 12
# Jaden N and Lucas F

# set working directory
# I do this manually

#install.packages("ggtext")
library(tidyverse)
library(lubridate)
library(ggtext)

BlackSeaBass <- read.csv("Week_12/BSB_tagging_data.csv")

BlackSeaBass <- BlackSeaBass %>%    #first sort into just the female bass
  mutate(SexChange = if_else(Sex_at_capture != Sex_at_recapture, "Y", "N"))

BlackSeaBassFemale <- BlackSeaBass[BlackSeaBass$Sex_at_capture =="F",]

#BlackSeaBassFemale <- BlackSeaBass[BlackSeaBass$Date_at_recapture =="F",]

#BlackSeaBassFemale <- BlackSeaBassFemale %>%
#  filter(month(Date_at_recapture) > 7)


### Objective 1 ####

#k = number of successes
#n = number of trials         This is for reference of what hte variables mean
#Shape1 = k + 1
#Shape2 = n – k + 1

k <- as.numeric(count(BlackSeaBassFemale[BlackSeaBassFemale$SexChange =="Y",]))
n <- as.numeric(count(BlackSeaBassFemale[BlackSeaBassFemale$Sex_at_capture =="F",]))

Shape1 <- k + 1
Shape2 <- n - k + 1

x <- seq(0, 1, length.out = 1000)

ProbDensityRuns <- 1:1000

y <- dbeta(x , Shape1, Shape2)

CombinedProbDensityResults <- data.frame(x = x, y = y)

ConfidenceInterval95 <- qbeta(c(0.025, 0.975), Shape1, Shape2)

ggplot(data=CombinedProbDensityResults, aes(x=x, y=y)) +
  geom_line() +   
  labs(x = "Probability", y = "Proabability Density") +
  geom_vline(xintercept = ConfidenceInterval95)

# 95% confidence interval x = 0.173 to 0.494


#### Objective 2 ####

BlackSeaBassFemale$SexChange <- as.factor(BlackSeaBassFemale$SexChange)

SexChangeModel <- glm(SexChange ~ Length_at_capture,data = BlackSeaBassFemale, family = binomial)
summary(SexChangeModel)

# the length of the female does not significantly influence the probability that the bass will change sexes
# p value 0.1122 

# for each mm of length at capture, the log odds of sex change increases by 0.04490 


# new column in df with probability for each individual to change sex between captures
BlackSeaBassFemale$ModelProb <- predict(SexChangeModel, BlackSeaBassFemale, type = "response")

caption <- "Modeled probability of sex change as a function of body length by
female black sea bass. Over two years, individuals were captured, length and
sex recorded, and released. At the end of the spawning season (May through July)
individuals were recaptured and their sex re-recorded. Although there was a 
positive trend, length was not a siginificant predictor of sex change 
probability (p=0.1122)."

ggplot(BlackSeaBassFemale, aes(x=Length_at_capture, y=ModelProb)) +
  geom_point() +
  geom_line(data = BlackSeaBassFemale, 
            aes(x = Length_at_capture,y = ModelProb)) + 
  labs(x = "Length at Capture (mm)", y = "Modeled Probability of Sex Change",
       caption = caption)  +
  theme(plot.caption = element_textbox_simple(size = 9, lineheight = 1))


