library(dbplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(nlme)
#### Bee Shit #####

setwd("C:/Users/heref/Documents/Classes_Fall_25/Stats/Week_4")
Bees <- read.table("Bees.txt", header = TRUE)
ggplot(data = Bees, aes(x = ))


var(Bees$Spobee)

lnspo <- vector()

i <- seq(from = 1, to = 24, by = 1)

for (i in 1:24) {
  lnspo <- append(lnspo, var(Bees$Spobee[Bees$Hive == i]), after = length(lnspo))
  
}

Bees$lnspo <- log(Bees$Spobee)

lnspo
lnspo <- log(lnspo)

Bees$lnspo[Bees$lnspo == -Inf] <- 0

model1 <- lm(lnspo ~ BeesN + Infection, data = Bees)
summary(model1)

m1plot <- residuals(model1, type = 'pearson')
Bees$resid <- m1plot

plot(Bees$resid ~ Bees$Hive)

plot(m1plot)


################################################
#Part 1 ####
library(AED); 

Beta <- vector(length = 9)
for (i in 1:9){
  Mi <- summary(lm(Richness ~ NAP,
                   subset = (Beach==i), data=RIKZ))
  Beta[i] <- Mi$coefficients[2, 1]}

Beta


fExposure9 <- factor(c(0, 0, 1, 1, 0, 1, 1, 0, 0))
tmp2 <- lm(Beta ~ fExposure9)


#randomintercept only model #####
library(nlme)
RIKZ$fBeach <- factor(RIKZ$Beach)
Mlme1 <- lme(Richness ~ NAP, random = ~ 1 | fBeach,
               data = RIKZ)
summary(Mlme1)

#fitted takes argument from lme + a level argument
#level = 0 implies fitted values from pop. model, level = 1 gives within level fitted values

F0 <- fitted(Mlme1, level = 0)
F1 <- fitted(Mlme1, level = 1)
I <- order(RIKZ$NAP); NAPs <- sort(RIKZ$NAP)
plot(NAPs, F0[I], lwd = 4, type = "l",
       ylim = c(0, 22), ylab = "Richness", xlab = "NAP")

for (i in 1:9){
  x1 <- RIKZ$NAP[RIKZ$Beach == i]
  y1 <- F1[RIKZ$Beach == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}
text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9)



# random intercept and slope model #####

Mlme2 <- lme(Richness ~ NAP,
             random = ~ 1 + NAP | fBeach, data = RIKZ)
summary(Mlme2)

# random effects model ####

Mlme3 <- lme(Richness ~ 1, random = ~1 | fBeach,
             data = RIKZ)
summary(Mlme3)


# Marginal Model + equivalent random int. model

M.mixed <- lme(Richness ~ NAP, random = ~ 1 | fBeach,
               method = "REML", data = RIKZ)

M.gls <- gls(Richness ~ NAP, method = "REML",
               correlation = corCompSymm(form = ~ 1 | fBeach),
               data = RIKZ)

summary(M.mixed)
summary(M.gls)


#Testing ML vs. REML ####
RIKZ$fExp <- RIKZ$Exposure
RIKZ$fExp[RIKZ$fExp == 8] <- 10
RIKZ$fExp <- factor(RIKZ$fExp, levels = c(10, 11))

M0.ML <- lme(Richness ~ NAP, data = RIKZ,
               random = ~ 1 | fBeach, method = "ML")

M0.REML <-lme(Richness ~ NAP, random = ~1 | fBeach,
              method = "REML", data = RIKZ)

M1.ML <- lme(Richness ~ NAP + fExp, data = RIKZ,
               random = ~ 1 | fBeach, method = "ML")

M1.REML <- lme(Richness ~ NAP + fExp, data = RIKZ,
                 random = ~ 1 | fBeach, method = "REML")


#Examples of poor model selection ####

Wrong1 <- gls(Richness ~ 1 + NAP, method = "REML", #No random, so gls function
              data = RIKZ)
Wrong2 <- lme(Richness ~ 1 + NAP, random = ~ 1|fBeach,   #random int. only
                method = "REML", data = RIKZ)
Wrong3 <- lme(Richness ~ 1 + NAP, method = "REML", # Random Int. and Slope
                random = ~ 1 + NAP | fBeach, data = RIKZ)
#Model Comparison, STEP 2
AIC(Wrong1, Wrong2, Wrong3)
anova(Wrong1, Wrong2, Wrong3) #there is aq problem in the ANOVA, testing on the boundary
# Our test is one-tailed, not two-tailed which we need, so we need to halve the p-value

0.5 * (1 - pchisq(12.720753, 1)) # p-val for Wrong1 v. Wrong2

0.5 * ((1 - pchisq(7.09, 1)) + (1 - pchisq(7.09, 2))) # p-val for Wrong2 v. Wrong3


#STEP3: search for optimal fixed for a given random
RIKZ$fExp <- RIKZ$Exposure
RIKZ$fExp[RIKZ$fExp == 8] <- 10
RIKZ$fExp <- factor(RIKZ$fExp, levels = c(10, 11))
lmc <- lmeControl(niterEM = 2200, msMaxIter = 2200)
Wrong4 <- lme(Richness ~ 1 + NAP * fExp,
                random = ~ 1 + NAP | fBeach,
                method = "REML", data = RIKZ)
anova(Wrong4)

# Testing third hypothesis using ML estimation

lmc <- lmeControl(niterEM = 5200, msMaxIter = 5200)
Wrong4A <- lme(Richness ~ 1 + NAP, method="ML",
                 control = lmc, data = RIKZ,
                 random = ~ 1 + NAP | fBeach)
Wrong4B <- lme(Richness ~ 1 + NAP + fExp,
                 random = ~ 1 + NAP | fBeach, method="ML",
                 data = RIKZ)
Wrong4C <- lme(Richness ~ 1 + NAP * fExp,
                 random = ~ 1 + NAP | fBeach, data = RIKZ,
                 method = "ML", control = lmc)
anova(Wrong4A, Wrong4B, Wrong4C)

#STEP4: Numerical output of optimal model
Wrong5 <- lme(Richness ~ 1+NAP,
              random = ~ 1 + NAP | fBeach,
              method = "REML", data = RIKZ)
summary(Wrong5)


#Example of Good Model Selection####
#Step 1: Test as many explanatory variables as possible
B1 <- gls(Richness ~ 1 + NAP * fExp,
          method = "REML", data = RIKZ)
B2 <- lme(Richness ~ 1 + NAP * fExp, data = RIKZ,
            random = ~ 1 | fBeach, method = "REML")
B3 <- lme(Richness ~ 1 + NAP * fExp, data = RIKZ,
            random = ~ 1 + NAP | fBeach, method="REML")
#Step 2: Find best fitting model

anova(B1,B2,B3) #B2 has the lowest AIC, so we use B2


#Step 3: Determine significance of all parameters

summary(B2) #All are significant, but NAP interaction with EXP is unconvincing,
#so we can drop it abd use fExp as its own variable

#Step 4: Final Optimal Model
B2 <- lme(Richness ~ 1 + NAP + fExp, data = RIKZ,
          random = ~ 1 | fBeach, method = "REML")

summary(B2)




# 10 step process  ####

#This library doesn't exist, so not worth running
#Step 1: Import, applies lin-reg model, and graphs
install.packages("remotes")
remotes::install_github("romunov/AED")
library(AED)
data(Owls)
M.lm <- lm(NegPerChick ~ SexParent * FoodTreatment +
               SexParent * ArrivalTime, data = Owls)
plot(M.lm, select = c(1))
#We saw heterogeneity along the horizontal axis, and tried plotting 
#residuals vs. different variables.

#there was no clear pattern, so the data was log-transformed (see below)
Owls$LogNeg <- log10(Owls$NegPerChick + 1)
M2.lm <- lm(LogNeg ~ SexParent * FoodTreatment +
                SexParent * ArrivalTime, data = Owls)
E <- rstandard(M2.lm)
boxplot(E ~ Nest, data = Owls, axes = FALSE,
        ylim = c(-3, 3))
abline(0,0); axis(2)
text(1:27, -2.5, levels(Owls$Nest), cex=0.75, srt=65)

#Step 2: fit model with GLS
library(nlme)
Form <- formula(LogNeg ~ SexParent * FoodTreatment +
                    SexParent * ArrivalTime)
M.gls <- gls(Form, data = Owls)

#Step 3: choose a variance structure
# not really sure about this step.  Honestly looks like every explanatory
# term individually and then a relationship between them.  IDK what this code means


#Step 4: Fitting the Model
M1.lme <- lme(Form, random = ~ 1 | Nest,
              method = "REML", data = Owls)

#Step 5: Compare new model with old model
anova(M.gls, M1.lme)
#likelihood test indicates the model with the random intercept is much better
#Use L statistic for paper publishing
#This was tested "on the boundary."  whatever that means


#Step6: Everything Okay?
#once again, checking for homogeneity.
#look again at residuals and see if it makes sense (even spread)
E2 <- resid(M1.lme, type = "normalized")
F2 <- fitted(M1.lme)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
MyYlab <- "Residuals"
plot(x = F2, y = E2, xlab = "Fitted values", ylab = MyYlab)
boxplot(E2 ~ SexParent, data = Owls,
          main = "Sex of parent", ylab = MyYlab)
boxplot(E2 ~ FoodTreatment, data = Owls,
        main = "Food treatment", ylab = MyYlab)
plot(x = Owls$ArrivalTime, y = E, ylab = MyYlab,
       main = "Arrival time", xlab = "Time (hours)")
par(op)

#Step 7 & Step 8: Optimal Fixed Structure
#three options are presented below: summary, anova, and a likelihood ratio test


#to determine optimal model for explanatory variables, summary(M1.lme)
summary(M1.lme)

#Neither interaction term is significant
#you shouldn't use anova(M1.lme) since it applies sequential testing
#But they use it here, so... what?  use it? don't?  what alternative?
anova(M1.lme)

#The bottom p-value is the same as that obtained from the summary command
#Our third option is Maximum likelihood.  we fit that here
M1.Full <- lme(Form, random = ~ 1 | Nest,
               method = "ML", data = Owls)
M1.A <- update(M1.Full, .~. -SexParent:FoodTreatment)
M1.B <- update(M1.Full, .~. -SexParent:ArrivalTime)
anova(M1.Full,M1.A)
anova(M1.Full,M1.B)
#update command here drops the the negative terms from the model
#we test again with another round of dropped terms,
#interaction, and food treatment
Form2 <-formula(LogNeg ~ SexParent + FoodTreatment +
                  SexParent * ArrivalTime)
M2.Full <- lme(Form2, random= ~ 1| Nest, method= "ML",
                 data = Owls)
M2.A <- update(M2.Full, .~. -FoodTreatment)
M2.B <- update(M2.Full, .~. -SexParent:ArrivalTime)
anova(M2.Full, M2.A)
anova(M2.Full, M2.B)
#interaction between sex and arrival time is not significant, so we omit this
#Finally, we have food treatment, sex, and arrival time.
#We create a new model droppin one of each term
Form3 <- formula(LogNeg ~ Sex-Parent + FoodTreatment +
                   ArrivalTime)
M3.Full <- lme(Form3, random= ~ 1 | Nest,
                 method = "ML", data = Owls)
M3.A <- update(M3.Full, .~. -FoodTreatment)
M3.B <- update(M3.Full, .~. -SexParent)
M3.C <- update(M3.Full, .~. -ArrivalTime)

anova(M3.Full, M3.A)
anova(M3.Full, M3.B)
anova(M3.Full, M3.C)

#The term sex of parent is not significant, so we omit it.
#Now we only have two terms left.  we drop each in turn
Form4 <- formula(LogNeg ~ FoodTreatment + ArrivalTime)
M4.Full <- lme(Form4, random= ~ 1 | Nest,
                 method = "ML", data = Owls)
M4.A <- update(M4.Full, .~. -FoodTreatment)
M4.B <- update(M4.Full, .~. -ArrivalTime)
anova(M4.Full, M4.A)
anova(M4.Full, M4.A)

#Both are significant, so we keep both.  this is the end of model selection

#Step 9: Refit REML and Validate the model
M5 <- lme(LogNeg ~ FoodTreatment + ArrivalTime,
          random= ~1 | Nest, method = "REML", data = Owls)
summary(M5)

#Because the values of the arrival time are not 0, and there is high correlation
#between the slope and intercept, we should try to 0 the arrival time.
Owls$CArrivalTime <- Owls$ArrivalTime -
mean(Owls$ArrivalTime)

#Step 10: Discussion?
#Step 10: Making an additive mixed model?
library(lattice)
xyplot(E2 ~ ArrivalTime | SexParent * FoodTreatment,
         data = Owls, ylab = "Residuals",
         xlab = "Arrival time (hours)",
         panel = function(x,y){
           panel.grid(h = -1, v = 2)
           panel.points(x, y, col = 1)
           panel.loess(x, y, span = 0.5, col = 1,lwd=2)})
#IN the plots, we look for a smoothed line.  There isn't, so we know the linear
#model is wrong

library(mgcv)
M6 <- gamm(LogNeg ~ FoodTreatment + s(ArrivalTime),
             random = list(Nest = ~ 1), data = Owls)
