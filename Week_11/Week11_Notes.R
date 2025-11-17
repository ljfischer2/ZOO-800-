#When extending linear model to categorical, use dummy variables
#di, where 0 is not level i, and 1 is level i
# created automatically when factor is included in lm, or can be specified
# using factor()
#compared to a reference level in lm() summary
    #mean of reference: 114
    #mean of treatment: 124

#Combining categorical & continuous variables Analysis of Covariance (ANCOVA)
# understand the effect of treatmenr while controlling for a nuisance covariate

                  #use this to understand treatment type?

#In Ecology, the categorical may be the nuisance variable.
#
#In ANCOVA plot with two intersecting lines:
# lm(Y ~ Xcon * Xfac)
# OR lm(Y~Xcon + Xfac + Xcon:Xfac)

#with no factor,
#lm(Y~Xcon + Xfac)


#One factor, multiple levels - ANOVA
  #also fits linear model framework

#(emmeans) package allows us to extract means and CIs from ANOVAs

#ANOVA assumptions:
  #Normality, Independence, etc.

#When we have only one value for each categorical, we cannot model it
  #If we can drop categorical and chang e it to contimuous, so that we could 
 # model it.


# In-Class Exercises ################################

#setup of our original data
#par(mfrow = c(1,1))

set.seed(71)
dfx <- data.frame(Mouth_Length = c(rnorm(106,270,10),rnorm(107, 250, 10)),
  Feed_Rate = c(rnorm(106, 75, 10),rnorm(107, 80, 10)))
dfx$Mouth_Type <- "Round"
dfx$Mouth_Type[107:213] <- "Snub"

ggplot(dfx, aes(x = Mouth_Length, y = Feed_Rate)) + 
  geom_point()

mod1 <- lm(Feed_Rate~Mouth_Length*Mouth_Type, data = dfx)
mod2 <- lm(Feed_Rate~Mouth_Length, data = dfx)

autoplot(mod1)
autoplot(mod2)
summary(mod1)
summary(mod2)

write.csv(x = dfx, file = "Week_11/trout_feeding.csv", )


#Congrats, you are in charge of a fish hatchery.  You have  phenotypes of true-
# breeding brook trout.  Your fishermen want trout stocked that are easier to 
# catch, because they suck.  Besides there are stupid naming conventions that
# are usual for strains of trout, you refer to them as snub-nose and round-nose
# trout.  Find the hungrier trout (those that have a higher feeding rate)

#Do brook trout vary in feeding rate based on the length of their mouth?