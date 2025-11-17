#You love feeding squirrels in your backyard. They need to eat nuts to survive,
# or they will slowly starve to death. Your neighbor’s mom also likes
# watching the squirrels, and she wants to know which species eats more nuts
# per day. She wants an answer by the end of the day.  Otherwise, she will send
# the grandkids to egg your car.
library(tidyverse)
library(ggfortify)

nuts <- read.csv('Week_11/SquirrelsEatNuts.csv')

mod1 <- lm(NutsConsumedPerDay ~ BodyMassPounds + species, data = nuts)
mod2 <- lm(NutsConsumedPerDay ~ BodyMassPounds, data = nuts)

autoplot(mod1)
autoplot(mod2)
summary(mod1) #Only Body mass was significant, so we can drop the species term.
summary(mod2) 

ggplot(nuts, aes(x = BodyMassPounds, y = NutsConsumedPerDay,
                 color = species)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

#There is no difference between the species, the predictor variable is the only
# significant term in the models.  Body mass is the only significant factor 
# in how many nuts a squirrel consumes per day.


#According to my partner, this is the correct interpretation.  There should be
# little to no difference between the groups.  The parameter estimates are 
# also relatively close, as the true parameters are bounded by the two lines.

nuts %>%
  mutate(model = predict(mod1)) %>%
  ggplot() +
  geom_point(aes(BodyMassPounds, NutsConsumedPerDay)) +
  geom_line(aes(BodyMassPounds, model))

