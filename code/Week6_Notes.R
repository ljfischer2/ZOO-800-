#geom refers to geometry, or how you are graphing the data.

#ex:) ggplot(data, aes(y =, x = , color = , size = , shape = ,)) +
#       geom_point()

library(palmerpenguins)
summary(penguins)

#basic plotting of the data
plot1 <- ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()


#adding color x Species to plot
plot2 <- ggplot(data = penguins, aes(x = flipper_length_mm,
                                     y = body_mass_g,
                                     color = species)) +
  geom_point()
plot2

#facet wrapping
plot3 <- plot2 + facet_wrap(~species, ncol = 3)
plot3


#facet_grid works well for two variables you want to illustrate

#adding a trendline to predict linear models
plot4 <- plot3 + geom_smooth(method = 'lm')
plot4


#other plots
plot5 <- ggplot(data = penguins, aes(x = species,
                                     y = body_mass_g)) +
  geom_boxplot()
plot5


plot6 <- ggplot(data = penguins, aes(x = species,
                                     y = body_mass_g)) +
  geom_violin() + geom_jitter()
plot6


#changing the order ggplot plots in.  ggplot does inherent list by default
penguins$species <- factor(penguins$species, 
                           levels = c('Gentoo',
                                      'Adelie',
                                      'Chinstrap'))

#changing colors in ggplot
plot9 <- plot2 + scale_color_brewer(palette = "Set2")
plot9


#In-class code practice
peng <- penguins
peng_noNA <- peng %>%
  filter(sex %in% c('male', 'female'))

ggplot(peng_noNA, aes(x = sex, y = flipper_length_mm, fill = sex)) + 
  geom_violin() + 
  scale_fill_brewer(palette = "Dark2") + 
  geom_jitter(size = 1, width = 0.2,
              alpha = 0.5) + 
  facet_wrap(~species) + 
  labs(x = 'Sex',
       y = 'Flipper Length(mm)',
       fill = 'sex') + 
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)) + 
  theme_classic()




#shits nd giggles plotting in 3D

#install.packages('rgl')
library(xfun)
library(rgl)
#install.packages('xfun')

mycolors <- c('royalblue1', 'forestgreen', 'darkred')


plot3d( 
  x=peng_noNA$flipper_length_mm,
  y=peng_noNA$body_mass_g,
  z=peng_noNA$bill_depth_mm, 
  col = mycolors, 
  type = 's', 
  radius = .1,
  xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")
