#In-class code practice

library(tidyverse)
library(palmerpenguins)
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



