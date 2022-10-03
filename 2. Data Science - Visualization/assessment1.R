library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

# Question 1
head(stars)
mean(stars$magnitude)
sd(stars$magnitude)

# Question 2
stars %>% ggplot(aes(magnitude)) + geom_density()

# Question 3
stars %>% ggplot(aes(temp)) + geom_density()

# Question 4
stars %>% ggplot(aes(temp, magnitude)) + geom_point()

# Question 5
stars %>% ggplot(aes(log10(temp), magnitude)) + geom_point() + scale_y_reverse() +
  scale_x_reverse()

# Question 8
library(ggrepel)
stars %>% ggplot(aes(temp, magnitude, label = star)) + geom_point(aes(col = type))

