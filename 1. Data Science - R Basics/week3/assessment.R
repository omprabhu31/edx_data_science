library(data.table)
library(tidyverse)
library(dplyr)
library(dslabs)
data(heights)
options(digits = 3)

avg <- heights %>% summarize(average = mean(height)) %>% pull()
ind <- heights$height > avg
sum(ind)

fem_height <- heights %>% filter(sex == "Female")
ind <- fem_height$height > avg
sum(ind)

gender <- heights$sex
men <- gender == "Male"
women <- gender == "Female"
sum(women) / (sum(men) + sum(women))

heights <- setDT(heights)
min_height <- heights[, .(minimum = min(height))]
index <- match(min_height, heights$height)
heights$sex[index]

max_height <- heights[, .(maximum = max(height))]
x <- seq(as.numeric(min_height), as.numeric(max_height))
x

sum(!x %in% heights$height)

heights2 <- mutate(heights, ht_cm = height * 2.54)
heights2[18]
heights2 %>% summarize(avg_cm = mean(ht_cm))

females <- heights2 %>% filter(sex == "Female") %>% data.frame()
females %>% summarize(avg = mean(ht_cm))

data(olive)
head(olive)
plot(olive$palmitic, olive$palmitoleic)

hist(olive$eicosenoic)

boxplot(olive$palmitic~olive$region)