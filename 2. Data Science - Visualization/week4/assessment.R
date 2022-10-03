options(digits = 3)   # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

################################ Question 2 ####################################
titanic %>% ggplot(aes(Age, y = ..count.., group = Sex, fill = Sex)) +
  geom_density(alpha = 0.2) + facet_grid(.~Sex)

# proportion between age 18 and 35
female <- ifelse(titanic$Sex == "female", 1, 0)
female_18_35 <- ifelse(titanic$Sex == "female" & between(titanic$Age, 18, 35) & !is.na(titanic$Age), 1, 0)
sum(female_18_35)/sum(female)

male <- ifelse(titanic$Sex == "male", 1, 0)
male_18_35 <- ifelse(titanic$Sex == "male" & between(titanic$Age, 18, 35) & !is.na(titanic$Age), 1, 0)
sum(male_18_35)/sum(male)

# proportion under age 17
female_17 <- ifelse(titanic$Sex == "female" & titanic$Age < 17 & !is.na(titanic$Age), 1, 0)
sum(female_17)/sum(female)

male_17 <- ifelse(titanic$Sex == "male" & titanic$Age < 17 & !is.na(titanic$Age), 1, 0)
sum(male_17)/sum(male)

################################ Question 3 ####################################
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) + geom_abline()

################################ Question 4 ####################################
titanic %>% ggplot(aes(Sex, fill = Survived)) + 
  geom_bar()

################################ Question 5 ####################################
titanic %>% ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, position = "stack")

################################ Question 6 ####################################
titanic %>% filter(!Fare == 0) %>% ggplot(aes(Fare, Survived, group = Survived)) +
  geom_boxplot() + scale_x_continuous(trans = "log2") +
  geom_jitter(alpha = 0.2)

################################ Question 7 ####################################
titanic %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar()

titanic %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = position_fill())

titanic %>% ggplot(aes(Survived, fill = Pclass)) + geom_bar(position = position_fill())

################################ Question 8 ####################################
titanic %>% ggplot(aes(Age, y = ..count.., fill=Survived)) + geom_density(position = "stack") +
  facet_grid(Sex~Pclass)
