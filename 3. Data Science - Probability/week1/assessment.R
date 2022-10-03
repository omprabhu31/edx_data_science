library(gtools)
library(tidyverse)
options(digits = 3)

# Question 1a-1c
nrow(permutations(8, 3)) #1a
nrow(permutations(3, 3)) #1b
nrow(permutations(3, 3))/nrow(permutations(8, 3)) #1c

# Question 1d
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", 
             "France", "South Africa")
set.seed(1)
B <- 10000
results <- replicate(B, {
  all_jamaica <- sample(runners, 3)
  (all_jamaica[1] %in% "Jamaica") & (all_jamaica[2] %in% "Jamaica") &
    (all_jamaica[3] %in% "Jamaica")
})
mean(results)

# Question 2
nrow(combinations(6, 1))*nrow(combinations(6, 2))*nrow(combinations(2, 1)) #2a
nrow(combinations(6, 1))*nrow(combinations(6, 2))*nrow(combinations(3, 1)) #2b
nrow(combinations(6, 1))*nrow(combinations(6, 3))*nrow(combinations(3, 1)) #2c

# Question 2d
n = seq(1,12)
num_entrees <- function(n) {
  nrow(combinations(n, 1))*nrow(combinations(6, 2))*nrow(combinations(3, 1))
}
sapply(n, num_entrees)

# Question 2e
n = seq(2,12)
num_sides <- function(n) {
  nrow(combinations(6, 1))*nrow(combinations(n, 2))*nrow(combinations(3, 1))
}
sapply(n, num_sides)

# Question 3
data(esoph)
nrow(esoph) #3a

all_cases <- sum(esoph$ncases) #3b
all_cases

all_controls <- sum(esoph$ncontrols) #3c
all_controls

# Question 4
esoph
esoph %>% filter(alcgp == "120+") %>% 
  summarize(sum(ncases)/(sum(ncases)+sum(ncontrols))) #4a

esoph %>% filter(alcgp == "0-39g/day") %>% 
  summarize(sum(ncases)/(sum(ncases)+sum(ncontrols))) #4b
cases <- esoph %>% filter(tobgp != "0-9g/day" & ncases != 0) %>% 
  summarize(cases = sum(ncases)) %>% pull(cases) #4c
cases / sum(esoph$ncases)

controls <- esoph %>% filter(tobgp != "0-9g/day" & ncontrols != 0) %>% 
  summarize(controls = sum(ncontrols)) %>% pull(controls) #4d
controls / sum(esoph$ncontrols)

# Question 5
cases <- esoph %>% filter(alcgp == "120+" & ncases != 0) %>% 
  summarize(cases = sum(ncases)) %>% pull(cases) #5a
p_alc <- cases / sum(esoph$ncases)

cases <- esoph %>% filter(tobgp == "30+" & ncases != 0) %>% 
  summarize(cases = sum(ncases)) %>% pull(cases) #5b
p_tob <- cases / sum(esoph$ncases)

cases <- esoph %>% filter(tobgp == "30+" & alcgp == "120+" & ncases != 0) %>% 
  summarize(cases = sum(ncases)) %>% pull(cases) #5c
p_tob_and_alc <- cases / sum(esoph$ncases)

p_tob_or_alc <- p_alc + p_tob - p_tob_and_alc #5d

# Question 6
controls <- esoph %>% filter(alcgp == "120+" & ncontrols != 0) %>% 
  summarize(controls = sum(ncontrols)) %>% pull(controls) #6a
p_alc <- controls / sum(esoph$ncontrols)

p_case <- esoph %>% filter(alcgp == "120+" & ncases != 0) %>% 
  summarize(p = sum(ncases)/sum(esoph$ncases)) %>% pull(p) 
p_control <- esoph %>% filter(alcgp == "120+" & ncontrols != 0) %>% 
  summarize(p = sum(ncontrols)/sum(esoph$ncontrols)) %>% pull(p)
p_case/p_control #6b

controls <- esoph %>% filter(tobgp == "30+" & ncontrols != 0) %>% 
  summarize(controls = sum(ncontrols)) %>% pull(controls) #6c
p_tob <- controls / sum(esoph$ncontrols)

controls <- esoph %>% filter(tobgp == "30+" & alcgp == "120+" & ncontrols != 0) %>% 
  summarize(controls = sum(ncontrols)) %>% pull(controls) #6d
p_tob_and_alc <- controls / sum(esoph$ncontrols)

p_tob_or_alc <- p_alc + p_tob - p_tob_and_alc #6e

p_case <- esoph %>% filter((alcgp == "120+" | tobgp == "30+") & ncases != 0) %>% 
  summarize(p = sum(ncases)/sum(esoph$ncases)) %>% pull(p) 
p_control <- esoph %>% filter((alcgp == "120+" | tobgp == "30+") & ncontrols != 0) %>% 
  summarize(p = sum(ncontrols)/sum(esoph$ncontrols)) %>% pull(p)
p_case/p_control #6f