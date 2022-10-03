library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

# Question 1
p_50f <- death_prob %>% filter(age == 50 & sex == "Female") %>% pull(prob) # 1a
p_50f

loss_if_death <- -150000
gain_if_alive <- 1150
expected_value <- p_50f * loss_if_death + (1-p_50f) * gain_if_alive # 1b
expected_value

std_error <- abs(loss_if_death - gain_if_alive) * sqrt(p_50f*(1-p_50f)) # 1c
std_error

n <- 1000
expected_profit <- n * expected_value # 1d
expected_profit

sum_error <- sqrt(n) * std_error # 1e
sum_error

pnorm(0, expected_profit, sum_error) # 1f

# Question 2
p_50m <- death_prob %>% filter(age == 50 & sex == "Male") %>% pull(prob) # 2a
p_50m

premium <- ((700000 / n) - p_50m * loss_if_death) / (1-p_50m) # 2b
premium

new_error <- abs(loss_if_death - premium) * sqrt(p_50m*(1-p_50m)) * sqrt(n) # 2c
new_error

pnorm(0, 700000, new_error) # 2d

# Question 3
p_new <- 0.015

expected_value <- n * (p_new * loss_if_death + (1-p_new) * gain_if_alive) # 3a
expected_value

std_error <- sqrt(n) * abs(loss_if_death - gain_if_alive) * sqrt(p_new*(1-p_new)) # 3b
std_error

pnorm(0, expected_value, std_error) # 3c

pnorm(-1000000, expected_value, std_error) # 3d

# Question 3e
p <- seq(.01, .03, .001) # 3e
expected <- n * (p * loss_if_death + (1-p) * gain_if_alive)
error <- sqrt(n) * abs(loss_if_death - gain_if_alive) * sqrt(p*(1-p))
index <- pnorm(0, expected, error) > 0.9
p*index

# Question 3f
p <- seq(.01, .03, .0025) # 3f
expected <- n * (p * loss_if_death + (1-p) * gain_if_alive)
error <- sqrt(n) * abs(loss_if_death - gain_if_alive) * sqrt(p*(1-p))
index <- pnorm(-1000000, expected, error) > 0.9
p*index

# Question 4a
p_loss <- 0.015
n <- 1000
set.seed(25, sample.kind = "Rounding")
samples <- sample(c(-150000, 1150), n, replace = TRUE, prob = c(p_loss, 1-p_loss))
sum(samples) / 10^6

# Question 4b
set.seed(27, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  samples <- sample(c(-150000, 1150), n, replace = TRUE, prob = c(p_loss, 1-p_loss))
  sum(samples) / 10^6
})
mean(S < -1)

# Question 5
n <- 1000
p <- 0.015
loss <- -150000
prob <- 0.05

# Question 5a
z <- qnorm(0.05)
x <- -loss * (n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))
x

profit_per_policy <- p*loss + x*(1-p) # 5b
profit_per_policy

expected_profit <- n * profit_per_policy # 5c

# Question 5d
set.seed(28, sample.kind = "Rounding")
B <- 10000
money <- replicate(B, {
  X <- sample(c(loss, x), n, replace = TRUE, prob = c(p, 1-p))
  sum(X)
})
mean(money < 0)

# Question 6
set.seed(29, sample.kind = "Rounding")
B <- 10000
simulated_data <- replicate(B, {
  p <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  X <- sample(c(loss, x), n, replace = TRUE, prob = c(p, 1-p))
  sum(X)
})

mean(simulated_data) # 6a
mean(simulated_data < 0) # 6b
mean(simulated_data < -1000000) # 6c