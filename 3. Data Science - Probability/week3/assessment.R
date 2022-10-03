# Question 1
p_correct <- 0.2 # 1a
p_wrong <- 1 - p_correct
weightages <- c(1, -0.25)

e <- (p_correct*weightages[1]) + (p_wrong*weightages[2]) # 1b, c
e

se_1 <- abs(-0.25-1)*sqrt(p_correct*p_wrong)
se_44 <- se_1*sqrt(44) # 1d
se_44

1 - pnorm(8, e, se_44) # 1e

set.seed(21, sample.kind = "Rounding")
S <- replicate(10000, {
  X <- sample(weightages, 44, replace = TRUE, prob = c(p_correct, p_wrong))
  sum(X)
})
mean(S>=8) # 1e

# Question 2
p_correct <- 0.25
p_wrong <- 1 - p_correct
weightages <- c(1, 0)

e <- (p_correct*weightages[1]) + (p_wrong*weightages[2]) # 1b, c
e * 44 # 2a

# Question 2b
p <- seq(0.25, 0.95, 0.05)
new_e <- (p * weightages[1])
new_sd <- 1 * sqrt(p*(1-p))

means <- 44 * new_e
sds <- sqrt(44) * new_sd
index <- (1 - pnorm(35, means, sds)) > 0.8
p * index

# Question 3
prob_win <- 5/38
prob_lose <- 1 - prob_win
n <- 500

expected_value <- ((prob_win * 6) + (prob_lose * -1)) # 3a
expected_value

std_error <- abs(-1-6) * sqrt(prob_win * prob_lose) # 3b
std_error

avg_expected <- expected_value # 3c
avg_expected

avg_error <- std_error / sqrt(n) # 3d
avg_error

total_expected <- n * expected_value # 3e
total_expected

total_error <- sqrt(n) * std_error # 3f
total_error

pnorm(0, total_expected, total_error) # 3g