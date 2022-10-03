# Question 1
set.seed(16, sample.kind = "Rounding")

act_avg <- 20.9
act_sd <- 5.7
act_scores <- rnorm(10000, act_avg, act_sd)

mean(act_scores) # 1a
sd(act_scores) # 1b
sum(act_scores >= 36) # 1c
mean(act_scores > 30) # 1d
mean(act_scores <= 10) # 1e

# Question 2
x <- seq(1, 36)
f_x <- dnorm(x, act_avg, act_sd)

lines(x, f_x)

# Question 3
z_score <- (act_scores - mean(act_scores))/sd(act_scores)

mean(z_score >= 2) # 3a
mean(act_scores) + 2*sd(act_scores) # 3b
qnorm(0.975, act_avg, act_sd) #3c

# Question 4
F <- function(value) pnorm(value, act_avg, act_sd)
scores <- seq(1, 36)

scores[F(scores) >= 0.95] # 4a
qnorm(0.95, act_avg, act_sd) # 4b

# Question 4c
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)

sample_quantiles[sample_quantiles <= 26]

# Question 4d
theoretical_quantiles <- qnorm(p, act_avg, act_sd)
params <- data.frame(sample_quantiles, theoretical_quantiles)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()
