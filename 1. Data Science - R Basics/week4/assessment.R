library(dslabs)
data(heights)

# Question 1
sum(ifelse(heights$sex == "Female", 1, 2))

# Question 2
mean(ifelse(heights$height > 72, heights$height, 0))

# Question 3
inches_to_ft <- function(x) {
  x = x / 12
  x
}

inches_to_ft(144)
index <- (inches_to_ft(heights$height) < 5)
sum(index)

# Question 5
# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for(n in 1:m) {
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n
