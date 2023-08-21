lambda0 <- 1
n <- 20
alpha <- 0.05
B <- 10000 # Run Wald test 10000 times.
W <- rep(0, B)
for (i in 1:B) {X <- rpois(n, lambda0) # Generate n random variables iid poisson distribution.
  W[i] <- (mean(X) - lambda0) / sqrt(mean(X) / n)
}
num_rejection <- sum(abs(W) > qnorm(1 - alpha / 2))
type_one_error_rate <- num_rejection / B