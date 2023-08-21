clouds <- read.table("~/Documents/STAT_201_B/Homework/HW1/clouds.dat", header = TRUE)
seeded <- clouds$Seeded
unseeded <- clouds$Unseeded
theta_hat <- median(seeded) - median(unseeded)
bootstrap <- function(data, B) {
  resample <- lapply(1 : B, function(i) sample(data, replace = TRUE))
  sample_median <- sapply(resample, FUN = function(x) median(x))
  variance <- var(sample_median)
  list(resample = resample, median = sample_median, variance = variance)
}
seeded_boot <- bootstrap(seeded, 10000)
unseeded_boot <- bootstrap(unseeded, 10000)
se_hat = sqrt(seeded_boot$variance + unseeded_boot$variance)
CI <- c(theta_hat + qnorm(0.025)*se_hat, theta_hat - qnorm(0.025)*se_hat)
print(CI)