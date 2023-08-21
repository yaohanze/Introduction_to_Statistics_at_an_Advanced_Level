library(boot)
clouds <- read.table("~/Documents/STAT_201_B/Homework/HW1/clouds.dat", header = TRUE)
seeded <- clouds$Seeded
unseeded <- clouds$Unseeded
theta_hat <- median(seeded) - median(unseeded)
sample_median <- function(x, d){return (median(x[d]))}
N <- 100
seeded_boot <- boot(seeded, sample_median, N)
unseeded_boot <- boot(unseeded, sample_median, N)
se_hat = sqrt(var(seeded_boot$t) + var(unseeded_boot$t))
CI <- c(theta_hat + qnorm(0.025) * se_hat, theta_hat - qnorm(0.025) * se_hat)
print(CI)