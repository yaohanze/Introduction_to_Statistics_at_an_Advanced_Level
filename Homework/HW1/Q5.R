quakes <- read.table(file = "~/Documents/STAT_201_B/Homework/HW1/fijiquakes.dat.txt", header = TRUE)
x <- quakes$mag
xsample <- seq(min(x), max(x), length = 100) # Take 100 samples for x.
Fhat <- apply(outer(x, xsample, "<="), 2, mean) # Calculate the ECDF.
n <- length(x)
# Calculate L(x) and U(x).
epsilon_n <- sqrt(log(2/0.05)/(2*n))
L <- sapply(Fhat, FUN=function(x) max(x - epsilon_n, 0))
U <- sapply(Fhat, FUN=function(x) min(x + epsilon_n, 1))
# Plot the ECDF and a 95% confidence interval for F.
plot(xsample, Fhat, xlab = "x", ylab = "Fhat(x)", type = "s", main = "Earthquake Magnitudes")
rug(x)
lines(xsample, L, lty = 2)
lines(xsample, U, lty = 2)
legend("bottomright", legend = c("Fhat(x)", "Confidence band"), lty = 1:2)