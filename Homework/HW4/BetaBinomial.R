install.packages("MCMCpack")
library(MCMCpack)

## Specify the prior distribution

m <- 365  # Prior mean
v <- 1000 # Prior variance

a <- (m*m + 2*v)/v
b <- (m*m*m + m*v)/v

## Plot the prior density

x <- seq(200, 499, length = 300)
prior <- dinvgamma(x, a, b) # the prior pdf
plot(x, prior, type = "l", xlab = expression(lambda),
     ylab = expression(f(lambda)), main = "Prior density")

## The data

load("~/Documents/STAT_201_B/Homework/HW4/BerkeleyEarthquakes.RData") # Load the data of EQs in Berkeley.
head(earthquakes)
y <- earthquakes$Lag[-1] # Extract the waiting time between each EQs.
n <- length(y)

## Update the parameters to get the posterior distribution

a.star <- n + a
b.star <- n*mean(y) + b
posterior <- dinvgamma(x, a.star, b.star)

## Plot the prior and posterior densities

plot(x, posterior, type = "l", xlab = expression(lambda), ylab = expression(f(lambda)))
lines(x, prior, col = 2)
legend("topleft", lty = rep(1, 2), col = 1:2,
       legend = c("Posterior", "Prior"), bty = "n")

## Exercise: calculate the equal tail and HPD credible intervals
## (They will be close, since the posterior is nearly symmetric)
