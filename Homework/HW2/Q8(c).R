precip <- read.csv("~/Documents/STAT_201_B/Homework/HW2/berkeleyprecip.csv", header = TRUE)
precip[precip == -99999] <- NA # Missing values
winter.precip <- precip$DEC + precip$JAN + precip$FEB
winter.precip <- winter.precip[!is.na(winter.precip)]
hist(winter.precip)

## Write a function for the negative log-likelihood

nll <- function(par, x, verbose = FALSE){
  alpha <- par[1]; beta <- par[2] # unpack
  ll <- sum(dgamma(x, alpha, beta, log = TRUE))
  if (verbose) print(c(par, -ll))
  return(-ll)
}

## Numerically minimize it

start <- c(alpha = 1, beta = 1) # starting values
eps <- 1e-10 # small value for lower bounds

op <- optim(par = start, fn = nll,
            lower = rep(eps, 2),
            x = winter.precip, verbose = TRUE)
op # This is a list; extract elements using $
mle <- op$par

## Examine the nll at the min

alpha.test <- cbind(seq(mle[1]/2, mle[1]*2, length = 100), mle[2])
nll.alpha <- apply(alpha.test, 1, nll, x = winter.precip, verbose = FALSE)
plot(alpha.test[,1], nll.alpha, type = "l")

beta.test <- cbind(mle[1], seq(mle[2]/2, mle[2]*2, length = 100))
nll.beta <- apply(beta.test, 1, nll, x = winter.precip, verbose = FALSE)
plot(beta.test[,2], nll.beta, type = "l")

## Do it again, this time estimating the Hessian

op <- optim(par = start, fn = nll,
            lower = rep(eps, 2), hessian = TRUE,
            x = winter.precip, verbose = TRUE)
mle <- op$par
J <- solve(op$hessian) # no negative - already working with negative ll
se.hat <- sqrt(diag(J))

lower <- mle - 2*se.hat
upper <- mle + 2*se.hat


