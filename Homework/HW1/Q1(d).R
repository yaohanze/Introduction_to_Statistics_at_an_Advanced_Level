n <- 1:15
plot(x = n, y = 2/((n+1)*(n+2)), xlab = "n", ylab = "MSE", type = "l")
lines(x = n, y = 1/(3*n), lty = 2)
legend("topright", legend = c(expression(paste("MSE(",hat(theta)[n],")", sep = "")),
                              expression(paste("MSE(",tilde(theta)[n],")", sep = ""))), lty = 1:2)