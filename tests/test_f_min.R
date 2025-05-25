
mu <- c(0.8, 0.9, 1)
sigma <- c(1, 1, 1)

ff_min_est <- f_min_normal(mu = mu, sigma = sigma)

plot(ff_min_est$x, ff_min_est$ff_min, type = "l")
lines(ff_min_est$x, ff_min_est$ff[,1], col = "blue")
lines(ff_min_est$x, ff_min_est$ff[,2], lty = 2)
lines(ff_min_est$x, ff_min_est$ff[,3], lty = 2, col = "red")

##
mu <- c(0.8, 0.9, 1)
sigma <- c(1, 1.2, 0.5)

ff_min_est <- f_min_normal(mu = mu, sigma = sigma)

plot(ff_min_est$x, ff_min_est$ff_min, type = "l", ylim = c(0, 1))
lines(ff_min_est$x, ff_min_est$ff[,1], col = "blue")
lines(ff_min_est$x, ff_min_est$ff[,2], lty = 2)
lines(ff_min_est$x, ff_min_est$ff[,3], lty = 2, col = "red")

##
mu <- c(0.8, 1, 2.2, 2.2)
sigma <- c(1, 1, 1, 1)

ff_min_est <- f_min_normal(mu = mu[-1], sigma = sigma[-1], xlim = c(-3, 6))
ffx1 <- dnorm(ff_min_est$x, mean = mu[1], sd = sigma[1])

plot(ff_min_est$x, ff_min_est$ff_min, type = "l", ylim = c(0, 1.2),
     col = "blue")
lines(ff_min_est$x, ffx1, col = "red")
lines(ff_min_est$x, ff_min_est$ff[,1], lty = 2, col = "gray60")
lines(ff_min_est$x, ff_min_est$ff[,2], lty = 2, col = "gray60")
lines(ff_min_est$x, ff_min_est$ff[,3], lty = 2, col = "gray60")

