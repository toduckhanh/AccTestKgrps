
X <- rnorm(50, 0, 2)
xx <- seq(-4, 4, by = 0.001)

system.time({
  fx_est <- f_hist(X, xx, length(X))
})

plot(xx, fx_est, type = "l")
lines(xx, dnorm(xx, mean = mean(X), sd = sd(X)), col = "blue")
