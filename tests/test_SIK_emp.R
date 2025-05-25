X1 <- rnorm(50, 0, 1)
X2 <- rnorm(30, 1.5, 1.05)
X3 <- rnorm(25, 1.5, 1.25)

system.time({
  out <- SIK_hist(X_list = list(X1, X2, X3))
})

out

system.time({
  out_simu <- sapply(1:5000, function(i){
    X1 <- rnorm(50, 0, 1)
    X2 <- rnorm(100, 1.5, 1.05)
    X3 <- rnorm(100, 1.5, 1.25)
    return(SIK_hist(X_list = list(X1, X2, X3)))
  })
})

mean(out_simu)

out2 <- SIK_normal(mu = c(0, 1.5, 1.5), sigma = c(1, 1.05, 1.25))

mean(out_simu) - out2

(mean(out_simu) - out2)/out2 * 100

hist(out_simu)
abline(v = out2, lty = 2)

coverage <- function(x, sd_x, val, ci_lev){
  mean(abs(x - val)/sd_x <= qnorm((1 + ci_lev)/2))
}

coverage(x = out_simu, sd_x = sd(out_simu), val = out2, ci_lev = 0.9)
coverage(x = out_simu, sd_x = sd(out_simu), val = out2, ci_lev = 0.95)
coverage(x = out_simu, sd_x = sd(out_simu), val = out2, ci_lev = 0.99)


### NOT RUN
X1 <- rnorm(50, 0, 1)
X2 <- rnorm(30, 1, 1)
X3 <- rnorm(25, 1.2, 1)

X_list <- list(X1, X2, X3)
X_lim <- sapply(X_list, range)
xlim <- c(min(X_lim[1,]), max(X_lim[2,]))

hx <- 0.001
xx <- seq(xlim[1], xlim[2], by = hx)
nx <- sapply(X_list, length)

system.time({
  fx_hist_est <- mapply(FUN = function(x, nn) {
    f_hist(X = x, x_eval = xx, n = nn)
  }, x = X_list, nn = nx)
})

ffk_max <- apply(fx_hist_est[,-1], 1, max)
ff_ovl <- pmin(fx_hist_est[, 1], ffk_max)

system.time({
  1 - pracma::trapz(x = xx, y = ff_ovl)
})

system.time({
  1 - 0.5*hx*(ff_ovl[1] + 2*sum(ff_ovl[2:(length(ff_ovl) - 1)]) + ff_ovl[length(ff_ovl)])
})

system.time({
  out_simu2 <- sapply(1:1000, function(i){
    X1 <- rnorm(50, 0, 1)
    X2 <- rnorm(75, 1, 1)
    X3 <- rnorm(75, 1.2, 1)
    X_list <- list(X1, X2, X3)
    X_lim <- sapply(X_list, range)
    xlim <- c(min(X_lim[1,]), max(X_lim[2,]))
    hx <- 0.001
    xx <- seq(xlim[1], xlim[2], by = hx)
    nx <- sapply(X_list, length)
    fx_hist_est <- mapply(FUN = function(x, nn) {
      f_hist(X = x, x_eval = xx, n = nn)
    }, x = X_list, nn = nx, SIMPLIFY = FALSE)
    ffk_max <- Reduce(pmax, fx_hist_est[-1]) # pmax(fx_hist_est[, 2], fx_hist_est[, 3])
    ff_ovl <- pmin(fx_hist_est[[1]], ffk_max)
    return(1 - pracma::trapz(x = xx, y = ff_ovl))
    # return(1 - 0.5*hx*(ff_ovl[1] + 2*sum(ff_ovl[2:(length(ff_ovl) - 1)]) + ff_ovl[length(ff_ovl)]))
  })
})

mean(out_simu2)


