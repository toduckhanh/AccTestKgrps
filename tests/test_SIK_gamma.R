DIK_gamma <- function(shape, scale){
  ff <- function(x, shape, scale){
    ffj <- numeric(length(shape))
    for(i in 1:length(shape)){
      ffj[i] <- dgamma(x, shape = shape[i], scale = scale[i])
    }
    return(min(ffj[1], max(ffj[2:length(shape)])))
  }
  return(1 - integrate(Vectorize(ff, vectorize.args = "x"), lower = 0,
                       upper = Inf, shape = shape, scale = scale)$value)
}

DIK_gamma_log <- function(shape, scale){
  ff <- function(x, shape, scale){
    ffj <- numeric(length(shape))
    for(i in 1:length(shape)){
      if (x > 700) ffj[i] <- 0
      else ffj[i] <- exp(x)*dgamma(exp(x), shape = shape[i], scale = scale[i])
    }
    return(min(ffj[1], max(ffj[2:length(shape)])))
  }
  return(1 - integrate(Vectorize(ff, vectorize.args = "x"), lower = -Inf,
                       upper = Inf, shape = shape, scale = scale)$value)
}


DIK_gamma(shape = c(1, 1.75, 2), scale = c(2, 1.7, 1.5))
DIK_gamma_log(shape = c(1, 1.75, 2), scale = c(2, 1.7, 1.5))

X1 <- rgamma(50, shape = 1, scale = 2)
X2 <- rgamma(50, shape = 1.75, scale = 1.7)
X3 <- rgamma(50, shape = 2, scale = 1.5)

DIK_hist(X_list = list(X1, X2, X3))
DIK_hist(X_list = list(log(X1), log(X2), log(X3)))


X1 <- rgamma(50, shape = 1, scale = 2)
X2 <- rgamma(50, shape = 1.75, scale = 1.7)

DIK_hist(X_list = list(X1, X2))
DIK_hist(X_list = list(log(X1), log(X2)))

DIK_hist_log <- function(X_list, xlim, hx = 0.001){
  ## X_list: list of X1, X2, ..., Xk
  ## xlim: limits of range for all X, where the DIK needs to be estimated
  if (missing(xlim)) {
    X_lim <- sapply(X_list, range)
    xlim <- c(min(X_lim[1,]), max(X_lim[2,]))
  }
  xx <- seq(xlim[1], xlim[2], by = hx)
  xx_log <- log(xx)
  nx <- sapply(X_list, length)
  fx_hist_est <- mapply(FUN = function(x, nn) {
    f_hist(X = log(x), x_eval = xx_log, n = nn)
  }, x = X_list, nn = nx, SIMPLIFY = FALSE)
  ffk_max <- Reduce(pmax, fx_hist_est[-1])
  ff_ovl <- pmin(fx_hist_est[[1]], ffk_max)
  return(1 - pracma::trapz(x = xx_log, y = ff_ovl))
}

DIK_hist_log(X_list = list(X1, X2))


