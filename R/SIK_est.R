#' @import stats

#' @export
SIK_normal <- function(mu, sigma){
  ff <- function(x, mu, sigma){
    ffj <- numeric(length(mu))
    for(i in 1:length(mu)){
      ffj[i] <- dnorm(x, mean = mu[i], sd = sigma[i])
    }
    return(min(ffj[1], max(ffj[2:length(mu)])))
  }
  1 - integrate(Vectorize(ff, vectorize.args = "x"), lower = -Inf, upper = Inf,
                mu = mu, sigma = sigma, rel.tol = 1e-9)$value
}

#' @export
area_SIK_normal <- function(mu, sigma, xlim){
  if (missing(xlim)) {
    X_lim <- mapply(function(a, b) a + c(-6, 6)*b, a = mu, b = sigma)
    xlim <- c(min(X_lim[1,]), max(X_lim[2,]))
  }
  xx <- seq(xlim[1], xlim[2], by = 0.001)
  ff <- mapply(function(a, b) dnorm(xx, mean = a, sd = b),
               a = mu, b = sigma, SIMPLIFY = TRUE)
  if (ncol(ff) == 2) max_ff <- ff[, 2]
  else max_ff <- apply(ff[, 2:ncol(ff)], 1, max)
  fx_ovl <- pmin(ff[, 1], max_ff)
  colnames(ff) <- paste("fx", 1:length(mu), sep = "")
  data_fx <- data.frame(xx = xx, ff, fx_ovl = fx_ovl)
  return(data_fx)
}

#' @export
AvROC_normal <- function(mu, sigma, prob = NULL, xlim){
  if (missing(xlim)) {
    X_lim <- mapply(function(a, b) a + c(-4, 4)*b, a = mu, b = sigma)
    xlim <- c(min(X_lim[1,]), max(X_lim[2,]))
  }
  xx <- seq(xlim[1], xlim[2], by = 0.001)
  AvSp <- pnorm(xx, mean = mu[1], sd = sigma[1])
  if (is.null(prob)) prob <- rep(1/(length(mu) - 1), length(mu) - 1)
  res <- mapply(function(aa, bb, cc) {
    cc*pnorm(xx, mean = aa, sd = bb)
  }, aa = mu[-1], bb = sigma[-1], cc = prob)
  AvSe <- 1 - rowSums(res)
  return(list(xx = xx, AvSp = AvSp, AvSe = AvSe))
}

#' @export
SIK_hist <- function(X_list, xlim, hx = 0.001){
  ## X_list: list of X1, X2, ..., Xk
  ## xlim: limits of range for all X, where the DIK needs to be estimated
  if (missing(xlim)) {
    X_lim <- sapply(X_list, range)
    xlim <- c(min(X_lim[1,]), max(X_lim[2,]))
  }
  xx <- seq(xlim[1], xlim[2], by = hx)
  nx <- sapply(X_list, length)
  fx_hist_est <- mapply(FUN = function(x, nn) {
    f_hist(X = x, x_eval = xx, n = nn)
  }, x = X_list, nn = nx, SIMPLIFY = FALSE)
  ffk_max <- Reduce(pmax, fx_hist_est[-1])
  ff_ovl <- pmin(fx_hist_est[[1]], ffk_max)
  return(1 - trapzr(fx = ff_ovl, hx = hx))
}


