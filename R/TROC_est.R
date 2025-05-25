
#' @export
TAUC_normal <- function(mu, sigma){
  a_k <- sigma[1]/sigma[-1]
  b_k <- (mu[-1] - mu[1])/sigma[-1]
  ff <- function(x, a_k, b_k){
    res <- mapply(function(aa, bb) pnorm(-x*aa + bb), aa = a_k, bb = b_k)
    return(dnorm(x)*apply(res, 1, prod))
  }
  integrate(ff, lower = -Inf, upper = Inf, a_k = a_k, b_k = b_k)$value
}

#' @export
ITFNR_normal <- function(mu, sigma){
  a_k <- sigma[1]/sigma[-1]
  b_k <- (mu[-1] - mu[1])/sigma[-1]
  ff <- function(x, a_k, b_k){
    res <- mapply(function(aa, bb) pnorm(x*aa - bb), aa = a_k, bb = b_k)
    return(dnorm(x)*apply(res, 1, prod))
  }
  integrate(ff, lower = -Inf, upper = Inf, a_k = a_k, b_k = b_k)$value
}

#' @export
TROC_normal <- function(mu, sigma, xlim){
  if (missing(xlim)) {
    X_lim <- mapply(function(a, b) a + c(-4, 4)*b, a = mu, b = sigma)
    xlim <- c(min(X_lim[1,]), max(X_lim[2,]))
  }
  xx <- seq(xlim[1], xlim[2], by = 0.001)
  TSp <- pnorm(xx, mean = mu[1], sd = sigma[1])
  res <- mapply(function(aa, bb) {
    pnorm(xx, mean = aa, sd = bb, lower.tail = FALSE)
  }, aa = mu[-1], bb = sigma[-1])
  TSe <- apply(res, 1, prod)
  return(list(xx = xx, TSp = TSp, TSe = TSe))
}

#' @export
f_min_normal <- function(mu, sigma, xlim){
  if (missing(xlim)) {
    X_lim <- mapply(function(a, b) a + c(-4, 4)*b, a = mu, b = sigma)
    xlim <- c(min(X_lim[1,]), max(X_lim[2,]))
  }
  xx <- seq(xlim[1], xlim[2], by = 0.001)
  ff <- mapply(function(a, b) dnorm(xx, mean = a, sd = b),
                a = mu, b = sigma, SIMPLIFY = TRUE)
  FF <- mapply(function(a, b) pnorm(xx, mean = a, sd = b),
                a = mu, b = sigma, SIMPLIFY = TRUE)
  ff_min <- numeric(length(xx))
  if (ncol(ff) == 2) {
    ff_min <- ff_min + ff[,1]*(1 - FF[,2]) + ff[,2]*(1 - FF[,1])
  } else{
    for (i in 1:ncol(ff)) {
      ff_min <- ff_min + ff[,i]*apply(1 - FF[,-i], 1, prod)
    }
  }
  return(list(x = xx, ff_min = ff_min, ff = ff))
}
