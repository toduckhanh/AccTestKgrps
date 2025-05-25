
#' @export
vus_normal <- function(mu, sigma) {
  a <- sigma[2]/sigma[1]
  b <- (mu[1] - mu[2])/sigma[1]
  c <- sigma[2]/sigma[3]
  d <- (mu[3] - mu[2])/sigma[3]
  return(integrate(function(x, a, b, c, d) {
    pnorm(a*x - b)*pnorm(-c*x + d)*dnorm(x)
  }, a = a, b = b, c = c, d = d, lower = -Inf, upper = Inf)$value)
}

#' @export
UV_normal <- function(mu, sigma){
  vus_normal(mu = mu, sigma = sigma) +
    vus_normal(mu = mu[c(2, 1, 3)], sigma = sigma[c(2, 1, 3)])
}

#' @export
UV_emp <- function(X_list){
  ## X_list: list of X1, X2, ..., Xk
  all_X_d <- expand.grid(X_list)
  res <- mean(apply(all_X_d, 1, function(x) (x[1] < x[2]) * (x[1] < x[3])))
  return(res)
}
