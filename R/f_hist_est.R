#' @importFrom Rcpp evalCpp
#' @useDynLib AccTestKgrps, .registration = TRUE

#' @export
f_hist <- function(X, x_eval, n){
  h <- 2*IQR(X)*n^(-1/3)
  fhat <- f_hist_C(X, x_eval, h)
  return(fhat)
}
