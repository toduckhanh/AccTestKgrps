#' @importFrom Rcpp evalCpp
#' @useDynLib AccTestKgrps, .registration = TRUE

#' @export
trapzr <- function(fx, hx){
  res <- trapz_C(fx, hx)
  return(res)
}
