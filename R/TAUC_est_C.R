#' @importFrom Rcpp evalCpp
#' @importFrom data.table CJ
#' @importFrom matrixStats rowMins
#' @useDynLib AccTestKgrps, .registration = TRUE

#' @export
tauc <- function(x, y){
  res <- auc_1(x, y)
  return(res)
}

#' @export
TAUC_emp_2 <- function(X_list) {
  ## X_list: list of X1, X2, ..., Xk
  xx <- X_list[[1]]
  all_X_d <- as.matrix(CJ(X_list[[2]], X_list[[3]], X_list[[4]]))
  min_X_d <- rowMins(all_X_d)
  res <- tauc(xx, min_X_d)
  return(res)
}

#' @export
TAUC_emp_3 <- function(X_list) {
  ## X_list: list of X1, X2, ..., Xk
  xx1 <- X_list[[1]]
  xx2 <- X_list[[2]]
  xx3 <- X_list[[3]]
  xx4 <- X_list[[4]]
  res <- tauc_C(xx1, xx2, xx3, xx4)
  return(res)
}
