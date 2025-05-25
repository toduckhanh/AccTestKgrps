#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double auc_2(NumericMatrix x) {
  int n = x.ncol();
  double res = 0;
  for(int i = 0; i < n; i++){
    res += x(0,i) < x(1,i);
  }
  return res/n;
}

// [[Rcpp::export]]
double auc_1(NumericVector x, NumericVector y) {
  int n1 = x.size(), n2 = y.size();
  double res = 0;
  for(int i = 0; i < n1; i++){
    for(int j = 0; j < n2; j++){
      res += x[i] < y[j];
    }
  }
  return res/n1/n2;
}

// [[Rcpp::export]]
double auc_3(NumericMatrix x) {
  double res = mean(x(0,_) < x(1,_));
  return res;
}


