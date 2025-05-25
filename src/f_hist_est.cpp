#include <Rcpp.h>
using namespace Rcpp;

static inline double ind_hist(double a, double b) {
  if(a > b) return 0;
  else return 1;
}


// [[Rcpp::export]]
NumericVector f_hist_C(NumericVector X, NumericVector x_eval, double h) {
  int nn1 = X.size(), nn2 = x_eval.size();
  NumericVector out(nn2);
  for(int i = 0; i < nn2; i++){
    double num = 0.0;
    for(int j = 0; j < nn1; j++){
        num += ind_hist(abs(X[j] - x_eval[i]), 0.5*h);
    }
    out[i] = num;
  }
  out = out/(nn1*h);
  return out;
}
