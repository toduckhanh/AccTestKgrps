#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double trapz_C(NumericVector fx, double hx) {
  int nn = fx.size();
  double res = 0.0;
  for(int i = 1; i < nn - 1; i++){
    res += fx[i];
  }
  double out = 0.0;
  out = 0.5*hx*(2*res + fx[0] + fx[nn - 1]);
  return out;
}
