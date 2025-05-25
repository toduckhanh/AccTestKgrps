#include <Rcpp.h>
using namespace Rcpp;

static inline double indauc(double a, double b) {
  if(a < b) return 1.0;
  else return 0.0;
}

// [[Rcpp::export]]
double tauc_C(NumericVector tt1, NumericVector tt2, NumericVector tt3,
              NumericVector tt4){
  int nn1 = tt1.size(), nn2 = tt2.size(), nn3 = tt3.size(), nn4 = tt4.size();
  double num = 0.0;
  double temp1, temp2;
  for(int i = 0; i < nn1; i++){
    for(int j = 0; j < nn2; j++){
      for(int k = 0; k < nn3; k++){
        temp1 = std::min(tt2[j], tt3[k]);
        for(int s = 0; s < nn4; s++){
          temp2 = std::min(temp1, tt4[s]);
          num += indauc(tt1[i], temp2);
        }
      }
    }
  }
  double out = num/(nn1*nn2*nn3*nn4);
  return out;
}
