#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export()]]
arma::mat vecmult (arma::mat x, arma::mat y) {
  for(unsigned int j=0; j < x.size(); j++){
    x[j] = x[j] * y[j];
  }
  return(x);
}
