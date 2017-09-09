#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export()]]
arma::mat matprod (arma::mat x) {
  arma::mat out = x * x.t();
  return(out);
}
