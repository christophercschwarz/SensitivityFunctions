#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export()]]
arma::mat cholcpp (arma::mat x) {
  arma::mat out = chol(x);
  return(out.t());
}
