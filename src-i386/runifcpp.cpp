#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export()]]
double runifcpp (double lower, double upper) {
  double out = R::runif(lower,upper);
  return(out);
}
