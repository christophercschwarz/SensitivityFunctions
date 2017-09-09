#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export()]]
arma::mat subset (arma::mat x, int row, int column_min, int column_max) {
  arma::mat submat = x.row(row-1);
  NumericVector out(column_max-column_min+1);
  for(int j = (column_min-1); j < column_max; j++){
    out[j] = submat[j];
  }
  return(out);
}
