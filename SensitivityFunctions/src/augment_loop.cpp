#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

arma::mat matprod (arma::mat x);
double runifcpp (double lower, double upper);
arma::mat subset (arma::mat x, int row, int column_min, int column_max);
arma::mat vecmult (arma::mat x, arma::mat y);


// [[Rcpp::export()]]
arma::mat augment_loop (arma::mat x, int n, double buff) {
  arma::mat tmp = x;
  int rn = n+1;
  for (int j=1; j<n; j++){
    arma::mat B_up = x;
    B_up(n,n) = 0;
    arma::mat B_lo = x;
    B_lo(n,n) = 0;

    arma::mat cholrow = subset(x,rn,1,j);
    double sumsq = std::inner_product(cholrow.begin(),cholrow.end(),cholrow.begin(),0.0);
    B_up(n,j) = sqrt(1-buff-sumsq);

    B_lo(n,j) = -sqrt(1-buff-sumsq);

    arma::mat C_up = B_up * B_up.t();
    double upper = C_up(n,j);

    arma::mat C_lo = B_lo * B_lo.t();
    double lower = C_lo(n,j);

    double cor = runifcpp(lower,upper);

    arma::vec vect = subset(x,j+1,1,j);
    arma::mat tmp = vecmult(cholrow,vect);
    x(n,j) = 1/x(j,j) * (cor - accu(tmp));

    arma::mat cholrow2 = subset(x,rn,1,n);
    double sumsq2 = std::inner_product(cholrow2.begin(),cholrow2.end(),cholrow2.begin(),0.0);
    x(n,n) = sqrt(1 - sumsq2);
  }
  return(x);
}
