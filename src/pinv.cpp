#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat pinv(const arma::mat& X) {
  return arma::pinv(X);
}