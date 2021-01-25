// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>

//' Matrix Multiplication via Eigen
//' @param A A matrix. 
//' @param B A matrix.
//' @return A matrix
// [[Rcpp::export]]
SEXP eigenMatMult(Eigen::MatrixXd A, Eigen::MatrixXd B){
  Eigen::MatrixXd C = A * B;
  
  return Rcpp::wrap(C);
}
