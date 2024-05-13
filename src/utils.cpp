#include <RcppArmadillo.h>
#include <RcppEigen.h>
#ifdef _OPENMP
  #include <omp.h>
#else
  #define omp_get_max_threads() 0
  #define EIGEN_DONT_PARALLELIZE
#endif

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]

using namespace Rcpp;


//' Matrix Multiplication via Eigen
//' @param A A matrix.
//' @param B A matrix.
//' @param nthreads Integer. Number of threads to use for matrix multiplication.
//' @return A matrix
//' @noRd
// [[Rcpp::export]]
SEXP eigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A,
                     Eigen::Map<Eigen::MatrixXd> B,
                     int nthreads){
  
  Eigen::setNbThreads(nthreads);
  //omp_set_num_threads(nthreads);
  Eigen::MatrixXd C = A * B;
  return Rcpp::wrap(C);
}

// code by Stéphane Laurent from the EigenR
// package, published under GPL-3
// https://github.com/stla/EigenR/blob/1ffb3091d4bfa6fb0cd5fbfdaa1cc06f7bb6ea69/src/Inverse.cpp

// template <typename Number>
// Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> pseudoInverse(
//     const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
//   Eigen::CompleteOrthogonalDecomposition<
//     Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
//     cod(M);
//   return cod.pseudoInverse();
// }

//' Moore-Penrose Pseudo Inverses via Eigen
//' @param A a matrix
//' @return A matrix. Pseudo-Inverse of A.
//' @noRd

// [[Rcpp::export]]
Eigen::MatrixXd eigen_pinv(const Eigen::MatrixXd& A) {
  return A.completeOrthogonalDecomposition().pseudoInverse();
}


//' Get maximum number of threads on hardware for open mp support
//' @noRd
// [[Rcpp::export]]
int cpp_get_nb_threads(){
  
  return omp_get_max_threads();
}
