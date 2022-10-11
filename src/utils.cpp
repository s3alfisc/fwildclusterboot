#include <RcppArmadillo.h>
#define NDEBUG 
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

//' Get maximum number of threads on hardware for open mp support
//' @noRd
// [[Rcpp::export]]
int cpp_get_nb_threads(){
  // return omp_get_max_threads();
  return 1;
}
