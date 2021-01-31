// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#else
#define omp_get_thread_num() 0
#endif


//' Matrix Multiplication via Eigen
//' @param A A matrix. 
//' @param B A matrix.
//' @param n_cores Number of cores to be used for parallel matrix multiplication
//' @return A matrix
// [[Rcpp::export]]
SEXP eigenMatMult(Eigen::MatrixXd A, 
                  Eigen::MatrixXd B){
  
    Eigen::MatrixXd C = A * B;
    return Rcpp::wrap(C);
}


//' Matrix Multiplication via Eigen
//' @param A A matrix. 
//' @param B A matrix.
//' @param n_cores Number of cores to be used for parallel matrix multiplication
//' @return A matrix
// [[Rcpp::export]]
SEXP eigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A,
                     Eigen::Map<Eigen::MatrixXd> B){
  
  //Eigen::setNbThreads(n_cores);
  Eigen::MatrixXd C = A * B;
  return Rcpp::wrap(C);
}

// [[Rcpp::export]]
int cpp_get_nb_threads(){
  return omp_get_max_threads();
}
