// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
#ifdef _OPENMP
  #include <omp.h>
#else
  #define omp_get_max_threads() 0
  #define EIGEN_DONT_PARALLELIZE
#endif


// //' Matrix Multiplication via Eigen
// //' @param A A matrix. 
// //' @param B A matrix.
// //' @param nthreads Integer. Number of threads to use for matrix multiplication.
// //' @return A matrix
// // [[Rcpp::export]]
// SEXP eigenMatMult(Eigen::MatrixXd A, 
//                   Eigen::MatrixXd B, 
//                   int nthreads){
//   
//     Eigen::setNbThreads(nthreads);
//     //omp_set_num_threads(nthreads);
//     Eigen::MatrixXd C = A * B;
//     return Rcpp::wrap(C);
// }


//' Matrix Multiplication via Eigen
//' @param A A matrix. 
//' @param B A matrix.
//' @param nthreads Integer. Number of threads to use for matrix multiplication.
//' @return A matrix
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
//' @return The maximum number of threads supported.
// [[Rcpp::export]]
int cpp_get_nb_threads(){
  return omp_get_max_threads();
}

