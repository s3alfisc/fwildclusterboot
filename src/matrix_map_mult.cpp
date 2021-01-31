// // [[Rcpp::depends(RcppEigen)]]
// 
// #ifdef _OPENMP
//   #include <omp.h>
// #endif
// #include <RcppEigen.h>
// 
// 
// //' Matrix Multiplication via Eigen
// //' @param A A matrix. 
// //' @param B A matrix.
// //' @param n_cores Number of cores to be used for parallel matrix multiplication
// //' @return A matrix
// // [[Rcpp::export]]
// SEXP eigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A,
//                      Eigen::Map<Eigen::MatrixXd> B){
//   
//   //Eigen::setNbThreads(n_cores);
//   Eigen::MatrixXd C = A * B;
//   return Rcpp::wrap(C);
// }