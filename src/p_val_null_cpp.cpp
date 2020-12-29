// // [[Rcpp::depends(RcppEigen)]]
// 
// #include <RcppEigen.h>
// 
// using namespace Rcpp;
// 
// // [[Rcpp::export]]
// NumericMatrix Rcpp_matrix_List_sum(List x) {
//  int n = x.size();
//  NumericMatrix result = as<NumericMatrix>(x[0]);
//  for ( int i = 1; i < n; ++i ) {
//    result += as<NumericMatrix>(x[i]);
//  }
//  return result;
// }
// 
// // [[Rcpp::export]]
// NumericVector naomit_cpp(Rcpp::NumericVector x){
//   int n = x.length(); 
//   //Rcpp::NumericVector r(n);
//   std::vector<double> r(n); // need to do this to use .resize
//   int k=0;
//   for (int i = 0; i < n; ++i) {
//    if (x[i]==x[i]) {
//      r[k] = x[i];
//      k++;
//    }
//   }
//   r.resize(k);
//   return Rcpp::wrap(r);  
// }

// // [[Rcpp::export]]
// SEXP p_val_null_cpp(int beta0, 
//                    Rcpp::NumericMatrix A, 
//                    Rcpp::NumericMatrix B, 
//                    Rcpp::List CC, 
//                    Rcpp::List CD,
//                    Rcpp::List DD,
//                    Rcpp::List clustid,
//                    int length_clustid,
//                    int boot_iter, 
//                    Rcpp::NumericVector  small_sample_correction){
//  
//  Rcpp::NumericVector numer = A + B * beta0; 
//  
//  Rcpp::List JJ;
//  for(int x = 0; x < length_clustid; x++ ){
//     Eigen::MatrixXd CC_sublist = CC[x]; 
//     Eigen::MatrixXd CD_sublist = CD[x]; 
//     Eigen::MatrixXd DD_sublist = DD[x]; 
//     int ssc_subvec = small_sample_correction[x];
//     Rcpp::NumericMatrix JJ_temp = ssc_subvec * (CC_sublist + 2 * CD_sublist*beta0+ DD_sublist* pow(beta0, 2.0));
//     JJ[x] = JJ_temp;
//  }
//  
//  // if(length_clustid == 1){
//  //   NumericMatrix JJ_sum = JJ[0]; 
//  // } else if(length_clustid > 1){
//  //   NumericMatrix JJ_sum = Rcpp_matrix_List_sum(JJ); 
//  // }
//  // 
//  // 
//  // 
//  // NumericVector is_na_JJ_sum = naomit_cpp(J);   
// 
// 
//  return Rcpp::wrap(JJ);
//  
// }
