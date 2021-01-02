// // [[Rcpp::depends(RcppEigen)]]
// 
// #include <RcppEigen.h>
// 
// using namespace Rcpp;
// 
// // // [[Rcpp::export]]
// // NumericMatrix Rcpp_matrix_List_sum(List x) {
// // int n = x.size();
// // NumericMatrix result = as<NumericMatrix>(x[0]);
// // for ( int i = 1; i < n; ++i ) {
// //   result += as<NumericMatrix>(x[i]);
// // }
// // return result;
// // }
// 
// // // [[Rcpp::export]]
// // NumericMatrix Eigen_matrix_List_sum(List x) {
// //   int n = x.size();
// //   NumericMatrix result = as<NumericMatrix>(x[0]);
// //   for ( int i = 1; i < n; ++i ) {
// //     result += as<NumericMatrix>(x[i]);
// //   }
// //   return result;
// // }
// 
// 
// // // [[Rcpp::export]]
// // NumericVector naomit_cpp(Rcpp::NumericVector x){
// // int n = x.length(); 
// // //Rcpp::NumericVector r(n);
// // std::vector<double> r(n); // need to do this to use .resize
// // int k=0;
// // for (int i = 0; i < n; ++i) {
// //  if (x[i]==x[i]) {
// //    r[k] = x[i];
// //    k++;
// //  }
// // }
// // // count the number of NA values in r
// // r.resize(k);
// // return Rcpp::wrap(r);  
// // }
// 
// // [[Rcpp::export]]
// double p_val_null_cpp(int beta0, 
//                     NumericVector A, 
//                     NumericVector B, 
//                  Rcpp::List CC, 
//                  Rcpp::List CD,
//                  Rcpp::List DD,
//                  Rcpp::List clustid,
//                  int length_clustid,
//                  int boot_iter, 
//                  NumericVector  small_sample_correction){
// 
// NumericVector numer = A + B * beta0; 
// 
// Rcpp::List JJ(length_clustid);
// for(int x = 0; x < length_clustid; x++ ){
//   Eigen::MatrixXd CC_sublist = CC[x]; 
//   Eigen::MatrixXd CD_sublist = CD[x]; 
//   Eigen::MatrixXd DD_sublist = DD[x]; 
//   int ssc_subvec = small_sample_correction[x];
//   Eigen::MatrixXd JJ_temp = ssc_subvec * (CC_sublist.array() + 2 * CD_sublist.array() * beta0 + DD_sublist.array() * pow(beta0, 2.0));
//   JJ[x] = JJ_temp.colwise().sum();
// }
// 
// List res; 
// res["numer"] = numer; 
// res["JJ"] = JJ; 
// 
// NumericVector JJ_sum = as<NumericVector>(JJ[0]);
// 
// // here it bugs
// if(length_clustid > 1){
//   for ( int i = 1; i < length_clustid; ++i ) {
//     JJ_sum += as<NumericVector>(JJ[i]);
//   }
// }
// 
// res["JJ_sum"] = JJ_sum; 
// 
// NumericVector denom = sqrt(JJ_sum);
// res["denom"] = sqrt(JJ_sum);
// 
// int boot_iter_plus_1 = boot_iter + 1; 
// NumericVector t = abs(numer) / denom;
// 
// //NumericVector t = denom[Range(1, boot_iter_plus_1)];
// //double p_val = mean(abs(t[0] - beta0) < t);
// //Numeric_Vector JJ_sum_pos = ifelse(JJ_sum > 0, 1, 0); 
// // NumericVector denom_pos = JJ_sum[JJ_sum_pos]; 
// // NumericVector numer_positive = numer[JJ_sum_pos];
// // 
// // int invalid_t = denom.length() - denom_positive.length();
// // 
// // NumericVector t = abs(numer_positive) / denom_positive; 
// // 
// // NumericVector keep_t_stats = Rcpp::Range(1, boot_iter + 1 - invalid_t);
// // NumericVector t_boot = t[keep_t_stats];
// double p_val = mean(abs(t[0] - beta0) < (t));
// //   
// //   
// // 
// // 
// // //NumericVector t = abs(as<NumericVector>(denom)) / denom;
// // 
//   return p_val;
//  
// }
// // 
// // // [[Rcpp::export]]
// // NumericVector conf_int_start_vals(int beta0, 
// //                                   NumericVector A, 
// //                                   NumericVector B, 
// //                                   Rcpp::List CC, 
// //                                   Rcpp::List CD,
// //                                   Rcpp::List DD,
// //                                   Rcpp::List clustid,
// //                                   int length_clustid,
// //                                   int boot_iter, 
// //                                   NumericVector  small_sample_correction){
// //    NumericVector res[16]; 
// //    for(int i = 0; i < 16; i++){
// //       res[i] = p_val_null_cpp(beta0, 
// //                     A, 
// //                     B, 
// //                     CC, 
// //                     CD,
// //                     DD,
// //                     clustid,
// //                     length_clustid,
// //                     boot_iter, 
// //                     small_sample_correction);
// //   }
// //    return res; 
// // }
//  
// 
// 
// /*** R
// library(data.table)
// library(fwildclusterboot)
// 
// setwd("C:/Users/alexa/Dropbox/fwildclusterboot/R")
// file.sources = list.files(pattern="*.R")
// sapply(file.sources, source, .GlobalEnv)
// #set.seed(6)
// voters <-
//   create_data_2(
//     N = 10000,
//     N_G1 = 40,
//     icc1 = 0.01,
//     N_G2 = 40,
//     icc2 = 0.01,
//     numb_fe1 = 10,
//     numb_fe2 = 10,
//     seed = 12345
//   )                   #   # create a missing variable in group_id1
// voters[1, group_id1 := NA]
// voters[2, proposition_vote := NA]
// object <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
// clustid <- c("group_id1", "group_id2") 
// param <- "treatment"
// beta0 = 0
// alpha = 0.05
// B = 100000
// weights = NULL
// conf_int = NULL 
// debug = FALSE
// seed = 1234
// preprocess <- preprocess(object = object, 
//                          param = param, 
//                          clustid = clustid, 
//                          beta0 = beta0,
//                          alpha = alpha, 
//                          seed = seed)
// 
// 
// res <- boot_algo2(preprocess, boot_iter = B)
// ABCD <- res$ABCD
// A <- ABCD$A
// B <- ABCD$B
// C <- ABCD$C
// D <- ABCD$D
// CC <- ABCD$CC
// CD <- ABCD$CD
// DD <- ABCD$DD
// clustid <- res$clustid
// G <- sapply(clustid, function(x) length(unique(x)))
// small_sample_correction <- G / (G - 1)
// library(microbenchmark)
// microbenchmark( 
//   cpp = p_val_null_cpp(beta0 = 0, 
//                     A = A, 
//                     B = B, 
//                     CC= CC, 
//                     CD = CD,
//                     DD = DD,
//                     clustid = clustid, 
//                     length_clustid = length(names(clustid)),
//                     boot_iter = 100000, 
//                     small_sample_correction =  small_sample_correction), 
// R = p_val_null2(beta0 = 0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = 100000, small_sample_correction = small_sample_correction), 
// times = 10
// )
//   
// ***/

