#include <RcppArmadillo.h>
#define NDEBUG
#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#else
#define omp_get_max_threads() 0
#endif

using namespace Rcpp;
// 
// // This is a simple example of exporting a C++ function to R. You can
// // source this function into an R session using the Rcpp::sourceCpp
// // function (or via the Source button on the editor toolbar). Learn
// // more about Rcpp at:
// //
// //   http://www.rcpp.org/
// //   http://adv-r.had.co.nz/Rcpp.html
// //   http://gallery.rcpp.org/
// //
// 
// // [[Rcpp::export]]
// List boot_algo3cpp(
//     const int G,
//     const int B,
//     arma::mat X,
//     arma::mat scores_list2,
//     arma::mat RtXXinv,
//     Rcpp::List Ag, 
//     arma::mat v, 
//     const int ssc, 
//     const int crv_type, 
//     const List inv_tXX_tXgXg, 
//     arma::vec R) {
// 
//   int k = X.n_cols;
//   
//   arma::vec delta_b_star(B);
//   arma::vec se(B);
// 
//   if(crv_type == 0){
//     
//     for(int b = 0; b < B; b++){
//       
//       // Step 1: calculate numerator
//       
//       arma::mat scores_g_boot(G, k);
//       arma::mat scores_boot(1,k);
//       
//       for(int g = 0; g < G; g++){
//         for (int j = 0; j < k; j++){
//           scores_g_boot(g, j) = scores_list2(j,g) * v(g,b);
//         }
//         scores_boot += scores_g_boot.row(g);
//       }
//       
//       delta_b_star(b) = arma::as_scalar(RtXXinv * scores_boot.t());
//       
//       // 2 denominator (variance) calculation
//       
//       arma::mat Sigma(k,k);
//       arma::mat score_hat_g_boot(1,k);
//       // arma::mat Sigma;
//       
//       for(int g = 0; g < G; g++){
//         score_hat_g_boot = scores_g_boot.row(g).t() - as<arma::mat>(Ag[g]) * scores_boot.t();
//         Sigma += score_hat_g_boot * score_hat_g_boot.t();
//       }
//       
//       se(b) = sqrt(arma::as_scalar(ssc * RtXXinv * Sigma * RtXXinv.t()));
//       
//     }
//     
//   } else if(crv_type == 1){
//     
//     // Step 1: calculate numerator
//   
//     
//     
//     for(int b = 0; b < B; b++){
//       
//       arma::mat scores_g_boot(G, k);
//       arma::mat scores_boot(1,k);
//       
//       for(int g = 0; g < G; g++){
//         for (int j = 0; j < k; j++){
//           scores_g_boot(g, j) = scores_list2(j,g) * v(g,b);
//         }
//         scores_boot += scores_g_boot.row(g);
//       }
//       
//       delta_b_star(b) = arma::as_scalar(RtXXinv * scores_boot.t());
//       
//       
//       // 2 get numerator 
//       
//       arma::mat Sigma(k,k);
//       arma::mat score_hat_g_boot(1,k);
//       // Rcout << "The value of Sigma is " << Sigma << std::endl;  
//       
//       // Rcout << "asd" << scores_boot - scores_g_boot.row(1)<< std::endl; 
//       // Rcout << "test 2" << arma::pow(
//       //           as<arma::mat>(
//       //             inv_tXX_tXgXg[1]
//       //           )
//       //         * arma::as_scalar((scores_boot - scores_g_boot.row(0))(find(R == 1)))
//       //           - delta_b_star(0), 2) << std::endl;
//       
//       arma::mat Sigma;
//       arma::vec delta_diff(k);
//       arma::vec score_diff(k);
//       
//       for(int g = 0; g < G; g++){
//        
//        score_diff = scores_boot - scores_g_boot.row(g);
//        delta_diff += 
//          arma::pow(
//            as<arma::mat>(
//              inv_tXX_tXgXg[g]
//              ) 
//          * score_diff 
//          - delta_b_star, 2);
//          
//       }
//       // 
//       // se(b) = sqrt((G-1) / G) *
//       //   arma::as_scalar(
//       //     delta_diff(
//       //       find(
//       //         R == 1
//       //       )
//       //     )
//       //   );
//       
//     }
//     
//   }
// 
//   arma::vec t_boot = delta_b_star / se;
// 
//   List res; 
//   res["delta_b_star"] = delta_b_star; 
//   res["se"] = se;
//   res["t_boot"] = t_boot;
// 
//   return res; 
//   
// }
// 
//   
//   List res; 
//   res[""] = ; 
//   return res; 
//   
// }

List matrix_to_arma_list(List x, int G){
  
  List res(G);
  for(int g = 0; g < G; g++){
     res[g] = as<arma::mat>(x[g]);
    }
  
  return res; 
  
}


/*** R
res <- boot_algo3cpp(
  G = G,
  B = 2,
  X = X,
  scores_list2 = scores_list2,
  RtXXinv = R %*% tXXinv,
  Ag = Ag, 
  v = v, 
  ssc = small_sample_correction, 
  crv_type = 1, 
  inv_tXX_tXgXg = inv_tXX_tXgXg, 
  R = R
)





*/
