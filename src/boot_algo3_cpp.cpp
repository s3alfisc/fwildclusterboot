#include <RcppArmadillo.h>
#define NDEBUG
#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#else
#define omp_get_max_threads() 0
#endif


using namespace Rcpp;

// [[Rcpp::export]]
List boot_algo3_crv1(
    const int B,
    const int G, 
    const int k, 
    arma::mat v, 
    arma::mat scores_mat, 
    arma::mat scores_boot, 
    arma::mat tXXinv, 
    arma::cube Ag, 
    double ssc, 
    int cores, 
    arma::mat R
    ){

  arma::vec se(B+1);
  arma::cube boot_vcov(k, k, B+1);
  
#pragma omp parallel for num_threads(cores)
  for(int b = 0; b < (B+1); b++){

    arma::mat score_hat_boot(k, k); 
    
    for(int g = 0; g < G; g++){

      arma::vec scores_g_boot = scores_mat.col(g) * v(g, b);
      arma::mat score_hat_boot_sq = scores_g_boot - 
        Ag.slice(g) * scores_boot.col(b);
      score_hat_boot += score_hat_boot_sq * score_hat_boot_sq.t();

    }
    
    boot_vcov.slice(b) = ssc * tXXinv * score_hat_boot * tXXinv;
    se(b) = arma::as_scalar(R * boot_vcov.slice(b) * R.t());
  }
  
  List res_list; 
  res_list["boot_vcov"] = boot_vcov; 
  res_list["se"] = se; 
  
  return res_list; 


}


// // [[Rcpp::export]]
// List boot_algo3_crv3( const int B,
//                       const int G,
//                       const int k,
//                       arma::mat v,
//                       arma::mat scores_mat,
//                       arma::mat scores_boot,
//                       arma::cube inv_tXX_tXgXg,
//                       int cores,
//                       arma::mat R, 
//                       arma::mat delta_b_star){
// 
//   arma::vec se(B+1);
//   arma::cube boot_vcov(k, k, B+1);
//   
//   double ssc = (G-1) / G;
// 
//   for(int b = 0; b < B +1; b++){
// 
//     arma::mat delta_diff(G,k);
// 
//     for(int g = 0; g < G; g++){
//       
//       arma::vec scores_g_boot = scores_mat.col(g) * v(g, b);
//       
//       arma::mat score_diff = scores_boot.col(b) - scores_g_boot;
//       delta_diff.row(g) = (inv_tXX_tXgXg.slice(g) * score_diff
//          - delta_b_star.col(g)).t();
//     }
//     
//     boot_vcov.slice(b) = ssc * delta_diff.t() * delta_diff;
//     se(b) = arma::as_scalar(sqrt(ssc * R * boot_vcov.slice(b) * R.t()));
// 
// 
//   }
//   
//   List res_list;
//   res_list["boot_vcov"] = boot_vcov;
//   res_list["se"] = se;
//   
//   return res_list; 
//   
// 
// 
// 
// }


/*** R
# pracma::tic()
# boot_algo3_crv1(
#   B = B,
#   G = G, 
#   k = k, 
#   v = v, 
#   scores_mat = scores_mat, 
#   scores_boot = scores_boot, 
#   tXXinv = tXXinv, 
#   Ag = Ag2, 
#   ssc = small_sample_correction, 
#   cores = 1, 
#   R = R
# )
# pracma::toc()
# 
# pracma::tic()
# res <- boot_algo3_crv1(
#   B = B,
#   G = G, 
#   k = k, 
#   v = v, 
#   scores_mat = scores_mat, 
#   scores_boot = scores_boot, 
#   tXXinv = tXXinv, 
#   Ag = Ag2, 
#   ssc = small_sample_correction, 
#   cores = 8, 
#   R = R
# )
# pracma::toc()

res <- 
boot_algo3_crv3( B=B,
                 G=G,
                 k=k,
                 v=v,
                 scores_mat= scores_mat,
                 scores_boot = scores_boot,
                 inv_tXX_tXgXg = inv_tXX_tXgXg2,
                 cores = 8,
                 R = R, 
                 delta_b_star = delta_b_star
)

res$se
res$boot_vcov
*/
