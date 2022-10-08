#include <RcppArmadillo.h>
#define NDEBUG
#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#else
#define omp_get_max_threads() 0
#endif


using namespace Rcpp;

// calculate the denominator for bootsraps of type x1 (that rely)
// on CRV1 covariance matrices
// [[Rcpp::export]]
arma::vec boot_algo3_crv1_denom(int B, 
                                int G, 
                                double ssc, 
                                arma::mat H, 
                                arma::vec Cg, 
                                arma::mat v, 
                                int cores){
  
  arma::vec denom(B+1);
  
#pragma omp parallel for num_threads(cores)
  for(int b = 0; b < (B+1); b++){
    
    //double Z_sq;
    arma::vec Zg(G);
    for(int g = 0; g < G; g++){
      double vH = 0; 
      for(int h = 0; h < G; h++){
        vH += v(h, b) * H(g, h);
      }
      Zg(g) = Cg(g) * v(g, b) - vH;
    }
    
    denom(b) = ssc * arma::as_scalar(sum(pow(Zg, 2)));
    
  }
  
  return denom;
  
}

// // [[Rcpp::export]]
// List boot_algo3_crv1(
//     const int B,
//     const int G, 
//     const int k, 
//     arma::mat v, 
//     arma::mat scores_mat, 
//     arma::vec scores_mat2, 
//     arma::mat scores_boot, 
//     arma::mat tXXinv, 
//     arma::cube Ag, 
//     double ssc, 
//     int cores, 
//     arma::mat R
// ){
//   
//   arma::vec se(B+1);
//   // arma::cube boot_vcov(k, k, B+1);
//   arma::mat RtXXinv = R * tXXinv;
//   
// #pragma omp parallel for num_threads(cores)
//   for(int b = 0; b < (B+1); b++){
//     
//     arma::mat score_hat_boot; 
//     
//     for(int g = 0; g < G; g++){
//       
//       arma::vec scores_g_boot = scores_mat.col(g) * v(g, b);
//       double scores_g_boot2 = scores_mat2(g) * v(g, b);
//       
//       arma::mat score_hat_boot_sq = pow( 
//         (scores_g_boot2 - Ag.slice(g) * scores_boot.col(b)),2);
//       score_hat_boot += score_hat_boot_sq;
//       
//     }
//     
//     // boot_vcov.slice(b) = ssc * tXXinv * score_hat_boot * tXXinv;
//     // se(b) = sqrt(arma::as_scalar(R * boot_vcov.slice(b) * R.t()));
//     se(b) = sqrt(ssc * arma::as_scalar(score_hat_boot));
//   }
//   
//   List res_list; 
//   // res_list["boot_vcov"] = boot_vcov; 
//   res_list["se"] = se; 
//   
//   return res_list; 
//   
//   
// }


// [[Rcpp::export]]

List boot_algo3_crv3( const int B,
                      const int G,
                      const int k,
                      arma::mat v,
                      arma::mat scores_mat,
                      arma::mat scores_boot,
                      arma::cube inv_tXX_tXgXg,
                      int cores,
                      arma::mat R,
                      arma::mat delta_b_star){
  
  arma::vec se(B+1);
  arma::cube boot_vcov(k, k, B+1);
  
  double ssc = (G-1) / G;
  
#pragma omp parallel for num_threads(cores)
  for(int b = 0; b < B +1; b++){
    
    arma::mat delta_diff(G,k);
    
    for(int g = 0; g < G; g++){
      
      arma::vec scores_g_boot = scores_mat.col(g) * v(g, b);
      
      arma::mat score_diff = scores_boot.col(b) - scores_g_boot;
      delta_diff.row(g) = (inv_tXX_tXgXg.slice(g) * score_diff
                             - delta_b_star.col(g)).t();
    }
    
    boot_vcov.slice(b) = ssc * delta_diff.t() * delta_diff;
    se(b) = arma::as_scalar(sqrt(ssc * R * boot_vcov.slice(b) * R.t()));
    
    
  }
  
  List res_list;
  res_list["boot_vcov"] = boot_vcov;
  res_list["se"] = se;
  
  return res_list;
  
  
  
  
}

// // list multiplication 
// // [[Rcpp::export]]
// arma::cube multiply_listwise(List List1, 
//                        List List2, 
//                        int G, 
//                        int k,
//                        int cores){
//   
//   arma::cube res(k, k, G); 
// #pragma omp parallel for num_threads(cores)
//   for(int g = 0; g < G; g++){
//     
//     arma::mat mat1 = List1[g]; 
//     arma::mat mat2 = List2[g];
//     res.slice(g) = mat1.t() * mat2; 
//   }
//   
//   return res; 
//   
// }




