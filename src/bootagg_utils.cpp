#include <Rcpp.h>
#define NDEBUG
#ifdef _OPENMP
#include <omp.h>
// [[plugins(openmp)]]
#else
#define omp_get_max_threads() 0
#endif

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector get_c_all_boot_cpp(
    NumericVector coef_boot, 
    NumericVector shares,
    int B, 
    int cores
){
  
  NumericVector c_all_boot(B+1); 
#pragma omp parallel for num_threads(cores)
  for(int b = 0; b < B + 1; b++){
    c_all_boot[b] = sum(shares * coef_boot[b]);
  }
  return c_all_boot; 
  
}

// [[Rcpp::export]]
NumericVector get_se_all_boot_cpp(
    NumericMatrix V_boot, 
    NumericVector s,
    int B, 
    int cores
){
  
  NumericVector se_all_boot(B+1); 
#pragma omp parallel for num_threads(cores)
  for(int b = 0; b < B + 1; b++){
    se_all_boot[b] = sqrt(sum(s * V_boot[b]));
  }
  return se_all_boot; 
  
}

    

/*** R

get_c_all_boot_cpp(
  coef_boot = coef_boot[v_names_pos,], 
  shares= shares, 
  B = B, 
  cores = 1
)

get_se_all_boot_cpp(
  V_boot = V_boot[v_names_pos, v_names_pos, ],
  s= s,
  B = B,
  cores = 1
)
*/
