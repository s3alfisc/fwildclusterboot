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

//'create bootstrap sample weights
//' @param G the number of clusters
//' @param type 0 for rademacher, 1 for webb
//' @noRd
// [[Rcpp::export]]
arma::mat sample_weights(int G,
                         int type){
  
  // type == 0 for rademacher
  // type == 1 for webb
  arma::vec weights(G);
  
  if(type == 0){
    for(int g = 0; g < G; g++){
      double v = arma::randu();
      if(v < 0.5){
        weights(g) = -1;
      } else {
        weights(g) = 1;
      }
    }
  } else if(type == 1){
    for(int g = 0; g < G; g++){
      double v = arma::randu();
      if(v < 1.0/6.0){
        weights(g) = -sqrt(1.5);
      } else if (v < 1.0/3.0){
        weights(g) = -1;
      } else if (v < 1.0/2.0){
        weights(g) = -sqrt(0.5);
      } else if (v < 2.0/3.0){
        weights(g) = sqrt(0.5);
      } else if (v < 5.0/6.0){
        weights(g) = 1;
      } else {
        weights(g) = sqrt(1.5);
      }
    }
  }
  
  
  return weights;
  
}


//' Implementation of the heteroskedastic wild bootstrap. Computes
//' HC robust variance estimators. For use in fwildclusterboot when no
//' cluster variable is provided
//' @param y A vector - the dependent variable
//' @param X A matrix - the design matrix
//' @param R A matrix - the constraints matrix for a hypothesis test R'beta = r.
//' @param r A vector - r in hypothesis test R'beta = r.
//' @param B An integer - controls the number of bootstrap iterations.
//' @param N_G_bootcluster - The number of bootstrap clusters. For 
//' heteroskesdatic wild bootstrap, N_G_bootcluster = N, where N 
//' is the number of observations.
//' @param cores Integer: the number of cores to be used.
//' @param type : Integer. Should rademacher or webb weights be used? 
//' For rademacher weights, set 'type = 0'. For webb weights, set 'type = 1'.
//' @param small_sample_correction: double. Small sample correction to be 
//' applied.
//' @return A matrix of bootstrapped t-statistics, where the null is imposed 
//' on the bootstrap dgp.
//' @noRd


// [[Rcpp::export]]
List wildboottestHC(const arma::vec & y,
                    const arma::mat & X,
                    const arma::mat & R,
                    const double & r,
                    const int & B,
                    const int & N_G_bootcluster,
                    const int & cores,
                    const int & type, 
                    const double & small_sample_correction, 
                    const int bootstrap_type) {
  
  // function implements wild cluster bootstrap,
  // imposing the null
  
  
  int N = X.n_rows;

  // impose null on the bootstrap dgp
  arma::mat XXinv = (X.t() * X ).i();
  arma::mat beta = XXinv * (X.t() * y);
  
  arma::mat XXinvX = XXinv * X.t();
  arma::mat RXXinvX = (R.t() * XXinv) * X.t(); //1 x N
  arma::mat RXXinvX_squared = (arma::pow(RXXinvX, 2));
    
  // calculate beta constrained
  arma::vec beta_r = beta - XXinv * R * (R.t() * XXinv * R).i() *
    (R.t() * beta - r);
  arma::vec yhat_r = X * beta_r;
  arma::vec resid_r = y - yhat_r;
  
  // NumericVector s(2);
  // NumericVector prob = NumericVector::create();
  // s[0] = 1;
  // s[1] = -1;
  
  // compute 1/(1-h_ii)'s -> resid_multiplier
  arma::vec resid_multiplier(N);
  // resid_multiplier.ones();
  
  if(bootstrap_type == 1){
    
    resid_multiplier.ones();
    
  } else {
    
    arma::mat hatmat = X * XXinv * X.t(); 
    arma::vec diag_hatmat(N);
    
    for(int i = 0; i < N; i++){
      diag_hatmat(i) = hatmat(i,i);
    }
    
    if(bootstrap_type == 2){
      resid_multiplier = 1 / arma::sqrt(1-diag_hatmat);    
    } else if(bootstrap_type == 3){
      resid_multiplier = 1 / (1-diag_hatmat);    
    }
    
  }
  
  arma::vec t_boot(B + 1);
  
#pragma omp parallel for num_threads(cores)
  for(int b = 1; b < B + 1; b++){
    
    // create bootstrap sample
    //arma::vec weights = RcppArmadillo::sample(s, N_G_bootcluster, true, prob);
    arma::vec weights = sample_weights(N_G_bootcluster, type);
    arma::vec y_boot = yhat_r + resid_multiplier % resid_r % weights;
    // get bootstrapped coefs and resids
    arma::vec coef_boot = XXinvX * y_boot ; // k x 1 
    arma::vec resid_boot = y_boot - X * coef_boot;
    arma::mat boot_var = RXXinvX_squared * pow(resid_boot, 2);
    
    // calculate t-stats
    t_boot(b) = arma::as_scalar((R.t() * coef_boot - r) / 
      // arma::sqrt(small_sample_correction * (R.t() * sigma_boot * R)));
      arma::sqrt(small_sample_correction * boot_var));
  }
  
  // for b = 0 - always HC1
  arma::vec y_boot = yhat_r + resid_r;
  // get bootstrapped coefs and resids
  arma::vec coef_boot = XXinvX * y_boot ; // k x 1 
  arma::vec resid_boot = y_boot - X * coef_boot;
  arma::mat boot_var = RXXinvX_squared * pow(resid_boot, 2);
  
  // calculate t-stats
  t_boot(0) = arma::as_scalar((R.t() * coef_boot - r) / 
    // arma::sqrt(small_sample_correction * (R.t() * sigma_boot * R)));
    arma::sqrt(small_sample_correction * boot_var));
  
  List res;
  res["t_boot"] = t_boot;
  
  return res;
  
}


//' Implementation of the wild  cluster bootstrap. Computes
//' cluster robust variance estimators. For use in fwildclusterboot when
//' the memory demands of the fast and wild algorithm are infeasible
//' @param y A vector - the dependent variable
//' @param X A matrix - the design matrix
//' @param R A matrix - the constraints matrix for a hypothesis test R'beta = r.
//' @param r A vector - r in hypothesis test R'beta = r.
//' @param B An integer - controls the number of bootstrap iterations.
//' @param N_G_bootcluster - The number of bootstrap clusters.
//' @param cores Integer: the number of cores to be used.
//' @param type : Integer. Should rademacher or webb weights be used?
//'  For rademacher weights, set 'type = 0'. For webb weights, set 'type = 1'.
//' @param cluster: Integer Vector. Contains information on the clusters.
//' @return A matrix of bootstrapped t-statistics, where the null is 
//' imposed on the bootstrap dgp.
//' @noRd


// [[Rcpp::export]]
List wildboottestCL(const arma::vec & y,
                    const arma::mat & X,
                    const arma::mat & R,
                    const double & r,
                    const int & B,
                    const int & N_G_bootcluster,
                    const int & cores,
                    const int & type,
                    const arma::vec & cluster, 
                    const double & small_sample_correction) {
  
  // function implements wild cluster bootstrap,
  // imposing the null
  
  int n = X.n_rows;
  int k = X.n_cols;
  
  // impose null on the bootstrap dgp
  arma::mat XXinv = (X.t() * X ).i();
  arma::mat beta = XXinv * (X.t() * y);
  
  // calculate beta constrained
  arma::vec beta_r = beta - XXinv * R * (R.t() * XXinv * R).i() 
    * (R.t() * beta - r);
  arma::vec yhat_r = X * beta_r;
  arma::vec resid_r = y - yhat_r;
  
  arma::vec t_boot(B + 1);
  
#pragma omp parallel for num_threads(cores)
  for(int b = 1; b < B + 1; b++){
    
    // create bootstrap sample
    arma::vec y_boot(n);
    for(int g = 0; g < N_G_bootcluster; g++){
      y_boot(find(cluster == g)) = yhat_r(find(cluster == g)) + 
        resid_r(find(cluster == g)) * sample_weights(1, type);
    }
    
    // get bootstrapped coefs and resids
    arma::vec coef_boot = XXinv * (X.t() * y_boot) ;
    arma::vec resid_boot = y_boot - X * coef_boot;
    
    // calculate CR vcov
    arma::mat meat(k, k);
    
    // for all g = 1,..., G clusters
    for(int n_g = 0; n_g < N_G_bootcluster; n_g++){
      arma::mat X_ng = X.rows(find(cluster == n_g));
      arma::mat resid_boot_ng = resid_boot(find(cluster == n_g));
      meat += X_ng.t() * resid_boot_ng * resid_boot_ng.t() * X_ng;
    }
    
    arma::mat sigma_boot =  (XXinv * meat * XXinv);
    
    // calculate t-stats
    t_boot(b) = arma::as_scalar((R.t() * coef_boot - r) / 
      arma::sqrt(small_sample_correction * (R.t() * sigma_boot * R)));
    
  }
  
  // for b = 0
  // create bootstrap sample
  arma::vec y_boot(n);
  for(int n_g = 0; n_g < N_G_bootcluster; n_g++){
    y_boot(find(cluster == n_g)) = yhat_r(find(cluster == n_g)) + 
      resid_r(find(cluster == n_g)) ;
  }
  
  // get bootstrapped coefs and resids
  arma::vec coef_boot = XXinv * (X.t() * y_boot) ;
  arma::vec resid_boot = y_boot - X * coef_boot;
  
  // calculate CR vcov
  arma::mat meat(k, k);
  //for(int g = 0; g < G; g++){
  for(int n_g = 0; n_g < N_G_bootcluster; n_g++){
    arma::mat X_ng = X.rows(find(cluster == n_g));
    arma::mat resid_boot_ng = resid_boot(find(cluster == n_g));
    meat += X_ng.t() * resid_boot_ng * resid_boot_ng.t() * X_ng;
  }
  //meat += meat_g;
  //}
  arma::mat sigma_boot =   (XXinv * meat * XXinv);
  
  // calculate t-stats
  t_boot(0) = arma::as_scalar((R.t() * coef_boot - r) / 
    arma::sqrt(small_sample_correction * (R.t() * sigma_boot * R)));
  
  List res;
  res["t_boot"] = t_boot;
  
  return res;
  
}



//' Implementation of the wild  cluster bootstrap. Computes
//' cluster robust variance estimators. For use in fwildclusterboot when
//' the memory demands of the fast and wild algorithm are infeasible
//' @param y A vector - the dependent variable
//' @param X A matrix - the design matrix
//' @param R A matrix - the constraints matrix for a hypothesis test R'beta = r.
//' @param r A vector - r in hypothesis test R'beta = r.
//' @param B An integer - controls the number of bootstrap iterations.
//' @param N_G_bootcluster - The number of bootstrap clusters.
//' @param cores Integer: the number of cores to be used.
//' @param cluster: Integer Vector. Contains information on the clusters.
//' @param v: enumerated weights matrix 
//' @return A matrix of bootstrapped t-statistics, where the null is
//'  imposed on the bootstrap dgp.
//' @noRd


// [[Rcpp::export]]
List wildboottestCL_enum(const arma::vec & y,
                         const arma::mat & X,
                         const arma::mat & R,
                         const double & r,
                         const int & B,
                         const int & N_G_bootcluster,
                         const int & cores,
                         const arma::vec & cluster, 
                         const double & small_sample_correction, 
                         const arma::mat & v) {
  
  // function implements wild cluster bootstrap,
  // imposing the null
  
  
  // int B = v.n_rows; 
  // int N_G_bootcluster = v.n_cols; 
  
  int n = X.n_rows;
  int k = X.n_cols;
  
  // impose null on the bootstrap dgp
  arma::mat XXinv = (X.t() * X ).i();
  arma::mat beta = XXinv * (X.t() * y);
  
  // calculate beta constrained
  arma::vec beta_r = beta - XXinv * R * (R.t() * XXinv * R).i()
    * (R.t() * beta - r);
  arma::vec yhat_r = X * beta_r;
  arma::vec resid_r = y - yhat_r;
  
  arma::vec t_boot(B + 1);
  
  
#pragma omp parallel for num_threads(cores)
  for(int b = 1; b < B + 1; b++){
    
    // create bootstrap sample
    arma::vec y_boot(n);
    for(int g = 0; g < N_G_bootcluster; g++){
      y_boot(find(cluster == g)) = yhat_r(find(cluster == g)) +
        resid_r(find(cluster == g)) * v(b, g);
    }
    
    // get bootstrapped coefs and resids
    arma::vec coef_boot = XXinv * (X.t() * y_boot) ;
    arma::vec resid_boot = y_boot - X * coef_boot;
    
    // calculate CR vcov
    arma::mat meat(k, k);
    
    // for all g = 1,..., G clusters
    for(int n_g = 0; n_g < N_G_bootcluster; n_g++){
      arma::mat X_ng = X.rows(find(cluster == n_g));
      arma::mat resid_boot_ng = resid_boot(find(cluster == n_g));
      meat += X_ng.t() * resid_boot_ng * resid_boot_ng.t() * X_ng;
    }
    
    arma::mat sigma_boot =  (XXinv * meat * XXinv);
    
    // calculate t-stats
    t_boot(b) = arma::as_scalar((R.t() * coef_boot - r) / 
      arma::sqrt(small_sample_correction * (R.t() * sigma_boot * R)));
    
  }
  
  
  
  // for b = 0
  // create bootstrap sample
  arma::vec y_boot(n);
  for(int n_g = 0; n_g < N_G_bootcluster; n_g++){
    y_boot(find(cluster == n_g)) = yhat_r(find(cluster == n_g)) + 
      resid_r(find(cluster == n_g)) ;
  }
  
  // get bootstrapped coefs and resids
  arma::vec coef_boot = XXinv * (X.t() * y_boot) ;
  arma::vec resid_boot = y_boot - X * coef_boot;
  
  // calculate CR vcov
  arma::mat meat(k, k);
  //for(int g = 0; g < G; g++){
  for(int n_g = 0; n_g < N_G_bootcluster; n_g++){
    arma::mat X_ng = X.rows(find(cluster == n_g));
    arma::mat resid_boot_ng = resid_boot(find(cluster == n_g));
    meat += X_ng.t() * resid_boot_ng * resid_boot_ng.t() * X_ng;
  }
  //meat += meat_g;
  //}
  arma::mat sigma_boot =   (XXinv * meat * XXinv);
  
  // calculate t-stats
  t_boot(0) = arma::as_scalar((R.t() * coef_boot - r) / 
    arma::sqrt(small_sample_correction * (R.t() * sigma_boot * R)));
  
  
  // collect results  
  List res;
  res["t_boot"] = t_boot;
  res["B"] = B; 
  res["N_G_bootcluster"] = N_G_bootcluster; 
  res["v"] = v; 
  res["resid_r"] = r; 
  
  
  return res;
  
}




