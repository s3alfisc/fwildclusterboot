#include <RcppArmadillo.h>
#define NDEBUG 
#include <RcppEigen.h>
#ifdef _OPENMP
  #include <omp.h>
// [[Rcpp::plugins(openmp)]]
#else
  #define omp_get_max_threads() 0
  #define EIGEN_DONT_PARALLELIZE
#endif

// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]

using namespace Rcpp;


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
// [[Rcpp::export]]
int cpp_get_nb_threads(){
  return omp_get_max_threads();
}


// [[Rcpp::export]]
arma::mat sample_weights(int G,
                         int type){

  // type == 0 for rademacher
  // type == 1 for mammen
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


// // [[Rcpp::export]]
// arma::mat sample_weights2(int G,
//                          int type){
//   
//   // R random function based random numbers
//   // type == 0 for rademacher
//   // type == 1 for mammen
//   arma::vec weights(G);
//   
//   if(type == 0){
//     for(int g = 0; g < G; g++){
//       double v = R::runif(0, 1);
//       if(v < 0.5){
//         weights(g) = -1;
//       } else {
//         weights(g) = 1;
//       }
//     }
//   } else if(type == 1){
//     for(int g = 0; g < G; g++){
//       double v = R::runif(0, 1);
//       if(v < 1/6){
//         weights(g) = -sqrt(1.5);
//       } else if (v < 1/3){
//         weights(g) = -1;
//       } else if (v < 1/2){
//         weights(g) = -sqrt(0.5);
//       }  else if (v < 2/3){
//         weights(g) = sqrt(0.5);
//       } else if (v < 5/6){
//         weights(g) = 1;
//       } else if (v >= 5/6){
//         weights(g) = sqrt(1.5);
//       }
//     }
//   }
//   
//   
//   return weights;
//   
// }


//' Implementation of the heteroskedastic wild bootstrap. Computes
//' HC robust variance estimators. For use in fwildclusterboot when no
//' cluster variable is provided
//' @param y A vector - the dependent variable
//' @param X A matrix - the design matrix
//' @param R A matrix - the constraints matrix for a hypothesis test R'beta = r.
//' @param r A vector - r in hypothesis test R'beta = r.
//' @param B An integer - controls the number of bootstrap iterations.
//' @param N_G_bootcluster - The number of bootstrap clusters. For heteroskesdatic wild bootstrap, N_G_bootcluster = N, where N is the number of observations.
//' @param cores Integer: the number of cores to be used.
//' @param type : Integer. Should rademacher or webb weights be used? For rademacher weights, set 'type = 0'. For webb weights, set 'type = 1'.
//' @param small_sample_correction: double. Small sample correction to be applied.
//' @return A matrix of bootstrapped t-statistics, where the null is imposed on the bootstrap dgp.

// [[Rcpp::export]]
List wildboottestHC(const arma::vec & y,
                    const arma::mat & X,
                    const arma::mat & R,
                    const double & r,
                    const int & B,
                    const int & N_G_bootcluster,
                    const int & cores,
                    const int & type, 
                    const double & small_sample_correction) {

  // function implements wild cluster bootstrap,
  // imposing the null
  

  //int n = X.n_rows;
  int k = X.n_cols;

  // impose null on the bootstrap dgp
  arma::mat XXinv = (X.t() * X ).i();
  arma::mat beta = XXinv * (X.t() * y);

  // calculate beta constrained
  arma::vec beta_r = beta - XXinv * R * (R.t() * XXinv * R).i() * (R.t() * beta - r);
  arma::vec yhat_r = X * beta_r;
  arma::vec resid_r = y - yhat_r;

  NumericVector s(2);
  NumericVector prob = NumericVector::create();
  s[0] = 1;
  s[1] = -1;

  arma::mat t_boot(k, B + 1);

#pragma omp parallel for num_threads(cores)
for(int b = 1; b < B + 1; b++){

    // create bootstrap sample
    //arma::vec weights = RcppArmadillo::sample(s, N_G_bootcluster, true, prob);
    arma::vec weights = sample_weights(N_G_bootcluster, type);
    arma::vec y_boot = yhat_r + resid_r % weights;
    // get bootstrapped coefs and resids
    arma::vec coef_boot = XXinv * (X.t() * y_boot) ;
    arma::vec resid_boot = y_boot - X * coef_boot;
    // because resid_boot is diagonal
    arma::mat meat = X.t() * arma::diagmat(pow(resid_boot,2)) * X;
    arma::mat sigma_boot =   (XXinv * meat * XXinv);

    // calculate t-stats
    t_boot.col(b) = coef_boot / arma::sqrt(small_sample_correction * arma::diagvec(sigma_boot));
  }

  // for b = 0
  arma::vec y_boot = yhat_r + resid_r;
  // get bootstrapped coefs and resids
  arma::vec coef_boot = XXinv * (X.t() * y_boot) ;
  arma::vec resid_boot = y_boot - X * coef_boot;
  // because resid_boot is diagonal
  arma::mat meat = X.t() * arma::diagmat(pow(resid_boot,2)) * X;
  arma::mat sigma_boot =   (XXinv * meat * XXinv);

  // calculate t-stats
  t_boot.col(0) = coef_boot / arma::sqrt(small_sample_correction * arma::diagvec(sigma_boot));

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
//' @param type : Integer. Should rademacher or webb weights be used? For rademacher weights, set 'type = 0'. For webb weights, set 'type = 1'.
//' @param cluster: Integer Vector. Contains information on the clusters.
//' @return A matrix of bootstrapped t-statistics, where the null is imposed on the bootstrap dgp.

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
  arma::vec beta_r = beta - XXinv * R * (R.t() * XXinv * R).i() * (R.t() * beta - r);
  arma::vec yhat_r = X * beta_r;
  arma::vec resid_r = y - yhat_r;

  arma::mat t_boot(k, B + 1);

#pragma omp parallel for num_threads(cores)
  for(int b = 1; b < B + 1; b++){

    // create bootstrap sample
    arma::vec y_boot(n);
    for(int g = 0; g < N_G_bootcluster; g++){
      y_boot(find(cluster == g)) = yhat_r(find(cluster == g)) + resid_r(find(cluster == g)) * sample_weights(1, type);
    }

    // get bootstrapped coefs and resids
    arma::vec coef_boot = XXinv * (X.t() * y_boot) ;
    arma::vec resid_boot = y_boot - X * coef_boot;

    // calculate CR vcov
    arma::mat meat(k, k);

    // for all g = 1,..., G clusters
    //for(int g = 0; g < G; g++){
    //arma::mat meat_g(k, k);
    for(int n_g = 0; n_g < N_G_bootcluster; n_g++){
      arma::mat X_ng = X.rows(find(cluster == n_g));
      arma::mat resid_boot_ng = resid_boot(find(cluster == n_g));
      //meat +=  vcov_sign(g) * small_sample_correction(g) * X_ng.t() * resid_boot_ng * resid_boot_g.t() * X_ng;
      meat += X_ng.t() * resid_boot_ng * resid_boot_ng.t() * X_ng;
    }
    //meat += meat_g;
    //}

    arma::mat sigma_boot =  (XXinv * meat * XXinv);

    // calculate t-stats
    t_boot.col(b) = coef_boot / arma::sqrt(small_sample_correction * arma::diagvec(sigma_boot));
  }

  // for b = 0
  // create bootstrap sample
  arma::vec y_boot(n);
  for(int n_g = 0; n_g < N_G_bootcluster; n_g++){
    y_boot(find(cluster == n_g)) = yhat_r(find(cluster == n_g)) + resid_r(find(cluster == n_g)) ;
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
    //meat +=  vcov_sign(g) * small_sample_correction(g) * X_ng.t() * resid_boot_ng * resid_boot_g.t() * X_ng;
    meat += X_ng.t() * resid_boot_ng * resid_boot_ng.t() * X_ng;
  }
  //meat += meat_g;
  //}
  arma::mat sigma_boot =   (XXinv * meat * XXinv);

  // calculate t-stats
  t_boot.col(0) = coef_boot / arma::sqrt(small_sample_correction * arma::diagvec(sigma_boot));

  List res;
  res["t_boot"] = t_boot;

  return res;

}
