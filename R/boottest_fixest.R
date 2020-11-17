boottest.fixest  <- function(object, 
                           clustid, 
                           param, 
                           B,
                           data,
                           alpha = NULL, 
                           fixed_effects = NULL, 
                           weights = NULL,
                           conf_int = NULL, 
                           debug = FALSE, 
                           seed = NULL, 
                           beta0 = 0, 
                           demean = NULL){
  
  
  #' Computes wild cluster bootstrap for object of class fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param demean If TRUE, fixed effects are projected out prior to the bootstrap. FALSE by default
  #'@return An object of class boottest
  #'@export
  #'@method boottest fixest


  # Step 1: check arguments of feols call
  #formula <- object$call$fml
  weights <- object$call$weights

  if(!is.null(weights)){
    stop("Currently, boottest does not support weighted least squares. weights 
         must be NULL.")
  }
  
  if(!is.null(seed)){
    set.seed(seed)
  } else if(is.null(seed)){
    set.seed(2)
  }
  
  
 
  preprocess <- preprocess(object = object, 
                                  param = param,
                                  clustid = clustid,
                                  beta0 = beta0,
                                  alpha = alpha,
                                  demean = demean)
  
  clustid_dims <- preprocess$clustid_dims
  # Invert p-value
  point_estimate <- object$coefficients[param]
  
  # if(clustid_dims == 1){
  #   # boot algoritm
  res <- boot_algo(preprocess)
  # compute confidence sets
  
  if(is.null(conf_int) || conf_int == TRUE){
    # calculate guess for covariance matrix and standard errors
    vcov <- sandwich::vcovCL(object, cluster = preprocess$clustid)
    coefs <- lmtest::coeftest(object, vcov)
    se_guess <- coefs[param, "Std. Error"]
    
    res_p_val <- invert_p_val(object = res, 
                                  point_estimate = point_estimate,
                                  se_guess = se_guess,
                                  #data = data,
                                  clustid = preprocess$clustid,
                                  X = preprocess$X,
                                  Y = preprocess$Y,
                                  param = param,
                                  R0 = preprocess$R0,
                                  alpha = preprocess$alpha,
                                  N = preprocess$N, 
                                  k = preprocess$k, 
                                  B = B,
                                  invXX = res$invXX,
                                  v = res$v,
                                  Xr = res$Xr,
                                  XinvXXr = res$XinvXXr,
                                  SXinvXXRX = res$SXinvXXRX)
  } else {
    res_p_val <- list( conf_int = NULL, 
                       p_test_vals = NULL, 
                       test_vals = NULL)
    }
  
  if(clustid_dims == 1){
    res_final <- list(point_estimate = point_estimate, 
                      p_val = res[["p_val"]], 
                      conf_int = res_p_val$conf_int, 
                      p_test_vals = res_p_val$p_test_vals, 
                      test_vals = res_p_val$test_vals,
                      t_stat = res$t_stat, 
                      regression = res$object, 
                      param = param, 
                      N = preprocess$N, 
                      B = B, 
                      clustid = clustid, 
                      #depvar = depvar, 
                      N_G = preprocess$N_G)
  } else if(clustid_dims > 1){
    res_final <- list(point_estimate = point_estimate, 
                      p_val = res[["p_val"]], 
                      #conf_int = res_p_val$conf_int, 
                      #p_test_vals = res_p_val$p_test_vals, 
                      #test_vals = res_p_val$test_vals,
                      t_stat = res$t_stat, 
                      regression = res$object, 
                      param = param, 
                      N = preprocess$N, 
                      B = B, 
                      clustid = clustid, 
                      #depvar = depvar, 
                      N_G = preprocess$N_G)
  }
  
  
  class(res_final) <- "boottest"
  
  res_final
  
} 
