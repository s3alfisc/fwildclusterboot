boottest.felm  <- function(object, 
                           clustid, 
                           param, 
                           B,
                           weights = NULL,
                           conf_int = NULL, 
                           debug = FALSE, 
                           seed = NULL, 
                           beta0 = 0, 
                           alpha = NULL, 
                           demean = NULL){
  
  
  #' Function that runs boottest for object of class felm
  #'@import Formula
  #'@import data.table
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@return An object of class boottest
  #'@export
  #'@method boottest felm
  
  #  object = felm_fit1
  #  param = "treatment"
  #  clustid = ~ group_id1
  #  beta0 = 0
  #  alpha = 0.05
  #  demean = FALSE 
  # B = 10000
  # weights = NULL
  # conf_int = NULL 
  # debug = FALSE 
  # seed = NULL 

    preprocess <- preprocess(object = object,
                           param = param,
                           clustid = clustid,
                           beta0 = beta0,
                           alpha = alpha,
                           demean = demean)

  clustid_dims <- preprocess$clustid_dims
  # Invert p-value
  point_estimate <- object$coefficients[param, ]
  
  # if(clustid_dims == 1){
  #   # boot algoritm
  res <- boot_algo(preprocess)
  # compute confidence sets
  
  if(is.null(conf_int) || conf_int == TRUE){
    # calculate guess for covariance matrix and standard errors
    #vcov <- sandwich::vcovCL(object, cluster = preprocess$clustid)
    #coefs <- lmtest::coeftest(object, vcov)
    #se_guess <- coefs[param, "Std. Error"]
    #vcov <- object$robustvcv
    #coefs <- object$coefficients[param]
    se_guess <- object$se[param]
    
    res_p_val <- invert_p_val(object = res,
                              point_estimate = point_estimate,
                              se_guess = se_guess, 
                              clustid = preprocess$clustid,
                              X = preprocess$X,
                              Y = preprocess$Y,
                              N = preprocess$N,
                              k = preprocess$k,
                              v = res$v,
                              param = param,
                              R0 = preprocess$R0,
                              B = B,
                              beta0 = preprocess$beta0,
                              alpha = preprocess$alpha)
  } else {
    res_p_val <- list( conf_int = NA, 
                       p_test_vals = NA, 
                       test_vals = NA)
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

