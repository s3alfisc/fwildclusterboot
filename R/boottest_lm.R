boottest.lm <- function(object, 
                        clustid, 
                        param, 
                        B,
                        weights = NULL, 
                        conf_int = NULL, 
                        debug = FALSE, 
                        seed = NULL, 
                        beta0 = NULL, 
                        alpha = NULL){
  
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@method boottest lm
  #'@return An object of class boottest
  #'@export
  #'@import sandwich
  #'@import lmtest


  
  # execute all functions in fwildclusterboot 
  #    setwd("C:/Users/alexa/Dropbox/fwildclusterboot/R")
  #     file.sources = list.files(pattern="*.R")
  #     sapply(file.sources, source, .GlobalEnv)
  #      set.seed(5)
  #      data <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 20, icc2 = 0.01)
  #      object <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = data)
  #      clustid <- data[, .(group_id1, group_id2)]
  #      param <- "treatment"
  #      beta0 = 0
  #      alpha = 0.05
  #      B = 10000
  #      weights = NULL
  #      conf_int = NULL 
  #      debug = FALSE
  #      seed = NULL
  # p_val_sandwich <- lmtest::coeftest(object, sandwich::vcovCL(x = object, ~ group_id1 + group_id2)) # 0.554481          
  
  preprocess <- preprocess(object = object, 
                           param = param, 
                           clustid = clustid, 
                           beta0 = beta0,
                           alpha = alpha)

  clustid_dims <- preprocess$clustid_dims
  # Invert p-value
  point_estimate <- object$coefficients[param]
  
  # if(clustid_dims == 1){
  #   # boot algoritm
    res <- boot_algo(preprocess)
    # compute confidence sets
   
    if(is.null(conf_int) || conf_int == TRUE){
      # calculate guess for covariance matrix and standard errors
      vcov <- sandwich::vcovCL(object, cluster =  preprocess$clustid)
      coefs <- lmtest::coeftest(object, vcov)
      se_guess <- coefs[param, "Std. Error"]
      
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
    res_p_val <- list(conf_int = NULL, 
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
  }

  
  class(res_final) <- "boottest"
  
  res_final
}
