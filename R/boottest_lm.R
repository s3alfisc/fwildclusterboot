boottest.lm <- function(object, 
                        clustid, 
                        param, 
                        B,
                        weights = NULL, 
                        conf_int = NULL, 
                        seed = NULL, 
                        beta0 = NULL, 
                        alpha = NULL){
  
  #' Function that runs boottest for object of class lm
  #'@param object An object of class lm
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@method boottest lm
  #'@return An object of class boottest
  #'@importFrom sandwich vcovCL 
  #'@importFrom lmtest coeftest
  #'@export


  #print(B)
  
   #execute all functions in fwildclusterboot 
                           # setwd("C:/Users/alexa/Dropbox/fwildclusterboot/R")
                           #  file.sources = list.files(pattern="*.R")
                           # sapply(file.sources, source, .GlobalEnv)
                           #   #set.seed(6)
                           # voters <-
                           #   create_data_2(
                           #     N = 10000,
                           #     N_G1 = 20,
                           #     icc1 = 0.01,
                           #     N_G2 = 10,
                           #     icc2 = 0.01,
                           #     numb_fe1 = 10,
                           #    numb_fe2 = 10,
                           #     seed = 12345
                           #   )                   #   # create a missing variable in group_id1
                           #  voters[1, group_id1 := NA]
                           #   voters[2, proposition_vote := NA]
                           #   object <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
                           #   clustid <- c("group_id1") 
                           #   param <- "treatment"
                           # beta0 = 0
                           #   alpha = 0.05
                           #   B = 100000
                           #   weights = NULL
                           #   conf_int = NULL 
                           #   debug = FALSE
                           #   seed = 1234
              
  check_arg(clustid, "character scalar | character vector")
  check_arg(param, "scalar character")
  check_arg(B, "scalar numeric ") 
  check_arg(alpha, "scalar numeric")
  check_arg(weights, "NULL")
  check_arg(conf_int, "logical scalar | NULL")
  check_arg(debug, "logical scalar")
  check_arg(seed, "scalar integer | NULL")
  check_arg(beta0, "numeric scalar | NULL")
  
  preprocess <- preprocess(object = object, 
                           param = param, 
                           clustid = clustid, 
                           beta0 = beta0,
                           alpha = alpha, 
                           seed = seed)

  clustid_dims <- preprocess$clustid_dims
  # Invert p-value
  point_estimate <- object$coefficients[param]
  
  clustid_fml <- as.formula(paste("~", paste(clustid, collapse = "+")))
  
  if(!is.numeric(B)){
    warning("Test 1: B is not numeric. ")
  }
  
  #   # boot algoritm
  N_G_2 <- 2^max(preprocess$N_G)
  if(N_G_2 < B){
    warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution. Therefore, 
                  B = ", N_G_2, "."))
    B <- N_G_2
  }
  
  #print(B)
  
  if(!is.numeric(B)){
    warning("Test 2: B is not numeric. ")
  }
  
  
  res <- boot_algo2(preprocess, boot_iter = B)
  
  #print(B)
    # compute confidence sets
   
  if(!is.numeric(res$B)){
    warning("Test 3: B is not numeric. ")
  }
  
    if(is.null(conf_int) || conf_int == TRUE){
      # calculate guess for covariance matrix and standard errors
      vcov <- suppressWarnings(vcovCL(object, cluster =  clustid_fml))
      coefs <- suppressWarnings(coeftest(object, vcov))
      se_guess <- coefs[param, "Std. Error"]
      
      # res_p_val <- invert_p_val(object = res,
      #                           point_estimate = point_estimate,
      #                           se_guess = se_guess, 
      #                           clustid = preprocess$clustid,
      #                           fixed_effect = preprocess$fixed_effect, 
      #                           X = preprocess$X,
      #                           Y = preprocess$Y,
      #                           N = preprocess$N,
      #                           k = preprocess$k,
      #                           v = res$v,
      #                           param = param,
      #                           R0 = preprocess$R0,
      #                           B = B,
      #                           beta0 = preprocess$beta0,
      #                           alpha = preprocess$alpha, 
      #                           W = preprocess$W, 
      #                           n_fe = preprocess$n_fe, 
      #                           N_G = preprocess$N_G)
      res_p_val <- invert_p_val2(object = res, B = B, point_estimate = point_estimate, se_guess = se_guess, clustid = preprocess$clustid, alpha = preprocess$alpha)
      
  } else {
    res_p_val <- list(conf_int = NA, 
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
  
  invisible(res_final)
}
