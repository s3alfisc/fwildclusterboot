boottest.fixest  <- function(object, 
                           clustid, 
                           param, 
                           B,
                           fe = NULL, 
                           alpha = NULL, 
                           weights = NULL,
                           conf_int = NULL, 
                           seed = NULL, 
                           beta0 = 0,
                           ...){
  
  
  #' Computes wild cluster bootstrap for object of class fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param fe A character scalar. Fixed effect to be projected out in the bootstrap
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param ... Further arguments passed to or from other methods.
  #'@return An object of class boottest
  #'@import dreamerr
  #'@export
  #'@method boottest fixest
  #'@examples
  #'library(fwildclusterboot)
  #'library(fixest)
  #'voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345)
  #'feols_fit <-feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", weights = NULL, data = voters)
  #'boot1 <- boottest(feols_fit, B = 10000, param = "treatment", clustid = "group_id1")
  #'boot2 <- boottest(feols_fit, B = 10000, param = "treatment", clustid = c("group_id1", "group_id2"))
  #'boot3 <- boottest(feols_fit, B = 10000, param = "treatment", clustid = c("group_id1", "group_id2"), fe = "Q1_immigration")
  #'boot4 <- boottest(feols_fit, B = 10000, param = "treatment", clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", alpha = 0.2, seed = 8, beta0 = 2)
  #'summary(boot1)
  #'tidy(boot1)
  #'plot(boot1)

  
 
  call <- match.call()
  
                # setwd("C:/Users/alexa/Dropbox/fwildclusterboot/R")
                # file.sources = list.files(pattern="*.R")
                #  sapply(file.sources, source, .GlobalEnv)
                #  # set.seed(6261)
                #  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345)
                #  voters[1:2, proposition_vote:=NA]
                #  voters[3, group_id1 := NA]      #  
                #  object <- feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", weights = NULL, data = voters)
                # clustid <- c("group_id1", "group_id2")
                #  fe = "Q1_immigration"
                #  param <- "treatment"
                #  B = 5000
                #  weights = NULL
                #  conf_int = NULL 
                #  debug = FALSE
                #  seed = NULL
                #  beta0 = NULL
                #  alpha = NULL
  
     # Step 1: check arguments of feols call
   check_arg(clustid, "character vector | scalar character")
   check_arg(param, "scalar character")
   check_arg(B, "scalar integer ") 
   check_arg(alpha, "scalar numeric | NULL")
   check_arg(weights, "NULL")
   check_arg(conf_int, "logical scalar | NULL")
   check_arg(seed, "scalar integer | NULL")
   check_arg(beta0, "numeric scalar | NULL")
   check_arg(fe, "character scalar | NULL")
  
   if((conf_int == TRUE || is.null(conf_int)) & B <= 100){
     stop("The function argument B is smaller than 100. The number of bootstrap iterations needs to be 100 or higher in order to guarantee that the root
         finding procudure used to find the confidence set works properly.", 
          .call = FALSE)
   }
   if(!is.null(alpha) & (alpha < 0 || alpha > 1)){
     stop("The function argument alpha is outside of the unit interval (0, 1). Please specify alpha so that it is within the unit interval.")
   }
   
   if(!is.null(fe) && fe %in% clustid){
     stop(paste("The function argument fe =", fe, "is contained in the clustering variables. This is not allowed. Please set fe to another factor variable or NULL."))
   }
   
   if(((1 - alpha) * (B + 1)) %% 1 != 0){
     warning(paste("The bootstrap usually performs best when the confidence level (here,", 1 - alpha, "%) times the number of replications plus 1 (", B, "+ 1 = ",B + 1,") is an integer."), 
             call. = FALSE)
   }
   
   # if(!is.null(panel_id)){
   #   stop(paste("boottest() currently does not work if an argument panel_id is applied to feols()."))
   # }
   # 
   # deparse_fml <- paste(deparse(object$fml, width.cutoff = 500), collapse="")
   # #deparse_fml <- deparse(object$fml)
   # if(grepl("[",deparse_fml) || 
   #    grepl("i(", deparse_fml) ||
   #    grepl("c(", deparse_fml) ||
   #    grepl("^", deparse_fml)){
   #   stop("Advanced formula notation in feols as c(), i(), ^ and [x] is not supported in boottest.")
   # }
   
  preprocess <- suppressWarnings(preprocess(object = object, 
                                  param = param,
                                  clustid = clustid,
                                  beta0 = beta0,
                                  alpha = alpha, 
                                  fe = fe, 
                                  seed = seed))
  
  
  clustid_dims <- preprocess$clustid_dims
  # Invert p-value
  point_estimate <- object$coefficients[param]
  
  # if(clustid_dims == 1){
  #   # boot algoritm
  
  N_G_2 <- 2^max(preprocess$N_G)
  if(N_G_2 < B){
    warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution. Therefore, 
                  B = ", N_G_2, "."))
    B <- N_G_2
  }

  
  res <- boot_algo2(preprocess, boot_iter = B)
  #boot_algo2(res_preprocess, boot_iter = B)$p_val


  # compute confidence sets
  
  if(is.null(conf_int) || conf_int == TRUE){
    # calculate guess for covariance matrix and standard errors
    #vcov <- sandwich::vcovCL(object, cluster = clustid)
    #coefs <- lmtest::coeftest(object, vcov)
    #se_guess <- coefs[param, "Std. Error"]
    
    se_guess <- object$se[param]
    
    
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
                      N_G = preprocess$N_G, 
                      alpha = preprocess$alpha,
                      call = call)
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
                      N_G = preprocess$N_G, 
                      alpha = preprocess$alpha,
                      call = call)
  }
  
  
  class(res_final) <- "boottest"
  
  invisible(res_final)

} 
