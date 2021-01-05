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
                           type = "rademacher",
                           ...){
  
  
  #' Computes wild cluster bootstrap for object of class fixest
  #'@param object An object of class fixest. Note: advanced formula tools in fixest or vectorized formulas are currently not supported.
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param fe A character scalar. Fixed effect to be projected out in the bootstrap
  #'@param alpha A numeric between 0 and 1. E.g. alpha = 0.05 returns 0.95% confidence intervals. By default, alpha = 0.05.
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param type character or function. The character string specifies the type of boostrap to use: One of "rademacher", "mammen", "norm" and "webb". Alternatively, type can be a function(n) for drawing wild bootstrap factors. "rademacher" by default.
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
          call. = FALSE)
   }
   if(!is.null(alpha) & (alpha < 0 || alpha > 1)){
     stop("The function argument alpha is outside of the unit interval (0, 1). Please specify alpha so that it is within the unit interval.", 
          call. = FALSE)
   }
   
   if(!is.null(fe) && fe %in% clustid){
     stop(paste("The function argument fe =", fe, "is contained in the clustering variables. This is not allowed. Please set fe to another factor variable or NULL."), 
          call. = FALSE)
   }
   
   # throw error if specific function arguments are used in feols() call
   call_object <- names(object$call)[names(object$call) != ""]
   banned_fun_args <- c("offset", "subset", "split", "fsplit", "panel.id", "demeaned")
   if(sum(call_object %in% banned_fun_args) > 0){
     stop("boottest.fixest currently does not accept objects of type fixest with function arguments 
          offset, subset, split, fsplit, panel.id & demeaned.", 
          call. = FALSE)
   }
   

   
   deparse_fml <- paste(deparse(object$fml_all, width.cutoff = 500), collapse="")
   #deparse_fml <- deparse(object$fml)
   if(grepl("[",deparse_fml, fixed = TRUE) || 
      grepl("i(", deparse_fml, fixed = TRUE) ||
      grepl("c(", deparse_fml, fixed = TRUE) ||
      grepl("^", deparse_fml, fixed = TRUE)){
     stop("Advanced formula notation in fixest / fixest (i(), ^, [x] and vectorized formulas via c(),) is currently not supported in boottest.")
   }
   
  preprocess <- preprocess(object = object, 
                                  param = param,
                                  clustid = clustid,
                                  beta0 = beta0,
                                  alpha = alpha, 
                                  fe = fe, 
                                  seed = seed)
  
  
  clustid_dims <- preprocess$clustid_dims
  # Invert p-value
  point_estimate <- object$coefficients[param]
  
  # if(clustid_dims == 1){
  #   # boot algoritm
  
  if(((1 - preprocess$alpha) * (B + 1)) %% 1 != 0){
    message(paste("Note: The bootstrap usually performs best when the confidence level (here,", 1 - preprocess$alpha, "%) times the number of replications plus 1 (", B, "+ 1 = ",B + 1,") is an integer."))
  }
  
  N_G_2 <- 2^max(preprocess$N_G)
  if(N_G_2 < B){
    warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution. Therefore, 
                  B = ", N_G_2, "."), 
            call. = FALSE)
    B <- N_G_2
  }

  
  # returns function
  # function taken from the sandwich package' vcovBS.lm function
  wild_draw_fun <- switch(type, 
                          rademacher = function(n) sample(c(-1,1), n, replace = TRUE), 
                          mammen = function(n) sample(c(-1,1) * (sqrt(5) + c(-1, 1))/2, n, replace = TRUE, prob = (sqrt(5) + c(1, -1))/(2 * sqrt(5))),
                          norm = function(n) rnorm(n), 
                          webb = function(n) sample(c(-sqrt((3:1)/2), sqrt((1:3)/2)), n, replace = TRUE), 
                          wild_draw_fun)
  
  res <- boot_algo2(preprocess, boot_iter = B, wild_draw_fun = wild_draw_fun)

  # compute confidence sets
  if(is.null(conf_int) || conf_int == TRUE){
    # calculate guess for covariance matrix and standard errors
    #vcov <- sandwich::vcovCL(object, cluster = clustid)
    #coefs <- lmtest::coeftest(object, vcov)
    #se_guess <- coefs[param, "Std. Error"]
    
    se_guess <- object$se[param]
    
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
                      call = call, 
                      type = type)
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
                      call = call, 
                      type = type)
  }
  
  
  class(res_final) <- "boottest"
  
  invisible(res_final)

} 
