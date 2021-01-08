boottest.lm <- function(object, 
                        clustid, 
                        param, 
                        B,
                        conf_int = NULL, 
                        seed = NULL, 
                        beta0 = NULL, 
                        alpha = NULL, 
                        type = "rademacher",
                        ...){
  
  #'Conducts wild cluster bootstrap inference for object of class lm.
  #'@param object An object of class lm
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param alpha A numeric between 0 and 1. E.g. alpha = 0.05 returns 0.95% confidence intervals. By default, alpha = 0.05.
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param type character or function. The character string specifies the type of boostrap to use: One of "rademacher", "mammen", "norm" and "webb". Alternatively, type can be a function(n) for drawing wild bootstrap factors. "rademacher" by default.
  #'@param ... Further arguments passed to or from other methods.
  #'@method boottest lm
  #'@return An object of class \code{boottest}
  #'\item{p_val}{The bootstrap p-value.}
  #'\item{t_stat}{The bootstrap t-statistic.}
  #'\item{conf_int}{The bootstrap confidence interval.}
  #'\item{param}{The tested parameter.}
  #'\item{N}{Sample size. Might differ from the regression sample size if the cluster variables contain NA values.}
  #'\item{B}{Number of Bootstrap Iterations.}
  #'\item{clustid}{Names of the cluster Variables.}
  #'\item{N_G}{Dimension of the cluster variables as used in boottest.}
  #'\item{alpha}{Significance level used in boottest.}
  #'\item{type}{Distribution of the bootstrap weights.}
  #'\item{p_test_vals}{All p-values calculated while calculating the confidence interval.}
  #'\item{test_vals}{All t-statistics calculated while calculating the confidence interval.}
  #'\item{regression}{The regression object used in boottest.}
  #'\item{call}{Function call of boottest.}
  
  #'@importFrom sandwich vcovCL 
  #'@importFrom lmtest coeftest
  #'@export
  #'@section Confidence Intervals: 
  #'\code{boottest} computes confidence intervals by inverting p-values. In practice, the following procedure is used: 
  #'\itemize{
  #'\item Based on an initial guess for starting values, calculate p-values for 26 equal spaced points between the starting values. 
  #'\item Out of the 26 calculated p-values, find the two pairs of values x for which the corresponding p-values px cross the significance level alpha.
  #'\item Feed the two pairs of x into an numerical root finding procedure and solve for the root. boottest currently relies on \code{stats::uniroot} and sets an absolute tolerance of 1e-06 and stops the procedure after 10 iterations.
  #'}
  #'@section Standard Errors: 
  #'\code{boottest} does not calculate standard errors.
  #'@references Roodman et al., 2019, "Fast and wild: Bootstrap inference in Stata using boottest", The Stata Journal. (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
  #'@examples
  #'library(fwildclusterboot)
  #'voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345)
  #'lm_fit <-lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
  #'boot1 <- boottest(lm_fit, B = 10000, param = "treatment", clustid = "group_id1")
  #'boot2 <- boottest(lm_fit, B = 10000, param = "treatment", clustid = c("group_id1", "group_id2"))
  #'boot3 <- boottest(lm_fit, B = 10000, param = "treatment", clustid = c("group_id1", "group_id2"), alpha = 0.2, seed = 8, beta0 = 2)
  #'summary(boot1)
  #'tidy(boot1)
  #'plot(boot1)

  call <- match.call()
  
  check_arg(clustid, "character scalar | character vector")
  check_arg(param, "scalar character")
  check_arg(B, "scalar integer") 
  check_arg(alpha, "scalar numeric")
  check_arg(weights, "NULL")
  check_arg(conf_int, "logical scalar | NULL")
  check_arg(seed, "scalar integer | NULL")
  check_arg(beta0, "numeric scalar | NULL")
  
  if((conf_int == TRUE || is.null(conf_int)) & B <= 100){
    stop("The function argument B is smaller than 100. The number of bootstrap iterations needs to be 100 or higher in order to guarantee that the root
         finding procudure used to find the confidence set works properly.", 
         call. = FALSE)
  }
  if(!is.null(alpha) & (alpha <= 0 || alpha >= 1)){
    stop("The function argument alpha is outside of the unit interval (0, 1). Please specify alpha so that it is within the unit interval.", 
         call. = FALSE)
  }
  
  
  # throw error if specific function arguments are used in lm() call
  call_object <- names(object$call)[names(object$call) != ""]
  banned_fun_args <- c("contrasts", "subset", "offset", "x", "y", "weights")
  if(sum(call_object %in% banned_fun_args) > 0){
    stop(paste("boottest.lm currently does not accept objects of type fixest with function arguments", 
               paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "), "and", banned_fun_args[length(banned_fun_args)], "."), 
         call. = FALSE)
  }

  
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
  
  
  if(((1 - preprocess$alpha) * (B + 1)) %% 1 != 0){
    message(paste("Note: The bootstrap usually performs best when the confidence level (here,", 1 - preprocess$alpha, "%) times the number of replications plus 1 (", B, "+ 1 = ",B + 1,") is an integer."))
  }
  
  #   # boot algoritm
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
      vcov <- suppressWarnings(vcovCL(object, cluster =  clustid_fml))
      coefs <- suppressWarnings(coeftest(object, vcov))
      se_guess <- coefs[param, "Std. Error"]
      if(is.na(se_guess)){
        se_guess <- object$se[param]
      }
      
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
                      call = call, 
                      alpha = preprocess$alpha, 
                      type = type)
  }

  
  class(res_final) <- "boottest"
  
  invisible(res_final)
}
