invert_p_val.algo_oneclust <- function(object, point_estimate, se_guess, clustid, X, Y, N, k, param, R0, B, invXX, v, Xr, XinvXXr, SXinvXXRX, alpha){
  
  #' Inverts the bootstrap p-value and calculates confidence sets
  #'@param object A regression object of class lm, feols or felm
  #'@param clustid A vector with the clusters
  #'@param X the design matrix with the (potentially demeand) covariates
  #'@param Y A numeric vector containing the outcome variable 
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param R0 A vector with the test constraint. Dimension (numb_covariates - numb_fe) x 1. 0 for covariate "param", else 1
  #'@param B An integer. Number of bootstrap iterations
  #'@param N An integer. Number of observations
  #'@param k An integer. Number of covariates (excluding fixed effects that are projected out)
  #'@param seed An integer. Seed.
  #'@param N_g An integer. Number of clusters.
  #'@param v A matrix. Draw from bootstrap distribution
  #'@param Xr A matrix. R0 %*% X
  #'@param XinvXXr A matrix. see boottest()
  #'@param SXinvXXRX A matrix. see boottest() for computation
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@import pracma
  
  if(alpha > 1 | alpha < 0){stop("Significance level needs to be between 0 and 1.")}
  
 # if(class(object) == "lm"){
    #lm_robust_fit <- estimatr::lm_robust(eval(object$call$formula), clusters = as.factor(clustid$clustid), data = data, se_type = "stata")
    #estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
    #st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  #} else if(class(object) == "lm_robust"){
    #lm_robust_fit <-  estimatr::lm_robust(object$call$formula, 
    #                           clusters = eval(object$call$clusters), 
    #                           data = data, 
    #                           se_type = "stata")
    #estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
    #st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  #} else if(class(object) == "felm"){
  #  lm_robust_fit <-  estimatr::lm_robust(formula(Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment"))), lhs = 1, rhs = 1), 
  #                             clusters = clustid[, "clustid"], 
  #                             data = data, se_type = "stata")
  #  estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
  #  st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  #} else if(class(object) == "fixest"){
    #fit <- fixest:::summary.fixest(object, se = "cluster", cluster = clustid)
    #estimate <- fit$coefficients[names(object$coefficients) == param]
    #st_error_guess <- fit$se[names(object$se) == param]
  #} else {
  #  stop("Function only designed for objects of type lm, lm_robust, felm and feols.")
  #}

  
  # --------------------------------------------------------------------------------------------- #
  # start inversion 

  SXinvXXRX_invXX <- SXinvXXRX  %*% invXX
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  
  Q <- Y - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Y))
  P <- Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Xr0)) - Xr0
  
  XinvXXr <- as.vector(XinvXXr)
  
  p_val_null <- function(beta0, Q, P, R0, X, XinvXXr, clustid, 
                         SXinvXXRu_prep, v, B){
    
    u_hat <- Q + P %*% matrix(beta0, 1, length(beta0))

    # SXinvXXRu_prep <- data.table::data.table(prod = as.vector(XinvXXr) * u_hat  , clustid = clustid) 
    # SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    # if(ncol(SXinvXXRu) == 1){
    #   SXinvXXRu <- as.vector(SXinvXXRu)
    # }
    
    SXinvXXRu <- collapse::fsum(XinvXXr * u_hat  , clustid)
    if(ncol(SXinvXXRu) == 1){
      SXinvXXRu <- as.vector(SXinvXXRu)
    }  
    
    #SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
    #SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL]) 
   
    SXu <- collapse::fsum(X * matrix(rep(u_hat, k), N, k), clustid)
    
    numer <- SXinvXXRu %*% v 
    J <- (diag(SXinvXXRu) - SXinvXXRX_invXX %*% t(SXu)) %*% v  
    t <- abs(numer)  / sqrt(colSums(J * J))    # note: absolute value is taken here - no negative t-stats
    t_boot <- t[2:(B + 1)]
    mean(abs(t[1] - beta0) < (t_boot))
  }
  
  # can be smaller than zero bc of -0.5
  p_val_null_x <- function(beta0){
    p_val_null(beta0, P = P, Q = Q, R0 = R0, X = X, XinvXXr = XinvXXr, clustid = clustid, 
               SXinvXXRu_prep = SXinvXXRu_prep, v = v, B = B) - alpha
  }
  
  # p-value must cross alpha
  check <- FALSE
  inflate_se <- c(2, 3, 5, 10)
  j <- 1
  while(check == FALSE){
    
    if(j > 4){
      break("Boottest confidence set calculation fails because no p-value < alpha could succesfully
            be guessed.")
    }
    # start guesses by taking sandwich cluster confidence intervals + inflation factor
    starting_vals <- as.numeric(point_estimate + c(-inflate_se[j], inflate_se[j]) * se_guess)
    # take 25 starting values in between the guesses
    test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
    
    # later: don't have to evaluate all guesses at all points - extreme points suffice - if < alpha at both extreme points
    # then evaluate all 26 points
    
    #min_test_val <- min(test_vals)
    #max_test_val <- max(test_vals)
    
    #p_val_null_x(min_test_val) + alpha <
    #benchmark(p_val_null_x(test_vals[1]))
    # get test values

    # calculate the p-values for all 26 guesses
    p <- rep(NaN, length(test_vals))
    
    #pb = txtProgressBar(min = 0, max = length(test_vals), initial = 0, style = 3) 
    for(i in 1:length(test_vals)){
      p[i] <- p_val_null_x(test_vals[i]) 
    #  setTxtProgressBar(pb,i)
    }
    #close(pb)
    
    # substract alpha in function so that I will not need to 
    # do it in root finding algorithm, but then I will need to add 
    # alpha here
    p <- p + alpha 
    
    #if(sum(p < alpha) < 1){warning("Need to djust starting values: they are not p < alpha. Therefore, choose more
    #                              extreme starting values.")}
    
    crossings <-  (p < alpha) - (p > alpha)
    
    x_crossings <- rep(NA, length(test_vals))
    for(i in 1:25){
      x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
    }
    
    check <- sum(x_crossings == 1, na.rm = TRUE) == 4
    j <- j + 1
    check    
  }
 


  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]

  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]

  if(length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0){
    stop("test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property x1 < 0.05 < x2 has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
         will not work.")
  }  
  
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
    #tmp <- secant_rel(f = p_val_null_x, x1 = min(x), x2 = max(x), B = B)
    #tmp <- secant_method(f = p_val_null_x, x0 = min(x), x1 = max(x))
    #tmp
    #tmp <-  NLRoot::SMfzero(p_val_null_x , x1 = min(x), x2 = max(x), num = 10, eps = 1/(B*1.0000001))
    #tmp <- secant_method(p_val_null_x, x1 = min(x), x2 = max(x))
    #tmp
    #tmp <- try(pracma::newtonRaphson(p_val_null_x, x0 =  x, dfun = NULL, maxiter = 10, tol = 1/B))
    #tmp$root
    #tmp <- pracma::fzero(p_val_null_x , x = x, maxiter = 10, tol = 1 / B)
    #tmp$x
    #tmp <- pracma::bisect(p_val_null_x , a = min(x), b = max(x), tol = 1e-6)
    #tmp <- pracma::secant(p_val_null_x , a = min(x), b = max(x), maxiter = 10, tol = 1e-6)
    #tmp <- pracma::muller(p_val_null_x , p0 = min(x), p1 = max(x), tol = 1e-6, maxiter = 10)
    tmp <- stats::uniroot(f = p_val_null_x, lower = min(x), upper = max(x), tol = 1e-6, maxiter = 10)
    
    tmp$root
  })

  conf_int <- unlist(res)

  res_all <- list(conf_int = conf_int, 
                  p_test_vals = p, 
                  test_vals = test_vals)
  
  res_all

}



invert_p_val.algo_multclust <- function(object, point_estimate, se_guess, clustid, X, Y, N, k, param, R0, B, invXX, v, Xr, XinvXXr, SXinvXXRX, alpha){
  
  #' Inverts the bootstrap p-value and calculates confidence sets
  #'@param object A regression object of class lm, feols or felm
  #'@param clustid A vector with the clusters
  #'@param X the design matrix with the (potentially demeand) covariates
  #'@param Y A numeric vector containing the outcome variable 
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param R0 A vector with the test constraint. Dimension (numb_covariates - numb_fe) x 1. 0 for covariate "param", else 1
  #'@param B An integer. Number of bootstrap iterations
  #'@param N An integer. Number of observations
  #'@param k An integer. Number of covariates (excluding fixed effects that are projected out)
  #'@param seed An integer. Seed.
  #'@param N_g An integer. Number of clusters.
  #'@param v A matrix. Draw from bootstrap distribution
  #'@param Xr A matrix. R0 %*% X
  #'@param XinvXXr A matrix. see boottest()
  #'@param SXinvXXRX A matrix. see boottest() for computation
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@import pracma
  
  if(alpha > 1 | alpha < 0){stop("Significance level needs to be between 0 and 1.")}
  
  # if(class(object) == "lm"){
  #lm_robust_fit <- estimatr::lm_robust(eval(object$call$formula), clusters = as.factor(clustid$clustid), data = data, se_type = "stata")
  #estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
  #st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  #} else if(class(object) == "lm_robust"){
  #lm_robust_fit <-  estimatr::lm_robust(object$call$formula, 
  #                           clusters = eval(object$call$clusters), 
  #                           data = data, 
  #                           se_type = "stata")
  #estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
  #st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  #} else if(class(object) == "felm"){
  #  lm_robust_fit <-  estimatr::lm_robust(formula(Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment"))), lhs = 1, rhs = 1), 
  #                             clusters = clustid[, "clustid"], 
  #                             data = data, se_type = "stata")
  #  estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
  #  st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  #} else if(class(object) == "fixest"){
  #fit <- fixest:::summary.fixest(object, se = "cluster", cluster = clustid)
  #estimate <- fit$coefficients[names(object$coefficients) == param]
  #st_error_guess <- fit$se[names(object$se) == param]
  #} else {
  #  stop("Function only designed for objects of type lm, lm_robust, felm and feols.")
  #}
  
  
  # --------------------------------------------------------------------------------------------- #
  # start inversion 
  
  SXinvXXRX_invXX <- SXinvXXRX  %*% invXX
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  
  Q <- Y - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Y))
  P <- Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Xr0)) - Xr0
  
  XinvXXr <- as.vector(XinvXXr)
  
  p_val_null <- function(beta0, Q, P, R0, X, XinvXXr, clustid, 
                         SXinvXXRu_prep, v, B){
    
    u_hat <- Q + P %*% matrix(beta0, 1, length(beta0))
    
    # SXinvXXRu_prep <- data.table::data.table(prod = as.vector(XinvXXr) * u_hat  , clustid = clustid) 
    # SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    # if(ncol(SXinvXXRu) == 1){
    #   SXinvXXRu <- as.vector(SXinvXXRu)
    # }
    
    SXinvXXRu <- collapse::fsum(XinvXXr * u_hat  , clustid)
    if(ncol(SXinvXXRu) == 1){
      SXinvXXRu <- as.vector(SXinvXXRu)
    }  
    
    #SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
    #SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL]) 
    
    SXu <- collapse::fsum(X * matrix(rep(u_hat, k), N, k), clustid)
    
    numer <- SXinvXXRu %*% v 
    J <- (diag(SXinvXXRu) - SXinvXXRX_invXX %*% t(SXu)) %*% v  
    t <- abs(numer)  / sqrt(colSums(J * J))    # note: absolute value is taken here - no negative t-stats
    t_boot <- t[2:(B + 1)]
    mean(abs(t[1] - beta0) < (t_boot))
  }
  
  # can be smaller than zero bc of -0.5
  p_val_null_x <- function(beta0){
    p_val_null(beta0, P = P, Q = Q, R0 = R0, X = X, XinvXXr = XinvXXr, clustid = clustid, 
               SXinvXXRu_prep = SXinvXXRu_prep, v = v, B = B) - alpha
  }
  
  # p-value must cross alpha
  check <- FALSE
  inflate_se <- c(2, 3, 5, 10)
  j <- 1
  while(check == FALSE){
    
    if(j > 4){
      break("Boottest confidence set calculation fails because no p-value < alpha could succesfully
            be guessed.")
    }
    # start guesses by taking sandwich cluster confidence intervals + inflation factor
    starting_vals <- as.numeric(point_estimate + c(-inflate_se[j], inflate_se[j]) * se_guess)
    # take 25 starting values in between the guesses
    test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
    
    # later: don't have to evaluate all guesses at all points - extreme points suffice - if < alpha at both extreme points
    # then evaluate all 26 points
    
    #min_test_val <- min(test_vals)
    #max_test_val <- max(test_vals)
    
    p_val_null_x(min_test_val) + alpha <
      #benchmark(p_val_null_x(test_vals[1]))
      # get test values
      
      # calculate the p-values for all 26 guesses
      p <- rep(NaN, length(test_vals))
    
    #pb = txtProgressBar(min = 0, max = length(test_vals), initial = 0, style = 3) 
    for(i in 1:length(test_vals)){
      p[i] <- p_val_null_x(test_vals[i]) 
      #  setTxtProgressBar(pb,i)
    }
    #close(pb)
    
    # substract alpha in function so that I will not need to 
    # do it in root finding algorithm, but then I will need to add 
    # alpha here
    p <- p + alpha 
    
    #if(sum(p < alpha) < 1){warning("Need to djust starting values: they are not p < alpha. Therefore, choose more
    #                              extreme starting values.")}
    
    crossings <-  (p < alpha) - (p > alpha)
    
    x_crossings <- rep(NA, length(test_vals))
    for(i in 1:25){
      x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
    }
    
    check <- sum(x_crossings == 1, na.rm = TRUE) == 4
    j <- j + 1
    check    
  }
  
  
  
  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]
  
  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]
  
  if(length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0){
    stop("test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property x1 < 0.05 < x2 has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
         will not work.")
  }  
  
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
    #tmp <- secant_rel(f = p_val_null_x, x1 = min(x), x2 = max(x), B = B)
    #tmp <- secant_method(f = p_val_null_x, x0 = min(x), x1 = max(x))
    #tmp
    #tmp <-  NLRoot::SMfzero(p_val_null_x , x1 = min(x), x2 = max(x), num = 10, eps = 1/(B*1.0000001))
    #tmp <- secant_method(p_val_null_x, x1 = min(x), x2 = max(x))
    #tmp
    #tmp <- try(pracma::newtonRaphson(p_val_null_x, x0 =  x, dfun = NULL, maxiter = 10, tol = 1/B))
    #tmp$root
    #tmp <- pracma::fzero(p_val_null_x , x = x, maxiter = 10, tol = 1 / B)
    #tmp$x
    #tmp <- pracma::bisect(p_val_null_x , a = min(x), b = max(x), tol = 1e-6)
    #tmp <- pracma::secant(p_val_null_x , a = min(x), b = max(x), maxiter = 10, tol = 1e-6)
    #tmp <- pracma::muller(p_val_null_x , p0 = min(x), p1 = max(x), tol = 1e-6, maxiter = 10)
    tmp <- stats::uniroot(f = p_val_null_x, lower = min(x), upper = max(x), tol = 1e-6, maxiter = 10)
    
    tmp$root
  })
  
  conf_int <- unlist(res)
  
  res_all <- list(conf_int = conf_int, 
                  p_test_vals = p, 
                  test_vals = test_vals)
  
  res_all
  
}
