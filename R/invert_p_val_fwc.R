invert_p_val.algo_oneclust <- function(object, point_estimate, se_guess, clustid, fixed_effect, X, Y, N, k, param, R0, B, v, Xr, alpha, beta0, W, n_fe, N_G){
  
  #' Inverts the bootstrap p-value and calculates confidence sets
  #'@param object A regression object of class lm, feols or felm
  #'@param clustid A vector with the clusters
  #'@param fixed_effect Fixed effect to be projected out in bootstrap
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
  #'@import dreamerr
  
  check_arg(point_estimate, "numeric scalar")
  check_arg(se_guess, "numeric scalar")
  check_arg(clustid, "data.frame")
  check_arg(X, "numeric matrix")
  check_arg(Y, "numeric vector | numeric matrix")
  check_arg(N, "numeric scalar")
  check_arg(k, "numeric scalar")
  check_arg(R0, "numeric vector | logical vector")
  check_arg(v, "numeric matrix")
  check_arg(Xr, "numeric matrix")
  check_arg(alpha, "numeric scalar")
  check_arg(beta0, "numeric scalar")

    
  if(alpha > 1 | alpha < 0){stop("Significance level needs to be between 0 and 1.")}

  # --------------------------------------------------------------------------------------------- #
  # start inversion 

  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  #Xr1 <- X
  #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
  
  # small sample correction for clusters 
  #G <- sapply(clustid, function(x) length(unique(x)))
  #small_sample_correction <- G / (G - 1)
  
  Q <- Y - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Y))
  P <- Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Xr0)) - Xr0
  
  # v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  # v[,1] <- 1
  invXX <- solve(t(X) %*% X) # k x k matrix
  XinvXXr <- X %*% (invXX %*% R0) # N x 1
  
  SXinvXXRX <- collapse::fsum(matrix(rep(XinvXXr, k), N, k) * X, clustid)  
  
  SXinvXXRX_invXX <- SXinvXXRX  %*% invXX


  XinvXXr <- as.vector(XinvXXr)
  
  p_val_null <- function(beta0, Q, P, R0, X, XinvXXr, clustid, 
                         SXinvXXRu_prep, v, B){
    
    u_hat <- Q + P %*% matrix(beta0, 1, length(beta0))

    
    SXinvXXRu <- collapse::fsum(XinvXXr * u_hat  , clustid)

    if(ncol(SXinvXXRu) == 1){
      SXinvXXRu <- as.vector(SXinvXXRu)
    }  
    diag_SXinvXXRu <- Matrix::Diagonal(N_G, SXinvXXRu)
    
    
    # if model with fixed effect
    if(!is.null(W)){
      S_XinvXXR_F <- crosstab2(as.matrix(XinvXXr), var1 = clustid, var2 = fixed_effect)
      S_Wu_F <- crosstab2(as.matrix(W %*% u_hat), var1 = clustid, var2 = fixed_effect)
      prod <- S_XinvXXR_F %*% t(S_Wu_F)
      diag_SXinvXXRu <- diag_SXinvXXRu - prod
    } 
    
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


    # for(i in 1:length(test_vals)){
    #   p[i] <- p_val_null_x(test_vals[i]) 
    # }
    
    p <- lapply(1:length(test_vals), function(i){ p_val_null_x(test_vals[i])})
    p <- do.call("c", p)  
    
    
      
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
    
        tmp <- stats::uniroot(f = p_val_null_x, lower = min(x), upper = max(x), tol = 1e-6, maxiter = 10)
    
    tmp$root
  })

  conf_int <- unlist(res)

  res_all <- list(conf_int = conf_int, 
                  p_test_vals = p, 
                  test_vals = test_vals)
  
  res_all

}



invert_p_val.algo_multclust <- function(object, point_estimate, se_guess, clustid, fixed_effect, v,X, Y, N, k, param, R0, B, beta0, alpha, W, n_fe, N_G){
  
  #' Inverts the bootstrap p-value and calculates confidence sets
  #'@param object A regression object of class lm, feols or felm
  #'@param clustid A vector with the clusters
  #'@param fixed_effect Fixed effect to be projected out in bootstrap
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
  
  check_arg(point_estimate, "numeric scalar")
  check_arg(se_guess, "numeric scalar")
  check_arg(clustid, "data.frame")
  check_arg(X, "numeric matrix")
  check_arg(Y, "numeric vector | numeric matrix")
  check_arg(N, "numeric scalar")
  check_arg(k, "numeric scalar")
  check_arg(R0, "numeric vector | logical vector")
  check_arg(v, "numeric matrix")
  check_arg(Xr, "numeric matrix")
  check_arg(alpha, "numeric scalar")
  check_arg(beta0, "numeric scalar")
  #check_arg(parallel, "logical scalar | NULL")
  
  if(alpha > 1 | alpha < 0){stop("Significance level needs to be between 0 and 1.")}
  
 
  
  
  # --------------------------------------------------------------------------------------------- #
  # start inversion 
  
  # error under the null hypothesis
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  #Xr1 <- X
  #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
  
  # small sample correction for clusters 
  G <- sapply(clustid, function(x) length(unique(x)))
  small_sample_correction <- G / (G - 1)
  
  Q <- Y - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Y))
  P <- Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Xr0)) - Xr0
  
  # v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  # v[,1] <- 1
  invXX <- solve(t(X) %*% X) # k x k matrix
  XinvXXr <- X %*% (invXX %*% R0) # N x 1
  
  p_val_null <- function(beta0, Q, P, R0, X, XinvXXr, clustid, 
                         v, B, small_sample_correction){
    
    # error under the null hypothesis
    #u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
    u_hat <- Q + P %*% matrix(beta0, 1, length(beta0))
    uX <- matrix(rep(u_hat, 1), N, k) * X
    SuX <- collapse::fsum(uX, clustid$clustid)
    tSuX <- t(SuX)
    XinvXXRu <- as.vector(XinvXXr * matrix(rep(u_hat, 1), N, 1))
    #diag_XinvXXRuS <- t(collapse::fsum(diag(as.vector(XinvXXRu)), clustid$clustid))
    
    
    tKK <- list()
    
    XinvXXrX <- matrix(rep(XinvXXr, k), N, k) * X
    XinvXXRuS <- t(collapse::fsum(XinvXXRu, clustid$clustid))
    SXinvXXRu <- XinvXXRuS
    #SXinvXXRu <- t(collapse::fsum(XinvXXRu, clustid$clustid))
    
    diag_XinvXXRuS <- Matrix::t(Matrix.utils::aggregate.Matrix(Matrix::Diagonal(N, as.vector(XinvXXRu)), clustid$clustid))
    
    
    if(is.null(W)){
      
      
      for(x in names(clustid)){
        #S_diag_XinvXXRu_S <- collapse::fsum(diag_XinvXXRuS, clustid[x])
        #SXinvXXRu <-collapse::fsum(XinvXXr , clustid[x])
        
        S_diag_XinvXXRu_S <- aggregate.Matrix(diag_XinvXXRuS, clustid[x]) # c* x c
        SXinvXXrX <-  collapse::fsum(XinvXXrX, clustid[x]) #c* x f
        K <- S_diag_XinvXXRu_S - SXinvXXrX %*% invXX %*% tSuX # c* x x
        tKK[[x]] <- small_sample_correction[x] * Matrix::t(K) %*% K # here: add small sample df correction
      }
    } else if(!is.null(W)){
      
      # does not need to be regularly re-computed
      S_Wu_F <- crosstab2(as.matrix(W %*% u_hat), var1 = clustid["clustid"], var2 = fixed_effect) # f x c*
      for(x in names(clustid)){
        
        #S_diag_XinvXXRu_S <- crosstab2(XinvXXRu, var1 = clustid["clustid"], var2 = clustid[x])
        S_diag_XinvXXRu_S <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS, clustid[x]) # c* x c
        # start projecting out fixed effect
        S_XinvXXR_F <- crosstab2(XinvXXr, var1 = clustid[x], var2 = fixed_effect) # c x f
        prod <- S_Wu_F %*% t(S_XinvXXR_F) # c* x c*
        #diag_SXinvXXRu <- Matrix::Diagonal(N_G["clustid"], SXinvXXRu)
        S_diag_XinvXXRu_S <- S_diag_XinvXXRu_S - t(prod)        
        # stop projecting out fixed effect
        
        SXinvXXrX <-  collapse::fsum(XinvXXrX, clustid[x])
        K <- S_diag_XinvXXRu_S - SXinvXXrX %*% invXX %*% tSuX
        tKK[[x]] <- small_sample_correction[x] * Matrix::t(K) %*% K # here: add small sample df correction
        
      }
    }
    
    
    tKK_sum <- Matrix::t(Reduce("+", tKK))
    denom <- Matrix::colSums(v * tKK_sum %*% v)
    numer <- SXinvXXRu %*% v 
    
    t <- abs(numer) / denom
    
    t_boot <- t[2:(B + 1)]
    p_val <- mean(abs(t[1] - beta0) < (t_boot))
    
    res <- list(p_val = p_val, 
                t = t, 
                t_boot = t_boot)
    res
  }
  
    
  
  
  # can be smaller than zero bc of -0.5
  p_val_null_x <- function(beta0){
    p_val_null(beta0, Q = Q, P = P, R0 = R0, X = X, XinvXXr = XinvXXr, clustid = clustid, v = v, B = B, small_sample_correction = small_sample_correction)$p_val - alpha
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

    #if(parallel == TRUE){
    #    #tic()
    #    cl <- parallel::makeCluster(2)
    #    parallel::clusterExport(cl, list("p_val_null", "p_val_null_x","invXX", "N","k", "Q", "P", "test_vals", "XinvXXr", "clustid", "v", "B", "alpha", "X", "small_sample_correction"))
    #    #clusterEvalQ(cl, library("thePackage"))
    #    p <- parallel::parLapply(cl, 1:length(test_vals), function(i){ p_val_null_x(test_vals[i])})
    #    p <- do.call("c", p)
    #    parallel::stopCluster(cl)   
    #    #toc()
    #} else {
      #tic()
      p <- rep(NaN, length(test_vals))
      for(i in 1:length(test_vals)){
        p[i] <- p_val_null_x(test_vals[i]) 
      }
      #toc()
    #}
   

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
