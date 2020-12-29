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
  XinvXXr <- as.vector(X %*% (invXX %*% R0)) # N x 1
  
  SXinvXXRX <- collapse::fsum(XinvXXr * X, clustid)  
  
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
      # needs to be clustid[, 1] for crosstab4 but not bor crosstab2
      S_XinvXXR_F <- crosstab2(as.matrix(XinvXXr), var1 = clustid, var2 = fixed_effect)
      S_Wu_F <- crosstab2(as.matrix(W %*% u_hat), var1 = clustid, var2 = fixed_effect)
      prod <- S_XinvXXR_F %*% t(S_Wu_F)
      diag_SXinvXXRu <- diag_SXinvXXRu - prod
    } 
    
    SXu <- collapse::fsum(X * as.vector(u_hat), clustid)
    
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
  inflate_se <- c(2, 3, 5, 10, 15, 25, 50, 100)
  len_inflate <- length(inflate_se)
  j <- 1

  while(check == FALSE){
      #print("check")
      if(j > len_inflate){
        break("Boottest confidence set calculation fails because no p-value < alpha could succesfully
            be guessed.")
      }
      # start guesses by taking sandwich cluster confidence intervals + inflation factor
      starting_vals <- as.numeric(point_estimate + c(-inflate_se[j], inflate_se[j]) * se_guess)
      #print(starting_vals)
      # take 25 starting values in between the guesses
      p_start <- rep(NaN, length(starting_vals))
      
      for(i in 1:length(starting_vals)){
        p_start[i] <- p_val_null_x(starting_vals[i]) 
      }
      p_start <- p_start + alpha 
      
      if(sum(p_start < alpha) == 2){
        check <- TRUE
      }
      j <- j + 1
      #print(p_start)
    }
    
    test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
    
    # later: don't have to evaluate all guesses at all points - extreme points suffice - if < alpha at both extreme points
    # then evaluate all 26 points
    p <- rep(NaN, length(test_vals))
    
    
    for(i in 2:(length(test_vals) - 1)){
      p[i] <- p_val_null_x(test_vals[i]) 
    }
    # substract alpha in function so that I will not need to 
    # do it in root finding algorithm, but then I will need to add 
    # alpha here
    p <- p + alpha 
    
    p[1] <- p_start[1]
    p[26] <- p_start[2]
    #if(sum(p < alpha) < 1){warning("Need to djust starting values: they are not p < alpha. Therefore, choose more
    #                              extreme starting values.")}
    
    # note: one alpha <= and one > to handle the case where p is exactly 0.05 - in this case
    # x crossing will not produce two pairs of (1, -1) and (-1, 1)
    crossings <-  (p <= alpha) - (p > alpha)
    
    x_crossings <- rep(NA, length(test_vals))
    for(i in 1:26){
      x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
    }
 


  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]

  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]

  if(length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0){
    stop("test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property |p(x1) < 0.05| has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
          procedure employed to invert p-values to compute confidence sets will not work.")
  }  
  
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
        tmp <- suppressWarnings(stats::uniroot(f = p_val_null_x, lower = min(x), upper = max(x), tol = 1e-6, maxiter = 10))
    
    tmp$root
  })

  conf_int <- unlist(res)

  res_all <- list(conf_int = conf_int, 
                  p_test_vals = p, 
                  test_vals = test_vals)
  
  res_all

}




#invert_p_val.algo_multclust <- function(object, point_estimate, se_guess, alpha, clustid){
invert_p_val2a.algo_multclust <- function(object, point_estimate, se_guess, clustid, fixed_effect, v,X, Y, N, k, param, R0, B, beta0, alpha, W, n_fe, N_G){
  
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
  
  
  
  # key idea: 
  # write numerator = A + B * r
  # write denominator as J*J 
  # J = C + D * r -> J *J ?  C*C + C*D*r+ D*D* r^2
  # J = K * v
  # t = (A + B * r) / sqrt(C*C + C*D*r+ D*D* r^2) 
  # my code equivalents: 
  # A: numer_a 
  # B: numer_b
  # C: K_a * v
  # D: K_b * v
  
  # --------------------------------------------------------------------------------------------- #
  # start inversion 
  boot_iter <- B
  rm(B)
  
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
  small_sample_correction <- small_sample_correction * c(rep(1, length(clustid) - 1), - 1)
  
  Q <- Y - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Y))
  P <- Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Xr0)) - Xr0
  
  # v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  # v[,1] <- 1
  invXX <- solve(t(X) %*% X) # k x k matrix#
  XinvXX <- X %*% invXX
  XinvXXr <- as.vector(X %*% (invXX %*% R0)) # N x 1
  XinvXXrX <- XinvXXr * X
  
  SuXa <- collapse::fsum( as.vector(Q) * X, clustid$clustid)
  XinvXXrQ <- XinvXXr * Q
  XinvXXrP <- XinvXXr * P
  
  XinvXXRuS_a <- collapse::fsum(XinvXXrQ, clustid$clustid)
  diag_XinvXXRuS_a <- Matrix::t(
    Matrix.utils::aggregate.Matrix(
      Matrix::Diagonal(N, 
                       as.vector(XinvXXrQ)),
      clustid$clustid)) # N x c*
  
  diag_XinvXXRuS_b <- Matrix::t(
    Matrix.utils::aggregate.Matrix(
      Matrix::Diagonal(N, 
                       as.vector(XinvXXrP)),
      clustid$clustid)) # N x c*  
  
  SXinvXXrX <- list()
  SXinvXXrX_invXX <- list()
  
  S_XinvXXR_F <- list()
  SXinvXXrX <- list()
  J_a <- list()
  K_b <- list()
  K_a <- list()
  C <- list()
  D <- list()
  CC <- list()
  DD <- list()
  CD <- list()
  
  #   # diag_XinvXXRuS_b <- Matrix::t(
  #   #   Matrix.utils::aggregate.Matrix(
  #   #     Matrix::Diagonal(N, 
  #   #                      as.vector(XinvXXr * P %*% matrix(beta0, 1, length(beta0)))),
  #   #     clustid$clustid)) # N x c*
  
  if(is.null(W)){
    for(x in names(clustid)){
      # all
      SXinvXXrX[[x]] <-  collapse::fsum(XinvXXrX, clustid[x]) #c* x f
      SXinvXXrX_invXX[[x]] <- SXinvXXrX[[x]] %*% invXX
      #S_XinvXXR_F <- crosstab2(XinvXXr, var1 = clustid[x], var2 = fixed_effect) # c x f
      #SXinvXXrX <-  collapse::fsum(XinvXXrX, clustid[x])
      # a
      #prod_a <- t(tcrossprod(S_Wu_F_a, S_XinvXXR_F))
      S_diag_XinvXXRu_S_a <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_a, clustid[x]) # c* x c
      #S_diag_XinvXXRu_S_a <- S_diag_XinvXXRu_S_a 
      K_a[[x]] <- S_diag_XinvXXRu_S_a  - tcrossprod(SXinvXXrX_invXX[[x]], SuXa) 
      #J_a[[x]] <- K_a %*% v
      # b
      #prod_b <- t(tcrossprod(S_Wu_F_b, S_XinvXXR_F))
      S_diag_XinvXXRu_S_b <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_b, clustid[x])
      #S_diag_XinvXXRu_S_b <- S_diag_XinvXXRu_S_b - prod_b
      K_b[[x]] <- S_diag_XinvXXRu_S_b - tcrossprod(SXinvXXrX_invXX[[x]], collapse::fsum(as.vector(P) * X, clustid$clustid)) 
      #print(x)
      #pracma::tic()
      C[[x]] <- eigenMatMult(as.matrix(K_a[[x]]), v) 
      D[[x]] <- eigenMatMult(as.matrix(K_b[[x]]), v) 
      #pracma::toc()
      #pracma::tic()
      CC[[x]] <- C[[x]] * C[[x]]
      DD[[x]] <- D[[x]] * D[[x]]
      CD[[x]] <- C[[x]] * D[[x]]
      #pracma::toc()
      
    }
  } else if(!is.null(W)){
    S_Wu_F_a <- crosstab2(as.matrix(W %*% Q), var1 = clustid["clustid"], var2 = fixed_effect) # f x c*
    S_Wu_F_b <- crosstab2(as.matrix(W %*% P), var1 = clustid["clustid"], var2 = fixed_effect) # f x c*
    
    pracma::tic()
    for(x in names(clustid)){
      # all
      SXinvXXrX[[x]] <-  collapse::fsum(XinvXXrX, clustid[x]) #c* x f
      SXinvXXrX_invXX[[x]] <- SXinvXXrX[[x]] %*% invXX
      S_XinvXXR_F <- crosstab2(XinvXXr, var1 = clustid[x], var2 = fixed_effect) # c x f
      #SXinvXXrX <-  collapse::fsum(XinvXXrX, clustid[x])
      # a
      prod_a <- t(tcrossprod(S_Wu_F_a, S_XinvXXR_F))
      S_diag_XinvXXRu_S_a <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_a, clustid[x]) # c* x c
      S_diag_XinvXXRu_S_a <- S_diag_XinvXXRu_S_a - prod_a
      K_a[[x]] <- S_diag_XinvXXRu_S_a  - tcrossprod(SXinvXXrX_invXX[[x]], SuXa) 
      #J_a[[x]] <- K_a %*% v
      # b
      prod_b <- t(tcrossprod(S_Wu_F_b, S_XinvXXR_F))
      S_diag_XinvXXRu_S_b <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_b, clustid[x])
      S_diag_XinvXXRu_S_b <- S_diag_XinvXXRu_S_b - prod_b
      K_b[[x]] <- S_diag_XinvXXRu_S_b - tcrossprod(SXinvXXrX_invXX[[x]], collapse::fsum(as.vector(P) * X, clustid$clustid)) 
      #print(x)
      #pracma::tic()
      C[[x]] <- eigenMatMult(as.matrix(K_a[[x]]), v) 
      D[[x]] <- eigenMatMult(as.matrix(K_b[[x]]), v) 
      #pracma::toc()
      #pracma::tic()
      CC[[x]] <- C[[x]] * C[[x]]
      DD[[x]] <- D[[x]] * D[[x]]
      CD[[x]] <- C[[x]] * D[[x]]
      #pracma::toc()
      
    }
    pracma::toc()
  }
  
  #numer <- crossprod(collapse::fsum(XinvXXr * Q, clustid$clustid) + collapse::fsum(XinvXXr * (P %*% matrix(beta0, 1, length(beta0))), clustid$clustid), 
  #                   v)
  numer_a <- collapse::fsum(XinvXXrQ, clustid$clustid)
  numer_b <- collapse::fsum(as.vector(XinvXXr) * P, clustid$clustid)
  A <- crossprod(numer_a, v)
  B <- crossprod(numer_b, v)
  
  JJ <- list()
  
  p_val_null2 <- function(beta0, A, B, C, D, boot_iter){
    
    # if(is.null(W)){
    #   for(x in names(clustid)){
    #     NULL
    #   } 
    # } else if(!is.null(W)){
    for(x in names(clustid)){
      numer <- A + B * beta0
      JJ[[x]] <- colSums(small_sample_correction[x] * (CC[[x]] + 2* CD[[x]]*beta0+ DD[[x]]* beta0^2))
    }
    #}
    JJ <- Reduce("+", JJ)
    
    denom <- suppressWarnings(sqrt(JJ))
    #numer <- SXinvXXRu %*% v 
    
    t <- abs(numer) / denom
    delete_invalid_t_total <- sum(is.na(t))
    t <- t[!is.na(t)]
    
    t_boot <- t[2:(boot_iter + 1 - delete_invalid_t_total)]
    p_val <- mean(abs(t[1] - beta0) < (t_boot))
    p_val
  }
  
  p_val_null2_x <- function(beta0){
    p_val_null2(beta0, A = A, B= B, C=C, D=D, boot_iter) - alpha
  }  
  
  
  # p-value must cross alpha
  check <- FALSE
  inflate_se <- c(2, 3, 5, 10, 15, 25, 50, 100)
  len_inflate <- length(inflate_se)
  j <- 1
  
  while(check == FALSE){
    #print("check")
    if(j > len_inflate){
      break("Boottest confidence set calculation fails because no p-value < alpha could succesfully
            be guessed.")
    }
    # start guesses by taking sandwich cluster confidence intervals + inflation factor
    starting_vals <- as.numeric(point_estimate + c(-inflate_se[j], inflate_se[j]) * se_guess)
    #print(starting_vals)
    # take 25 starting values in between the guesses
    p_start <- rep(NaN, length(starting_vals))
    
    # progress_bar <- function(i, n = length(starting_vals) * width){
    #   width <- options()$width
    #   cat(paste0(rep('.', i / n), collapse = ''))
    #   #Sys.sleep(.05)
    #   if (i == n) cat('.')
    #   else cat('\014')
    # }
    
    for(i in 1:length(starting_vals)){
      p_start[i] <- p_val_null2_x(starting_vals[i]) 
    }
    p_start <- p_start + alpha 
    
    if(sum(p_start < alpha) == 2){
      check <- TRUE
    }
    j <- j + 1
    #print(p_start)
  }
  
  test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
  
  # later: don't have to evaluate all guesses at all points - extreme points suffice - if < alpha at both extreme points
  # then evaluate all 26 points
  p <- rep(NaN, length(test_vals))
  
  
  pb = txtProgressBar(min = 0, max = (length(test_vals) - 2), initial = 0, style = 3, char = "-") 
  
  for(i in 2:(length(test_vals) - 1)){
    p[i] <- p_val_null2_x(test_vals[i]) 
    setTxtProgressBar(pb,i)
  }
  close(pb)
  # substract alpha in function so that I will not need to 
  # do it in root finding algorithm, but then I will need to add 
  # alpha here
  p <- p + alpha 
  
  p[1] <- p_start[1]
  p[26] <- p_start[2]
  #if(sum(p < alpha) < 1){warning("Need to djust starting values: they are not p < alpha. Therefore, choose more
  #                              extreme starting values.")}
  
  crossings <-  (p < alpha) - (p > alpha)
  
  x_crossings <- rep(NA, length(test_vals))
  for(i in 1:26){
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }
  
  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]
  
  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]
  
  if(length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0){
    stop("test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property |p(x1) < 0.05| has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
         will not work.")
  }  
  
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
    tmp <- suppressWarnings(stats::uniroot(f = p_val_null2_x, lower = min(x), upper = max(x), tol = 1e-6, maxiter = 10))
    
    tmp$root
  })
  
  conf_int <- unlist(res)
  
  res_all <- list(conf_int = conf_int, 
                  p_test_vals = p, 
                  test_vals = test_vals)
  
  res_all
  
}

# ----------------------------------------------------------------------------------------------- #
# invert p val 2
# ----------------------------------------------------------------------------------------------- #

invert_p_val2.algo_oneclust <- function(object, B, point_estimate, se_guess, clustid, alpha){
  
  #' Inverts the bootstrap p-value and calculates confidence sets
  #'@param object A regression object of class lm, feols or felm
  
  check_arg(point_estimate, "numeric scalar")
  check_arg(se_guess, "numeric scalar")
  check_arg(clustid, "data.frame")
  
  # names(clustid) <- "clustid"
  
  if(alpha > 1 | alpha < 0){stop("Significance level needs to be between 0 and 1.")}
  boot_iter <- B
  rm(B)
  
  G <- sapply(clustid, function(x) length(unique(x)))
  small_sample_correction <- G / (G - 1)
  #small_sample_correction <- small_sample_correction * c(rep(1, length(clustid) - 1), - 1)
  
  ABCD <- object$ABCD
  A <- ABCD$A
  B <- ABCD$B
  CC <- ABCD$CC
  CD <- ABCD$CD
  DD <- ABCD$DD
  
  #p_val_null2 <- function(beta0, A, B, CC, CD, DD, clustid, boot_iter, small_sample_correction){
  
  p_val_null2_x <- function(beta0, alpha){
    p_val_null2(beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction = small_sample_correction)$p_val - alpha
  }
  
  # p-value must cross alpha
  check <- FALSE
  inflate_se <- c(2, 3, 5, 10, 15, 25, 50, 100)
  len_inflate <- length(inflate_se)
  j <- 1
  
  while(check == FALSE){
    #print("check")
    if(j > len_inflate){
      break("Boottest confidence set calculation fails because no p-value < alpha could succesfully
            be guessed.")
    }
    # start guesses by taking sandwich cluster confidence intervals + inflation factor
    starting_vals <- as.numeric(point_estimate + c(-inflate_se[j], inflate_se[j]) * se_guess)
    #print(starting_vals)
    # take 25 starting values in between the guesses
    p_start <- rep(NaN, length(starting_vals))
    
    # progress_bar <- function(i, n = length(starting_vals) * width){
    #   width <- options()$width
    #   cat(paste0(rep('.', i / n), collapse = ''))
    #   #Sys.sleep(.05)
    #   if (i == n) cat('.')
    #   else cat('\014')
    # }
    
    for(i in 1:length(starting_vals)){
      p_start[i] <- p_val_null2_x(starting_vals[i], alpha = alpha) 
    }
    p_start <- p_start + alpha 
    
    if(sum(p_start < alpha) == 2){
      check <- TRUE
    }
    j <- j + 1
    #print(p_start)
  }
  
  test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
  
  # later: don't have to evaluate all guesses at all points - extreme points suffice - if < alpha at both extreme points
  # then evaluate all 26 points
  p <- rep(NaN, length(test_vals))
  
  
  pb = txtProgressBar(min = 0, max = (length(test_vals) - 2), initial = 0, style = 3, char = "-") 
  
  for(i in 2:(length(test_vals) - 1)){
    p[i] <- p_val_null2_x(test_vals[i], alpha) 
    setTxtProgressBar(pb,i)
  }
  close(pb)
  # substract alpha in function so that I will not need to 
  # do it in root finding algorithm, but then I will need to add 
  # alpha here
  p <- p + alpha 
  
  p[1] <- p_start[1]
  p[26] <- p_start[2]
  #if(sum(p < alpha) < 1){warning("Need to djust starting values: they are not p < alpha. Therefore, choose more
  #                              extreme starting values.")}
  
  crossings <-  (p < alpha) - (p > alpha)
  
  x_crossings <- rep(NA, length(test_vals))
  for(i in 1:26){
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }
  
  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]
  
  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]
  
  if(length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0){
    stop("test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property |p(x1) < 0.05| has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
         will not work.")
  }  
  
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
    p_val_null2_x_alpha <- function(x){
      p_val_null2_x(x, alpha = alpha)
    }
    tmp <- suppressWarnings(stats::uniroot(f = p_val_null2_x_alpha, lower = min(x), upper = max(x), tol = 1e-6, maxiter = 10))
    
    tmp$root
  })
  
  conf_int <- unlist(res)
  
  res_all <- list(conf_int = conf_int, 
                  p_test_vals = p, 
                  test_vals = test_vals)
  
  res_all
  
  
}


invert_p_val2.algo_multclust <- function(object, B, point_estimate, se_guess, clustid, alpha){
  
  #' Inverts the bootstrap p-value and calculates confidence sets
  #'@param object A regression object of class lm, feols or felm
  
  check_arg(point_estimate, "numeric scalar")
  check_arg(se_guess, "numeric scalar")
  check_arg(clustid, "data.frame")
  
  if(alpha > 1 | alpha < 0){stop("Significance level needs to be between 0 and 1.")}
  boot_iter <- B
  rm(B)
  
  G <- sapply(clustid, function(x) length(unique(x)))
  small_sample_correction <- G / (G - 1)
  small_sample_correction <- small_sample_correction * c(rep(1, length(clustid) - 1), - 1)
  
  ABCD <- object$ABCD
  A <- ABCD$A
  B <- ABCD$B
  CC <- ABCD$CC
  CD <- ABCD$CD
  DD <- ABCD$DD
  
  #p_val_null2 <- function(beta0, A, B, CC, CD, DD, clustid, boot_iter, small_sample_correction){
  
  p_val_null2_x <- function(beta0, alpha){
    p_val_null2(beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction = small_sample_correction)$p_val - alpha
  }
  
  p_val_null2_x_cmp <- compiler::cmpfun(p_val_null2_x)
  
  # p-value must cross alpha
  check <- FALSE
  inflate_se <- c(2, 3, 5, 10, 15, 25, 50, 100)
  len_inflate <- length(inflate_se)
  j <- 1
  
  while(check == FALSE){
    #print("check")
    if(j > len_inflate){
      break("Boottest confidence set calculation fails because no p-value < alpha could succesfully
            be guessed.")
    }
    # start guesses by taking sandwich cluster confidence intervals + inflation factor
    starting_vals <- as.numeric(point_estimate + c(-inflate_se[j], inflate_se[j]) * se_guess)
    #print(starting_vals)
    # take 25 starting values in between the guesses
    p_start <- rep(NaN, length(starting_vals))
    
    # progress_bar <- function(i, n = length(starting_vals) * width){
    #   width <- options()$width
    #   cat(paste0(rep('.', i / n), collapse = ''))
    #   #Sys.sleep(.05)
    #   if (i == n) cat('.')
    #   else cat('\014')
    # }
    
    for(i in 1:length(starting_vals)){
      p_start[i] <- p_val_null2_x_cmp(starting_vals[i], alpha = alpha) 
    }
    p_start <- p_start + alpha 
    
    if(sum(p_start < alpha) == 2){
      check <- TRUE
    }
    j <- j + 1
    #print(p_start)
  }
  
  test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
  
  # later: don't have to evaluate all guesses at all points - extreme points suffice - if < alpha at both extreme points
  # then evaluate all 26 points
  p <- rep(NaN, length(test_vals))
  
  
  pb = txtProgressBar(min = 0, max = (length(test_vals) - 2), initial = 0, style = 3, char = "-") 
  
  for(i in 2:(length(test_vals) - 1)){
    p[i] <- p_val_null2_x_cmp(test_vals[i], alpha) 
    setTxtProgressBar(pb,i)
  }
  close(pb)
  # substract alpha in function so that I will not need to 
  # do it in root finding algorithm, but then I will need to add 
  # alpha here
  p <- p + alpha 
  
  p[1] <- p_start[1]
  p[26] <- p_start[2]
  #if(sum(p < alpha) < 1){warning("Need to djust starting values: they are not p < alpha. Therefore, choose more
  #                              extreme starting values.")}
  
  crossings <-  (p < alpha) - (p > alpha)
  
  x_crossings <- rep(NA, length(test_vals))
  for(i in 1:26){
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }
  
  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]
  
  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]
  
  if(length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0){
    stop("test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property |p(x1) < 0.05| has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
         will not work.")
  }  
  
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
    p_val_null2_x_alpha <- function(x){
      p_val_null2_x_cmp(x, alpha = alpha)
    }
    tmp <- suppressWarnings(stats::uniroot(f = p_val_null2_x_alpha, lower = min(x), upper = max(x), tol = 1e-6, maxiter = 10))
    
    tmp$root
  })
  
  conf_int <- unlist(res)
  
  res_all <- list(conf_int = conf_int, 
                  p_test_vals = p, 
                  test_vals = test_vals)
  
  res_all
  
  
}
