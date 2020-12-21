boot_algo.oneclust <- function(preprocessed_object){
  
  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2009)
  #' @param preprocessed_object A preprocessed object of time preprocessed_boottest
  #' @return A list of ... 
  #' 
  
  # if(!inherits(preprocessed_object, "boottest_preprocessed")){
  #   stop("Estimation only works for inputs of class boottest_preprocessed.")
  # }
  
  #preprocess <- preprocess.fixest(object = preprocessed_object, param = param, clustid = clustid, beta0 = beta0, alpha = alpha, demean = demean)
  # preprocessed_object <- preprocess
  # preprocessed_object <- res_preprocess
  
  X <- preprocessed_object$X
  Y <- preprocessed_object$Y
  R0 <- preprocessed_object$R0
  data <- preprocessed_object$data
  N <- preprocessed_object$N
  k <- preprocessed_object$k
  clustid <- preprocessed_object$clustid
  fixed_effect <- preprocessed_object$fixed_effect
  beta0 <- preprocessed_object$beta0
  N_G <- preprocessed_object$N_G
  alpha <- preprocessed_object$alpha
  param <- preprocessed_object$param
  W <- preprocessed_object$W
  n_fe <- preprocessed_object$n_fe
  
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  # error under the null hypothesis
  u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
  invXX <- solve(t(X) %*% X) # k x k matrix
  
  v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  v[,1] <- 1
  
  XinvXXr <- X %*% (invXX %*% R0) # N x 1
  
  # benchmark collapse::fsum with data table
  # microbenchmark(collapse = collapse::fsum(XinvXXr * matrix(rep(u_hat, 1), N, 1), clustid), 
  #                data.table = as.matrix(data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid)[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL]), 
  #                times = 100)
  
  SXinvXXRu <-collapse::fsum(XinvXXr * matrix(rep(u_hat, 1), N, 1), clustid)
  
  # SXinvXXRu_prep <- data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid) 
  # SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  if(ncol(SXinvXXRu) == 1){
    SXinvXXRu <- as.vector(SXinvXXRu)
  }
  diag_SXinvXXRu <- Matrix::Diagonal(N_G, SXinvXXRu)
  
  numer <- SXinvXXRu %*% v 

  # if(use_fixef == FALSE){
  
  # "old" code - if no fixed effects used in calculation
  # test if identical 
  #SXinvXXRX1 <- as.matrix(data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  # SXinvXXRX <- collapse::fsum(matrix(rep(XinvXXr, k), N, k) * X, clustid)  
  # mean(colMeans(abs(SXinvXXRX1 - SXinvXXRX)))
  # microbenchmark(collapse = collapse::fsum(matrix(rep(XinvXXr, k), N, k) * X, clustid), 
  #                 data.table = as.matrix(data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL]), 
  #                 times = 10)
  SXinvXXRX <- collapse::fsum(matrix(rep(XinvXXr, k), N, k) * X, clustid)  
  
  # if model with fixed effect
  if(!is.null(W)){
    S_XinvXXR_F <- crosstab2(XinvXXr, var1 = clustid, var2 = fixed_effect)
    S_Wu_F <- crosstab2(as.matrix(W %*% u_hat), var1 = clustid, var2 = fixed_effect)
    prod <- S_XinvXXR_F %*% t(S_Wu_F)
    diag_SXinvXXRu <- diag_SXinvXXRu - prod
  }
  #SXinvXXRX_prep <- data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)
  #SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  
  #SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
  #SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  
  SXu <- collapse::fsum(X * matrix(rep(u_hat, k), N, k), clustid)
  
  J <- (diag_SXinvXXRu - SXinvXXRX  %*% invXX %*% t(SXu)) %*% v  
  
  t <- abs(numer)  / sqrt(Matrix::colSums(J * J))    # note: absolute value is taken here - no negative t-stats
  
  
  t_boot <- t[2:(B + 1)]
  
  p_val <- mean(abs(t[1] - beta0) < (t_boot))
  
  # res <- list(p_val = p_val#, 
  #             #conf_int = conf_int
  #             )
  
  #paste("The wild cluster bootstrap p-value for the parameter", param, "is", p_val, ",", "with B", B,  "bootstrap iterations.")
  
  res  <- list(p_val = p_val,
               t_stat = t[1],
               t_boot = t_boot,
               X = X,
               Y = Y, 
               B = B, 
               R0 = R0, 
               param = param, 
               clustid = clustid,
               invXX = invXX,
               v = v,
               Xr = Xr,
               XinvXXr = XinvXXr,
               SXinvXXRX = SXinvXXRX)
  
  class(res) <- "algo_oneclust"
  res
}


boot_algo.multclust <- function(preprocessed_object){
  
  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2009)
  #' @param preprocessed_object A preprocessed object of time preprocessed_boottest
  #' @return A list of ... 
  #' @import Matrix.utils
  
  # if(!inherits(preprocessed_object, "boottest_preprocessed")){
  #   stop("Estimation only works for inputs of class boottest_preprocessed.")
  # }
  
  #preprocess <- preprocess.fixest(object = preprocessed_object, param = param, clustid = clustid, beta0 = beta0, alpha = alpha, demean = demean)
  
  #preprocessed_object <- preprocess
  
  # 1) preprocess
  X <- preprocessed_object$X
  Y <- preprocessed_object$Y
  R0 <- preprocessed_object$R0
  data <- preprocessed_object$data
  N <- preprocessed_object$N
  k <- preprocessed_object$k
  clustid <- preprocessed_object$clustid
  fixed_effect <- preprocessed_object$fixed_effect
  beta0 <- preprocessed_object$beta0
  N_G <- preprocessed_object$N_G
  alpha <- preprocessed_object$alpha
  param <- preprocessed_object$param
  W <- preprocessed_object$W
  n_fe <- preprocessed_object$n_fe
  
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  v <- matrix(sample(c(1, -1), N_G["clustid"] * (B + 1), replace = TRUE), N_G["clustid"], B + 1) # rademacher weights for all replications
  v[,1] <- 1
  invXX <- solve(t(X) %*% X) # k x k matrix
  XinvXXr <- X %*% (invXX %*% R0) # N x 1

  # small sample correction for clusters 
  G <- sapply(clustid, function(x) length(unique(x)))
  small_sample_correction <- G / (G - 1)
  small_sample_correction <- small_sample_correction * c(rep(1, length(clustid) - 1), - 1)
  
  Q <- Y - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Y))
  P <- Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Xr0)) - Xr0
  
  
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
        
        S_diag_XinvXXRu_S <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS, clustid[x]) # c* x c
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
    denom_2 <- Matrix::colSums(v * tKK_sum %*% v)
    #denom_2 <- ifelse(denom_2 > 0, denom_2, NA)
    denom <- suppressWarnings(sqrt(denom_2))
    numer <- SXinvXXRu %*% v 
    
    t <- abs(numer) / denom
    delete_invalid_t_total <- sum(is.na(t))
    if(delete_invalid_t_total > 0){
      warning(paste0(delete_invalid_t_total, " replications returned an infeasible test statistic and were deleted from the bootstrap distribution."))
    }
    
    t <- t[!is.na(t)]
    
    t_boot <- t[2:(B + 1 - delete_invalid_t_total)]
    p_val <- mean(abs(t[1] - beta0) < (t_boot))
    
    res <- list(p_val = p_val, 
                t = t, 
                t_boot = t_boot, 
                delete_invalid_t_total = delete_invalid_t_total)
    res
  }
  

  
  p_val_res <- p_val_null(beta0 = beta0, Q =Q, P = P, R0 = R0, X = X, XinvXXr = XinvXXr, clustid = clustid, 
                v = v, B = B, small_sample_correction = small_sample_correction)
  
  p_val <- p_val_res$p_val
  t <- p_val_res$t
  t_boot <- p_val_res$t_boot
  invalid_t <- p_val_res$delete_invalid_t_total
    
  res  <- list(p_val = p_val,
               t_stat = t[1],
               t_boot = t_boot,
               X = X,
               Y = Y, 
               B = B, 
               R0 = R0, 
               param = param, 
               clustid = clustid,
               invXX = invXX,
               v = v,
               Xr = Xr,
               XinvXXr = XinvXXr, 
               invalid_t = invalid_t)
  class(res) <- "algo_multclust"
  
  res
}

