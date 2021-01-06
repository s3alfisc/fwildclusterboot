boot_algo.oneclust <- function(preprocessed_object, B, wild_draw_fun, ...){
  
  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2009)
  #' @param preprocessed_object A preprocessed object of time preprocessed_boottest
  #' @param B number of bootstrap iterations
  #'@param ... Further arguments passed to or from other methods.
  #' @param wild_draw_fun function. Specifies the type of bootstrap to use.
  #' @method boot_algo oneclust
  #' @export
  #' @return A list of ... 

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
  seed <- preprocessed_object$seed
  
  set.seed(seed)
  
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  # error under the null hypothesis
  u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
  invXX <- solve(t(X) %*% X) # k x k matrix
  
  # print(N_G["clustid"])
  # print(B)
  # print(class(B))
  
  v <- matrix(wild_draw_fun(n = N_G["clustid"] * (B + 1)), N_G["clustid"], B + 1)
  #v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  v[,1] <- 1
  
  XinvXXr <- X %*% (invXX %*% R0) # N x 1
  
  # benchmark collapse::fsum with data table
  # microbenchmark(collapse = collapse::fsum(XinvXXr * matrix(rep(u_hat, 1), N, 1), clustid), 
  #                data.table = as.matrix(data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid)[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL]), 
  #                times = 100)
  
  SXinvXXRu <-collapse::fsum(XinvXXr * u_hat, clustid)
  
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
  #SXinvXXRX <- collapse::fsum(XinvXXr * X, clustid)  
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
  
  #SXu <- collapse::fsum(X * as.vector(u_hat), clustid)
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


boot_algo.multclust <- function(preprocessed_object, B, wild_draw_fun, ...){
  
  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2009)
  #' @param preprocessed_object A preprocessed object of type preprocessed_boottest
  #' @param B number of bootstrap iterations
  #' @param wild_draw_fun function. Specifies the type of bootstrap to use.
  #'@param ... Further arguments passed to or from other methods.
  #' @import Matrix.utils
  #' @import Matrix
  #' @export
  #' @method boot_algo multclust
  #' @return A list of ... 


  
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
  seed <- preprocessed_object$seed
  
  set.seed(seed)
  
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  v <- matrix(wild_draw_fun(n = N_G["clustid"] * (B + 1)), N_G["clustid"], B + 1)
  #v <- matrix(sample(c(1, -1), N_G["clustid"] * (B + 1), replace = TRUE), N_G["clustid"], B + 1) # rademacher weights for all replications
  v[,1] <- 1
  invXX <- solve(t(X) %*% X) # k x k matrix
  #XinvXXr <- as.vector(X %*% (invXX %*% R0)) # N x 1
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
    #uX <- as.vector(u_hat) * X
    uX <- matrix(rep(u_hat, 1), N, k) * X 
    SuX <- collapse::fsum(uX, clustid$clustid)
    tSuX <- t(SuX)
    #XinvXXRu <-as.vector(XinvXXr * u_hat)
    XinvXXRu <- as.vector(XinvXXr * matrix(rep(u_hat, 1), N, 1)) 
    #diag_XinvXXRuS <- t(collapse::fsum(diag(as.vector(XinvXXRu)), clustid$clustid))

    
    tKK <- list()
    JJ <- list()
    
    #XinvXXrX <- XinvXXr * X
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
        tKK[[x]] <- small_sample_correction[x] * Matrix::t(K) %*% K # here: add small sa[xmple df correction
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
 
    # JJ_sum <- Reduce("+", JJ)
    # denom_2 <- Matrix::colSums(JJ_sum)
    
    tKK_sum <- Matrix::t(Reduce("+", tKK))
    
    if(nrow(tKK_sum) >= 40){
      #denom_2 <- colSums(v * Rfast::mat.mult(as.matrix(tKK_sum), v))
      denom_2 <- colSums(v * eigenMatMult(as.matrix(tKK_sum), as.matrix(v)))
    } else{
      denom_2 <- Matrix::colSums(v * tKK_sum %*% v)
    }
    
    denom <- suppressWarnings(sqrt(denom_2))
    numer <- SXinvXXRu %*% v 
    
    t <- abs(numer) / denom
    delete_invalid_t_total <- sum(is.na(t))
    if(delete_invalid_t_total > 0){
      warning(paste0(delete_invalid_t_total, " replications returned an infeasible test statistic and were deleted from the bootstrap distribution."), 
              .call = FALSE)
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
  
  invisible(res)
}



# --------------------------------------------------------------------------------- # 
# boot_algo2
# --------------------------------------------------------------------------------- #

boot_algo2.oneclust <- function(preprocessed_object, boot_iter, wild_draw_fun, ...){
  
  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2009)
  #' @param preprocessed_object A preprocessed object of time preprocessed_boottest
  #' @param boot_iter number of bootstrap iterations
  #'@param ... Further arguments passed to or from other methods.
  #' @param wild_draw_fun function. Specifies the type of bootstrap to use.
  #' @import Matrix.utils
  #' @import Matrix
  #' @export
  #' @method boot_algo2 oneclust
  #' @return A list of ... 
  
  dreamerr::check_arg(preprocessed_object, "class(oneclust)")
  
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
  seed <- preprocessed_object$seed
  
  # names(clustid) <- "clustid"
  # names(N_G) <- "clustid"
  
  set.seed(seed)
  
  # bootstrap error 
  v <- matrix(wild_draw_fun(n = N_G["clustid"] * (boot_iter + 1)), N_G["clustid"], boot_iter + 1)
  #v <- matrix(sample(c(1, -1), N_G["clustid"] * (boot_iter + 1), replace = TRUE), N_G["clustid"], boot_iter + 1) # rademacher weights for all replications
  v[,1] <- 1
  
  G <- sapply(clustid, function(x) length(unique(x)))
  small_sample_correction <- G / (G - 1)
  #small_sample_correction <- small_sample_correction * c(rep(1, length(clustid) - 1), - 1)
  
  # error under the null hypothesis
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  Q <- Y - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Y))
  P <- Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Xr0)) - Xr0
  
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
  }
  
  numer_a <- collapse::fsum(XinvXXrQ, clustid$clustid)
  numer_b <- collapse::fsum(as.vector(XinvXXr) * P, clustid$clustid)
  A <- crossprod(numer_a, v)
  B <- crossprod(numer_b, v)
  
  p_val_res <- p_val_null2(beta0 = beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction= small_sample_correction)
  
  p_val <- p_val_res$p_val
  t <- p_val_res$t
  t_boot <- p_val_res$t_boot
  invalid_t <- p_val_res$delete_invalid_t_total
  
  ABCD <- list(A = A, B = B, CC = CC, CD = CD, DD = DD)
  
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
               invalid_t = NULL, 
               ABCD = ABCD)
  class(res) <- "algo_oneclust"
  
  invisible(res)
  
}

boot_algo2.multclust <- function(preprocessed_object, boot_iter, wild_draw_fun, ...){
  
  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2009)
  #' @param preprocessed_object A preprocessed object of time preprocessed_boottest
  #' @param boot_iter number of bootstrap iterations
  #' @param ... Further arguments passed to or from other methods.
  #' @param wild_draw_fun function. Specifies the type of bootstrap to use.
  #' @import Matrix.utils
  #' @import Matrix
  #' @export
  #' @method boot_algo2 multclust
  #' @return A list of ... 
  
  
  dreamerr::check_arg(preprocessed_object, "class(multclust)")
  
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
  seed <- preprocessed_object$seed
  
  set.seed(seed)
  
  # bootstrap error 
  v <- matrix(wild_draw_fun(n = N_G["clustid"] * (boot_iter + 1)), N_G["clustid"], boot_iter + 1)
  #v <- matrix(sample(c(1, -1), N_G["clustid"] * (boot_iter + 1), replace = TRUE), N_G["clustid"], boot_iter + 1) # rademacher weights for all replications
  v[,1] <- 1
  # invXX <- solve(t(X) %*% X) # k x k matrix
  # XinvXXr <- as.vector(X %*% (invXX %*% R0)) # N x 1
  
  # start inversion 
  # boot_iter <- B
  # rm(B)
  # 
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
    
    #pracma::tic()
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
    #pracma::toc()
  }
  
  numer_a <- collapse::fsum(XinvXXrQ, clustid$clustid)
  numer_b <- collapse::fsum(as.vector(XinvXXr) * P, clustid$clustid)
  A <- crossprod(numer_a, v)
  B <- crossprod(numer_b, v)
  
  p_val_res <- p_val_null2(beta0 = beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction= small_sample_correction)
  
  p_val <- p_val_res$p_val
  t <- p_val_res$t
  t_boot <- p_val_res$t_boot
  invalid_t <- p_val_res$delete_invalid_t_total
  
  ABCD <- list(A = A, B = B, CC = CC, CD = CD, DD = DD)
  
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
               invalid_t = invalid_t, 
               ABCD = ABCD)
  class(res) <- "algo_multclust"
  
  invisible(res)
}








