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
  model_frame_fe <- preprocessed_object$model_frame_fe
  beta0 <- preprocessed_object$beta0
  N_G <- preprocessed_object$N_G
  alpha <- preprocessed_object$alpha
  param <- preprocessed_object$param
  W <- preprocessed_object$W
  n_fe <- preprocessed_object$n_fe
  
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  #Xr1 <- X
  #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
  
  
  
  #clustid <- as.vector(clustid)
  #clustid <- rep(1:20, 100)
  #clustid <- clustid$clustid
  
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
    
    tmp1 <- data.frame(names = paste0(1:80, ".", levels(model_frame_fe[, 1])))
    tmp2 <- (collapse::fsum(x = XinvXXr, g = cbind(clustid, model_frame_fe)))
    tmp2 <- data.frame(names = rownames(tmp2), vals = as.vector(tmp2))
    tmp <- merge(tmp1, tmp2, by = "names", all.x = TRUE)
    tmp$vals[is.na(tmp$vals)] <- 0
    vals <- tmp$vals
    
    S_XinvXXR_F <- matrix(vals, n_fe, N_G)
    
    #S_XinvXXR_F <- matrix(collapse::fsum(x = XinvXXr, g = cbind(clustid, model_frame_fe)), n_fe, N_G)
    tmp2 <- collapse::fsum(x = XinvXXr, g = cbind(clustid, model_frame_fe))
    tmp2 <- data.frame(names = rownames(tmp2), vals = as.vector(tmp2))
    tmp <- merge(tmp1, tmp2, by = "names", all.x = TRUE)
    tmp$vals[is.na(tmp$vals)] <- 0
    vals <- tmp$vals
    
    S_Wu_F <- matrix(vals, n_fe, N_G)
    #S_XinvXXR_F <- matrix(collapse::fsum(x = XinvXXr, g = cbind(clustid, model_frame_fe)), n_fe, N_G)
    #S_Wu_F <- matrix(collapse::fsum(as.vector(W %*% u_hat), g = cbind(clustid, model_frame_fe)), n_fe, N_G)
    prod <- t(S_XinvXXR_F) %*% S_Wu_F
    diag_SXinvXXRu <- diag_SXinvXXRu - prod
  }
  #SXinvXXRX_prep <- data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)
  #SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  
  #SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
  #SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  
  SXu <- collapse::fsum(X * matrix(rep(u_hat, k), N, k), clustid)
  
  J <- (diag_SXinvXXRu - SXinvXXRX  %*% invXX %*% t(SXu)) %*% v  
  
  # } else if(use_fixef == TRUE){
  #   
  #   crosstab_XinvXXRu_prep <- data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid) 
  #   crosstab_XinvXXRu <- crosstab(crosstab_XinvXXRu_prep, groups = c("clustid.clustid", "clustid.clustid"), rename_var = "prod.V1")
  #   
  #   if(matrixcalc::is.diagonal.matrix(crosstab_XinvXXRu) == FALSE){break("Matrix is not diagonal.")}
  #   
  #   crosstab_XinvXXR_prep <- data.table::data.table(prod = matrix(XinvXXr, N, 1), clustid = clustid, fe = fixed_effects)
  #   crosstab_XinvXXR <- crosstab(data = crosstab_XinvXXR_prep, groups = c("clustid.clustid", "fe.fixed_effect_1"), rename_var = "prod.V1")
  #   
  #   S_XinvXXRX_prep <- data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)
  #   S_Xu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
  #   
  #   S_XinvXXRX <- S_XinvXXRX_prep[, lapply(.SD, sum), by = c("clustid.clustid")][, clustid.clustid := NULL]
  #   S_Xu <- S_Xu_prep[, lapply(.SD, sum), by = c("clustid.clustid")][, clustid.clustid := NULL]
  #   
  #   setDT(fixed_effects)
  #   fixed_effects[, tab := .N, by = "fixed_effect_1"]
  #   fixed_effects[, freq := tab / nrow(fixed_effects)][, tab:=NULL]
  #   W <- Diagonal(n = N) * fixed_effects$freq
  #   crosstab_Wu_prep <- data.table(prod = as.vector(W %*% u_hat), clustid = clustid, fe = fixed_effects$fixed_effect_1)
  #   crosstab_Wu <- crosstab(data = crosstab_Wu_prep, groups = c("clustid.clustid", "fe"), rename_var = "prod")
  #   
  #   S_XinvXXRX <- as.matrix(S_XinvXXRX)
  #   S_Xu <- as.matrix(S_Xu)
  #   
  #   # now combine all
  #   K <- crosstab_XinvXXRu - crosstab_XinvXXR %*% t(crosstab_Wu) - S_XinvXXRX %*% invXX %*% t(S_Xu)
  #   J <- K %*% v
  #   
  # }
  
  
  
  #hypothesis <- c(0, rep(beta0, B))
  #hypothesis <- c(beta0, rep(0, B))
  t <- abs(numer)  / sqrt(colSums(J * J))    # note: absolute value is taken here - no negative t-stats
  
  
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
  
  X <- preprocessed_object$X
  Y <- preprocessed_object$Y
  R0 <- preprocessed_object$R0
  data <- preprocessed_object$data
  N <- preprocessed_object$N
  k <- preprocessed_object$k
  clustid <- preprocessed_object$clustid
  fixed_effects <- preprocessed_object$fixed_effects
  beta0 <- preprocessed_object$beta0
  N_G <- preprocessed_object$N_G
  alpha <- preprocessed_object$alpha
  param <- preprocessed_object$param
  
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
  
  v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  v[,1] <- 1
  invXX <- solve(t(X) %*% X) # k x k matrix
  XinvXXr <- X %*% (invXX %*% R0) # N x 1
  
  p_val_null <- function(beta0, Q, P, R0, X, XinvXXr, clustid, 
                         SXinvXXRu_prep, v, B, small_sample_correction){
    # error under the null hypothesis
    #u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
    u_hat <- Q + P %*% matrix(beta0, 1, length(beta0))
    uX <- matrix(rep(u_hat, 1), N, k) * X
    SuX <- collapse::fsum(uX, clustid$clustid)
    tSuX <- t(SuX)
    XinvXXRu <- as.vector(XinvXXr * matrix(rep(u_hat, 1), N, 1))
    SXinvXXRu <-collapse::fsum(XinvXXr * matrix(rep(u_hat, 1), N, 1), clustid)
    #SXinvXXRu <- collapse::fsum(XinvXXRu, clustid$clustid)
    XinvXXRuS <- t(collapse::fsum(XinvXXRu, clustid$clustid))
    #diag_XinvXXRuS <- t(collapse::fsum(diag(as.vector(XinvXXRu)), clustid$clustid))
    diag_XinvXXRuS <- Matrix::t(Matrix.utils::aggregate.Matrix(Matrix::Diagonal(N, as.vector(XinvXXRu)), clustid$clustid))
    
    #tKK <- list()
    #JJ <- list()
    #n_clustid <- length(unique(clustid$clustid))
    #tKK <- array(NA, dim = c(n_clustid, n_clustid, ncol(clustid)))
    #i <- 1
    
    tKK <- list()
    
    XinvXXrX <- matrix(rep(XinvXXr, k), N, k) * X
    
    for(x in names(clustid)){
      #S_diag_XinvXXRu_S <- collapse::fsum(diag_XinvXXRuS, clustid[x])
      S_diag_XinvXXRu_S <- aggregate.Matrix(diag_XinvXXRuS, clustid[x])
      SXinvXXrX <-  collapse::fsum(XinvXXrX, clustid[x])
      K <- S_diag_XinvXXRu_S - SXinvXXrX %*% invXX %*% tSuX
      tKK[[x]] <- small_sample_correction[x] * Matrix::t(K) %*% K # here: add small sample df correction
      #i <- i + 1
      #tKK[[x]] <-  t(K) %*% K # here: add small sample df correction
      #J <- K %*% v
      #JJ[[x]] <- small_sample_correction[x] * J * J
    }
  
    #JJ_sum <- Reduce("+", JJ)
    tKK_sum <- Reduce("+", tKK)
    #tKK_sum <- rowSums(tKK, dims = 2)
    denom <- Matrix::colSums(v * tKK_sum %*% v)
    numer <- t(SXinvXXRu) %*% v 
    
    t <- abs(numer) / denom
    
    t_boot <- t[2:(B + 1)]
    p_val <- mean(abs(t[1] - beta0) < (t_boot))
    
    res <- list(p_val = p_val, 
                t = t, 
                t_boot = t_boot)
  }
  
  # error under the null hypothesis
  #u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
  # u_hat <- Q + P %*% matrix(beta0, 1, length(beta0))
  # 
  # invXX <- solve(t(X) %*% X) # k x k matrix
  # 
  # v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  # v[,1] <- 1
  
  #XinvXXrX <- XinvXXr * X
  
  

  # start collapsing for clustid and individual clusters 
  # clustid: boot_cluster
  
  # XinvXXRu <- as.vector(XinvXXr * matrix(rep(u_hat, 1), N, 1))
  # SXinvXXRu <-collapse::fsum(XinvXXr * matrix(rep(u_hat, 1), N, 1), clustid)
  # 
  # #SXinvXXRu <- collapse::fsum(XinvXXRu, clustid$clustid)
  # XinvXXRuS <- t(collapse::fsum(XinvXXRu, clustid$clustid))
  

  
  #N_G3 <- length(unique(clustid$clustid))
  # things in for loop
  #tKK <- array(NA, dim = c(N_G3, N_G3, 2))
  # diag_XinvXXRuS <- t(collapse::fsum(diag(as.vector(XinvXXRu)), clustid$clustid))
  # tKK <- list()
  # for(x in names(clustid)){
  #   S_diag_XinvXXRu_S <- collapse::fsum(diag_XinvXXRuS, clustid[x])
  #   SXinvXXrX <-  collapse::fsum(matrix(rep(XinvXXr, k), N, k) * X, clustid[x])
  #   K <- S_diag_XinvXXRu_S - SXinvXXrX %*% invXX %*% tSuX
  #   tKK[[x]] <- small_sample_correction[x] * t(K) %*% K # here: add small sample df correction
  #   #tKK[[x]] <-  t(K) %*% K # here: add small sample df correction
  #   #J <- K %*% v
  # }
  # 
  # tKK_sum <- Reduce("+", tKK)
  # denom <- colMeans(v * tKK_sum %*% v)
  # numer <- t(SXinvXXRu) %*% v 
  # 
  # t <- abs(numer) / denom
  # 
  # t_boot <- t[2:(B + 1)]
  # p_val <- mean(abs(t[1] - beta0) < (t_boot))
  
  p_val_res <- p_val_null(beta0 = beta0, Q =Q, P = P, R0 = R0, X = X, XinvXXr = XinvXXr, clustid = clustid, 
                SXinvXXRu_prep = SXinvXXRu_prep, v = v, B = B, small_sample_correction = small_sample_correction)
  p_val <- p_val_res$p_val
  t <- p_val_res$t
  t_boot <- p_val_res$t_boot
    
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
               XinvXXr = XinvXXr)
  class(res) <- "algo_multclust"
  
  res
}

