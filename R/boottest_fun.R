boottest_fun <- function(Y, X, R0, clustid, B){
  
  N <- length(Y)
  k <- ncol(X)
  
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  #Xr1 <- X
  #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
  
  
  
  #clustid <- as.vector(clustid)
  #clustid <- rep(1:20, 100)
  N_G <- nrow(unique(clustid)) #number of clusters
  if(N_G > 2000){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  }
  #clustid <- clustid$clustid
  
  # error under the null hypothesis
  #u_hat <- Y - Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr) %*% Y # N x 1 matrix 
  u_hat <- Yr - Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr) %*% Yr # N x 1 matrix 
  
  
  #u_hat <- Y - R %*% (Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr) %*% Y) - beta0
  
  invXX <- solve(t(X) %*% X) # k x k matrix
  
  v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  v[,1] <- 1
  
  XinvXXr <- X %*% (invXX %*% R0) # N x 1
  SXinvXXRu_prep <- data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid) 
  SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  
  if(ncol(SXinvXXRu) == 1){
    SXinvXXRu <- as.vector(SXinvXXRu)
  }
  
  SXinvXXRX_prep <- data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)
  SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  
  SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
  SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  
  numer <- SXinvXXRu %*% v 
  J <- (diag(SXinvXXRu) - SXinvXXRX  %*% invXX %*% t(SXu)) %*% v  
  
  #hypothesis <- c(0, rep(beta0, B))
  #hypothesis <- c(beta0, rep(0, B))
  t <- abs(numer)  / sqrt(colSums(J * J))    # note: absolute value is taken here - no negative t-stats
  
  
  t_boot <- t[2:(B + 1)]
  #t_conf <- quantile(t_boot, c(0.025, 0.975))
  #conf_int <- 2*t[1] - t_conf
  #t_boot <- t_boot
  #p_val <- mean(abs(t[1]) < abs(t_boot - c(rep(beta0, B))))
  p_val <- mean(abs(t[1] - beta0) < (t_boot))
  
  # res <- list(p_val = p_val#, 
  #             #conf_int = conf_int
  #             )
  
  #paste("The wild cluster bootstrap p-value for the parameter", param, "is", p_val, ",", "with B", B,  "bootstrap iterations.")
  
  res  <- list(p_val = p_val, X = X, Y = Y, B = B, R0 = R0, param = param, clustid = clustid, regression = object)
  
}