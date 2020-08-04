#' boot_estimation <- function(X, Y, R0, clustid, B){
#'   
#'   #' Bootstrap estimation function. 
#'   #' Estimates bootstrap p-values for all regression objects. 
#'   #' @param X The matrix of covariates
#'   #' @param Y The vector of outcome variables
#'   #' @param R0 The vector of constraints
#'   #' @param B The number of bootstrap iterations
#'   
#'   N <- length(Y)
#'   k <- ncol(X)
#'   
#'   Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
#'   
#'   
#'   
#'   #clustid <- as.vector(clustid)
#'   #clustid <- rep(1:20, 100)
#'   N_G <- nrow(unique(clustid)) #number of clusters
#'   if(N_G > 2000){
#'     warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
#'   }
#'   
#'   # error under the null hypothesis
#'   u_hat <- Y - Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr) %*% Y # N x 1 matrix
#'   
#'   invXX <- solve(t(X) %*% X) # k x k matrix
#'   
#'   v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
#'   v[,1] <- 1
#'   
#'   XinvXXr <- X %*% (invXX %*% R0) # N x 1
#'   SXinvXXRu_prep <- data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid) 
#'   SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
#'   
#'   if(ncol(SXinvXXRu) == 1){
#'     SXinvXXRu <- as.vector(SXinvXXRu)
#'   }
#'   
#'   SXinvXXRX_prep <- data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)
#'   SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
#'   
#'   SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
#'   SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
#'   
#'   numer <- SXinvXXRu %*% v
#'   J <- (diag(SXinvXXRu) - SXinvXXRX  %*% invXX %*% t(SXu)) %*% v  
#'   
#'   t <- abs(numer)  / sqrt(colSums(J * J))       
#'   
#'   p_val <- mean(t[1] < t[2:(B + 1)])
#'   
#' }