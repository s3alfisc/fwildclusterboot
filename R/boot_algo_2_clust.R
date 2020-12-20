#' boot_algo.2_cluster <- function(preprocessed_object){
#'   
#'   #' function that implements the fast bootstrap algorithm as described in Roodman et al (2009)
#'   #' @param preprocessed_object A preprocessed object of time preprocessed_boottest
#'   #' @return A list of ... 
#'   #' 
#'   
#'   # if(!inherits(preprocessed_object, "boottest_preprocessed")){
#'   #   stop("Estimation only works for inputs of class boottest_preprocessed.")
#'   # }
#'   
#'   #preprocess <- preprocess.fixest(object = preprocessed_object, param = param, clustid = clustid, beta0 = beta0, alpha = alpha, demean = demean)
#'   
#'   #preprocessed_object <- res_preprocess
#'   #preprocessed_object <- preprocess
#'   
#'   X <- preprocessed_object$X
#'   Y <- preprocessed_object$Y
#'   R0 <- preprocessed_object$R0
#'   data <- preprocessed_object$data
#'   N <- preprocessed_object$N
#'   k <- preprocessed_object$k
#'   clustid <- preprocessed_object$clustid
#'   fixed_effects <- preprocessed_object$fixed_effects
#'   beta0 <- preprocessed_object$beta0
#'   N_G <- preprocessed_object$N_G
#'   alpha <- preprocessed_object$alpha
#'   param <- preprocessed_object$param
#'   
#'   Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
#'   
#'   # Yr for constraint leas squares with beta0 = c
#'   Yr <- Y - X[, which(R0 == 1)] * beta0
#'   
#'   #Xr1 <- X
#'   #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
#'   
#'   
#'   
#'   # error under the null hypothesis
#'   u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
#'   invXX <- solve(t(X) %*% X) # k x k matrix
#'   
#'   v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
#'   v[,1] <- 1
#'   
#'   XinvXXr <- X %*% (invXX %*% R0) # N x 1
#'   #XinvXXrX <- XinvXXr * X
#'   
#'   uX <- matrix(rep(u_hat, 1), N, k) * X
#'   SuX <- collapse::fsum(uX, clustid$clustid)
#'   tSuX <- t(SuX)
#'   # start collapsing for clustid and individual clusters 
#'   # clustid: boot_cluster
#'   
#'   XinvXXRu <- as.vector(XinvXXr * matrix(rep(u_hat, 1), N, 1))
#'   SXinvXXRu <-collapse::fsum(XinvXXr * matrix(rep(u_hat, 1), N, 1), clustid)
#'   
#'   #SXinvXXRu <- collapse::fsum(XinvXXRu, clustid$clustid)
#'   XinvXXRuS <- t(collapse::fsum(XinvXXRu, clustid$clustid))
#'   
#'   # small sample correction for clusters 
#'   G <- sapply(clustid, function(x) length(unique(x)))
#'   small_sample_correction <- G / (G - 1)
#'   
#'   #N_G3 <- length(unique(clustid$clustid))
#'   # things in for loop
#'   #tKK <- array(NA, dim = c(N_G3, N_G3, 2))
#'   diag_XinvXXRuS <- t(collapse::fsum(diag(as.vector(XinvXXRu)), clustid$clustid))
#'   tKK <- list()
#'   for(x in names(clustid)){
#'     S_diag_XinvXXRu_S <- collapse::fsum(diag_XinvXXRuS, clustid[x])
#'     SXinvXXrX <-  collapse::fsum(matrix(rep(XinvXXr, k), N, k) * X, clustid[x])
#'     K <- S_diag_XinvXXRu_S - SXinvXXrX %*% invXX %*% tSuX
#'     #K[K < 0] <- NA
#'     tKK[[x]] <- small_sample_correction[x] * t(K) %*% K # here: add small sample df correction
#'     #tKK[[x]] <-  t(K) %*% K # here: add small sample df correction
#'     #J <- K %*% v
#'   }
#'   
#'   tKK_sum <- Reduce("+", tKK)
#'   denom <- colMeans(v * tKK_sum %*% v)
#'   numer <- t(SXinvXXRu) %*% v 
#'   
#'   t <- abs(numer) / denom
#'   
#'   t_boot <- t[2:(B + 1)]
#'   p_val <- mean(abs(t[1] - beta0) < (t_boot))
#'   
#'   res  <- list(p_val = p_val,
#'                t_stat = t[1],
#'                t_boot = t_boot,
#'                X = X,
#'                Y = Y, 
#'                B = B, 
#'                R0 = R0, 
#'                param = param, 
#'                clustid = clustid,
#'                invXX = invXX,
#'                v = v,
#'                Xr = Xr,
#'                XinvXXr = XinvXXr)
#'   
#'   res
#' }
