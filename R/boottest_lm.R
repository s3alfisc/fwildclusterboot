preprocess_lm <- function(object, param, clustid, beta0, alpha){
  
  data <- get_model_frame(object)
  
  #formula <- object$call$fml
  weights <- object$call$weights
  
  if(!is.null(weights)){
    stop("Currently, boottest does not support weighted least squares. weights 
         must be NULL.")
  }
  
  if(!is.null(seed)){
    set.seed(seed)
  } else if(is.null(seed)){
    set.seed(2)
  }
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  
  if(!is.null(object$call$weights)){
    stop("Function currently does not allow weights.")
  }
  
  # retrieve clusters / multiple clusters
  if(inherits(clustid, "formula")) {
    clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
    clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
  } else {
    clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
  }
  
  if(!(param %in% c(names(object$coefficients)))){
    warning("Parameter to test not in model or all. Please specify appropriate parameters to test.")
  }
  
  # how many clustids? uniway/multiway?
  clustid_dims <- ncol(clustid)
  
  
  # Handle omitted or excluded observations
  if(!is.null(object$na.action)) {
    if(class(object$na.action) == "exclude") {
      clustid <- clustid[-object$na.action,]
    } else if(class(object$na.action) == "omit") {
      clustid <- clustid[-object$na.action,]
    }
    clustid <- as.data.frame(clustid)  # silly error somewhere
  }
  #if(debug) print(class(clustid))
  
  if(is.null(beta0)){
    beta0 <- 0
  }
  
  # Factors in our clustiding variables can potentially cause problems
  # Blunt fix is to force conversion to characters
  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)
  
  # Make all combinations of clustid dimensions
  # if(clustid_dims > 1) {
  #   for(i in acc) {
  #     clustid <- cbind(clustid, Reduce(paste0, clustid[,i]))
  #   }
  # }
  
  
  # start estimation here: 
  
  R0 <- as.numeric(param == names(object$coefficients))
  groupvars <- names(coef(object))
  
  # if(object_type == "felm"){
  #   
  #   depvar <- names(object$response)
  #   Y <- object$response
  #   X <- lfe:::model.matrix.felm(felm_fit) 
  # }
  
  
  depvar <- all.vars(as.formula(object$call))[1]
  #measurevar <- "y"
  #formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  
  X <- model.matrix(as.formula(object$call), object$model)
  Y <- as.matrix(model.frame(object)[, depvar])  
  
  #res <- boottest_fun(Y = Y, X = X, R0 = R0, clustid = clustid, B = B, param = param)
  
  N <- length(Y)
  k <- ncol(X)
  
  #Xr1 <- X
  #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
  
  
  
  #clustid <- as.vector(clustid)
  #clustid <- rep(1:20, 100)
  N_G <- nrow(unique(clustid)) #number of clusters
  if(N_G > 200){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  }
  
  res_preprocess <- list(data = data, 
                         clustid = clustid, 
                         N = N, 
                         k = k, 
                         Y = Y, 
                         X = X, 
                         #depvar = depvar, 
                         #groupvars = groupvars, 
                         beta0 = beta0, 
                         clustid_dims, 
                         R0 = R0, 
                         N_G = N_G, 
                         alpha = alpha)
  
  res_preprocess
  
}


boottest.lm <- function(object, 
                        clustid, 
                        param, 
                        B,
                        weights = NULL, 
                        conf_int = NULL, 
                        debug = FALSE, 
                        seed = NULL, 
                        beta0 = NULL, 
                        alpha = NULL){
  
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'#'@method boottest lm
  #'@output An object of class boottest
  #'@export


  
  #boottest.lm(lm_fit, 1:2000, B = 1000, seed = 1, param = "x2", beta0 = NULL)
  # object <- lm_fit
  # clustid = voters$group_id
  # #B <- 10000
  # seed <- 1
  # param <- "treatment"
  # beta0 <- 0
  
  preprocess <- preprocess_lm(object, param, clustid, beta0, alpha)

  X <- preprocess$X
  Y <- preprocess$Y
  R0 <- preprocess$R0
  data <- preprocess$data
  N <- preprocess$N
  k <- preprocess$k
  clustid <- preprocess$clustid
  beta0 <- preprocess$beta0
  N_G <- preprocess$N_G
  alpha <- preprocess$alpha
  
  #clustid <- clustid$clustid
  
  # error under the null hypothesis
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  Yr <- Y - X[, which(R0 == 1)] * beta0
  u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
  
  
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
  
  res  <- list(p_val = p_val, X = X, Y = Y, B = B, R0 = R0, param = param, clustid = clustid)
  # Invert p-value

  
  if(is.null(conf_int) || conf_int == TRUE){
    res_p_val <- invert_p_val_fwc(object, data, clustid, X, Y, param, R0, B, N, k, seed, N_g, invXX, v, Xr, XinvXXr, SXinvXXRX, alpha)
    res_final <- list(p_val = res[["p_val"]], 
                      conf_int = res_p_val$conf_int, 
                      p_test_vals = res_p_val$p_test_vals, 
                      test_vals = res_p_val$test_vals,
                      t_stat = t[1], 
                      regression = object, 
                      param = param, 
                      N = N, 
                      B = B, 
                      clustid = clustid, 
                      #depvar = depvar, 
                      N_G = N_G)
    
  } else{
    res_final <- list(p_val = res[["p_val"]], 
                      t_stat = t[1], 
                      conf_int = conf_int, 
                      regression = object, 
                      param = param, 
                      N = N, 
                      B = B, 
                      clustid = clustid, 
                      #depvar = depvar, 
                      N_G = N_G)    
  }
  

 
  class(res_final) <- "boottest"
  res_final
  
  
  
  
} 

