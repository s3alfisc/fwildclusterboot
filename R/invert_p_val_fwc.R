invert_p_val_fwc <- function(object, data, clustid, X, Y, param, R0, B, N, k, seed, N_g, invXX, v, Xr, XinvXXr, SXinvXXRX){
  

  #object <- boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
  #if(class(object) != "fast_boot"){warning("Feed in object of types lm_robust and fast_boot.")}
  
  #tic()
  
  #rm(!c(lm_fit, lm_robust_fit, data))
  #object <- lm_fit
  #clustid = data$cluster
  #param = "x"
  

  # if(!is.null(seed)){
  #   set.seed(seed)
  # } else if(is.null(seed)){
  #   set.seed(2)
  # }
  
  # retrieve clusters / multiple clusters
  # if(inherits(clustid, "formula")) {
  #   clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
  #   clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
  # } else {
  #   clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
  # }
  # 
  # if(!(param %in% c(names(object$coefficients)))){
  #   warning("Parameter to test not in model or all. Please specify appropriate parameters to test.")
  # }
  # # how many clustids? uniway/multiway?
  # clustid_dims <- ncol(clustid)
  # 
  # 
  # # Handle omitted or excluded observations
  # if(!is.null(object$na.action)) {
  #   if(class(object$na.action) == "exclude") {
  #     clustid <- clustid[-object$na.action,]
  #   } else if(class(object$na.action) == "omit") {
  #     clustid <- clustid[-object$na.action,]
  #   }
  #   clustid <- as.data.frame(clustid)  # silly error somewhere
  # }
  #if(debug) print(class(clustid))
  
  #if(is.null(beta0)){
  #  beta0 <- 0
  #}
  
  # Factors in our clustiding variables can potentially cause problems
  # Blunt fix is to force conversion to characters
  #i <- !sapply(clustid, is.numeric)
  #clustid[i] <- lapply(clustid[i], as.character)
  
  # Make all combinations of clustid dimensions
  # if(clustid_dims > 1) {
  #   for(i in acc) {
  #     clustid <- cbind(clustid, Reduce(paste0, clustid[,i]))
  #   }
  # }
  
  
  # start estimation here: 
  
  #R0 <- as.numeric(param == names(object$coefficients))
  # R0 <- object$R0
  # groupvars <- names(coef(object))
  # 
  # # if(object_type == "felm"){
  # #   
  # #   depvar <- names(object$response)
  # #   Y <- object$response
  # #   X <- lfe:::model.matrix.felm(felm_fit) 
  # # }
  # 
  # 
  # depvar <- all.vars(as.formula(object$call))[1]
  #measurevar <- "y"
  #formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  
  #X <- model.matrix(as.formula(object$call), lm_fit$model)
  #Y <- as.matrix(model.frame(object)[, depvar])  
  # X <- object$X
  # Y <- object$Y
  # B <- object$B
  # param <- object$param
  # clustid <- object$clustid
  # 
  # N <- length(Y)
  # k <- ncol(X)
  # 
  # R0 <- matrix(R0, k, 1)
  
  # this needs to be rewritten so that correct fixed effects are used
  if(class(object) == "lm"){
    lm_robust_fit <- lm_robust(eval(object$call$formula), clusters = as.factor(clustid$clustid), data = data, se_type = "stata")
  } else if(class(object) == "lm_robust"){
    lm_robust_fit <- lm_robust(object$call$formula, 
                               clusters = eval(object$call$clusters), 
                               data = data, 
                               se_type = "stata")
  } else if(class(object) == "felm"){
    lm_robust_fit <- lm_robust(formula(Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment"))), lhs = 1, rhs = 1), 
                               clusters = clustid[, "clustid"], 
                               data = data, se_type = "stata")
  } else if(class(object) == "fixest"){
    lm_robust_fit <- lm_robust(object$call$fml, 
                               clusters =  clustid[, "clustid"], 
                               data = data, se_type = "stata")
  } else {
    stop("Function only designed for objects of type lm, lm_robust, felm and feols.")
  }

  
  # --------------------------------------------------------------------------------------------- #
  # start inversion 
  tidy_obj <- broom::tidy(lm_robust_fit)
  setDT(tidy_obj)
  estimate <- as.numeric(tidy_obj[term == param, "estimate"])
  st_error_guess <- as.numeric(tidy_obj[term == param, "std.error"])
  starting_vals <- as.numeric(estimate + c(-3,3) * st_error_guess)
  
  test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      

  #N_G <- nrow(unique(clustid)) #number of clusters
  
  #invXX <- solve(t(X) %*% X) # k x k matrix
  
  #v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  #v[,1] <- 1
  
  #R0 <- as.numeric(param == names(object$coefficients))
  #Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  #XinvXXr <- X %*% (invXX %*% R0) # N x 1
  
  #SXinvXXRX_prep <- data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)
  #SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
  
  
  XrinvXrXrtXr <- Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr)
  SXinvXXRX_invXX <- SXinvXXRX  %*% invXX
  Xr0 <- X[, which(R0 == 1)]
  
  p_val_null <- function(beta0, R0, Y, X, Xr,XinvXXr, clustid, 
                          SXinvXXRu_prep,  k,  N, v, B){
    
    Yr <- Y -Xr0 * beta0
    u_hat <- Yr - XrinvXrXrtXr %*% Yr # N x 1 matrix 
    #XrinvXrXrtXr <- Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr)
    
    SXinvXXRu_prep <- data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid) 
    SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    if(ncol(SXinvXXRu) == 1){
      SXinvXXRu <- as.vector(SXinvXXRu)
    }
    
    #a <- function(){
    SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
    SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL]) 
    #}
    #b <- function(){
    #Sxu1 <- as.matrix(aggregate(X * matrix(rep(u_hat, k), N, k), clustid, sum))
    #}
    #c <-function(){
    #  Sxu2 <- aggregate.Matrix(x = X * matrix(rep(u_hat, k), N, k), groupings = clustid, fun = "sum")
    #}
    #benchmark(a(), b(), c(), replications = 1000)
    
    #d <- function(){
    #  mat <- X * matrix(rep(u_hat, k), N, k)
    #  lapply(ncol(mat), function(i) lm(mat[,i] ~ 1 + factor(clustid$clustid)))
    #}
    
    #benchmark(a(), d(), replications = 100)
    
    
    numer <- SXinvXXRu %*% v 
    J <- (diag(SXinvXXRu) - SXinvXXRX_invXX %*% t(SXu)) %*% v  
    t <- abs(numer)  / sqrt(colSums(J * J))    # note: absolute value is taken here - no negative t-stats
    t_boot <- t[2:(B + 1)]
    mean(abs(t[1] - beta0) < (t_boot))
  }
  
  # can be smaller than zero bc of -0.5
  p_val_null_x <- function(beta0){
    p_val_null(beta0, R0 = R0, Y = Y, X = X, Xr = Xr, XinvXXr = XinvXXr, clustid = clustid, 
                SXinvXXRu_prep = SXinvXXRu_prep, k = k, N = N, v = v, B = B) - 0.05
  }
  #benchmark(p_val_null_x(test_vals[1]))
  # get test values
  p <- rep(NaN, length(test_vals))
  
  for(i in 1:length(test_vals)){
    p[i] <- p_val_null_x(test_vals[i]) 
  }

  p <- p + 0.05
  
  if(sum(p < 0.05) < 1){warning("Need to djust starting values: they are not p < 0.05. Therefore, choose more
                                extreme starting values.")}
  
  crossings <-  (p < 0.05) - (p > 0.05)
  x_crossings <- rep(NA, length(test_vals))
  for(i in 1:25){
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }
  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]

  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]

  
  
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    #tmp <- pracma::newtonRaphson(p_val_null_x, x0 =  x, dfun = NULL, maxiter = 25, tol = 1e-4)
    #tmp$root
    #tmp <- pracma::fzero(p_val_null_x , x = test_vals_higher, maxiter = 10, tol = 1e-12)
    #tmp$x
    tmp <- pracma::bisect(p_val_null_x , a = min(x), b = max(x), maxiter = 10)
    tmp$root
  })


  conf_int <- unlist(res)
  #p_val_null_x(conf_int[1])
  #p_val_null_x(conf_int[2] + 0.00001)
  
  
  return(conf_int)
  #conf_int
  #lm_robust_fit
  #summary(lm_fit)  
  
  #toc()
  
}
