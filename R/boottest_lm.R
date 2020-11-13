preprocess_lm <- function(object, param, clustid, beta0, alpha){
  
  #' function that pre-processes regression objects of type lm
  #'@param object An object of class lm
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param demean If TRUE, fixed effects are projected out prior to the bootstrap. FALSE by default
  #'@return preprocessed object of class boottest_preprocessed
  
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
  
  res_preprocess <- list(fixed_effects = NULL, 
                         data = data, 
                         param = param, 
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
  
  class(res_preprocess) <- "boottest_preprocessed"
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
  #'@method boottest lm
  #'@return An object of class boottest
  #'@export
  #'@import sandwich
  #'@import lmtest


  
  #boottest.lm(lm_fit, 1:2000, B = 1000, seed = 1, param = "x2", beta0 = NULL)
  # object <- lm_fit
  # clustid = voters$group_id
  # #B <- 10000
  # seed <- 1
  # param <- "treatment"
  # beta0 <- 0
  
  preprocess <- preprocess_lm(object = object, 
                              param = param, 
                              clustid = clustid, 
                              beta0 = beta0,
                              alpha = alpha)

  res <- boot_algo(preprocess)
  
  # Invert p-value
  point_estimate <- object$coefficients[param]
  
  
  if(is.null(conf_int) || conf_int == TRUE){
    
    # calculate guess for covariance matrix and standard errors
    vcov <- sandwich::vcovCL(object, cluster = voters$group_id)
    coefs <- lmtest::coeftest(object, vcov)
    se_guess <- coefs[param, "Std. Error"]

    res_p_val <- invert_p_val_fwc(object = object, 
                                  point_estimate = point_estimate,
                                  se_guess = se_guess,
                                  #data = data,
                                  clustid = preprocess$clustid,
                                  X = preprocess$X,
                                  Y = preprocess$Y,
                                  param = param,
                                  R0 = preprocess$R0,
                                  alpha = preprocess$alpha,
                                  N = preprocess$N, 
                                  k = preprocess$k, 
                                  B = B,
                                  invXX = res$invXX,
                                  v = res$v,
                                  Xr = res$Xr,
                                  XinvXXr = res$XinvXXr,
                                  SXinvXXRX = res$SXinvXXRX)
    
    res_final <- list(point_estimate = point_estimate, 
                      p_val = res[["p_val"]], 
                      conf_int = res_p_val$conf_int, 
                      p_test_vals = res_p_val$p_test_vals, 
                      test_vals = res_p_val$test_vals,
                      t_stat = res$t_stat, 
                      regression = res$object, 
                      param = param, 
                      N = preprocess$N, 
                      B = B, 
                      clustid = clustid, 
                      #depvar = depvar, 
                      N_G = preprocess$N_G)
  } else{
    res_final <- list(point_estimate = point_estimate, 
                      p_val = res[["p_val"]], 
                      t_stat = res$t_stat, 
                      conf_int = res$conf_int, 
                      regression = res$object, 
                      param = param, 
                      N = preprocess$N, 
                      B = B, 
                      clustid = clustid, 
                      #depvar = depvar, 
                      N_G = preprocess$N_G)     
  }
  
  class(res_final) <- "boottest"
  
  res_final
}