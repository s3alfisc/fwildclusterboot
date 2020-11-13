preprocess.fixest <- function(object, param, clustid, beta0, alpha, demean){
  
  #' function that pre-processes regression objects of type fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param demean If TRUE, fixed effects are projected out prior to the bootstrap. FALSE by default
  #'@return preprocessed object of class boottest_preprocessed
  
  #data <- model.frame(object)
  
  data <- get_model_frame(object)
  
  if(is.null(object$fixef_vars)){
    fixed_effects <- NULL
    numb_fe <- NULL
  } else {
    fixed_effects <- get_model_fe(object)
    numb_fe <- ncol(fixed_effects)
    fml_only_fe <- formula(paste0("~",names(fixed_effects), collapse = "+"))
  }
  
  fml_exclude_fe <- formula(object$call$fml)

  
  # if non demeaning is chosen: set demeaning as default
  if(is.null(demean)){
    demean <- FALSE
  }
  

  if(is.null(numb_fe)){
    fixed_effects <- NULL
  }
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  
  if(!is.numeric(alpha) || alpha > 1 || alpha < 0 || length(alpha) > 1){
    stop("The level of significance alpha must be a numeric between 0 and 1")
  }

  N_G <- length(unique(clustid)) #number of clusters
  if(N_G > 200){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
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
  
  numb_clusters <- ncol(clustid)
  
  if(numb_clusters == 2){
    clustid[, clustid12 := paste0(get(names(clustid)[1]), "-", get(names(clustid)[2]))]
  }
  
  if(!(param %in% c(names(object$coefficients)))){
    stop("Parameter to test not in model or all. Please specify appropriate parameters to test.")
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
  
  
  # if fixed effects are specified, demean: 
  if(!is.null(numb_fe) & demean == TRUE){
    demean_data <- fixest::demean(data, 
                                  fixed_effects, 
                                  tol = 1e-06)
    #names(fixed_effects) <- paste0("fixed_effect_", 1:ncol(fixed_effects))
    # data is the demeaned data if fe are used
    data <- as.data.frame(demean_data)
    model_frame <- model.frame(fml_exclude_fe, data = data)
    Y <- model.response(model_frame)
    X <- model.matrix(model_frame, data = data)
    # note: why do I take out (Intercept) from fixed effcts R0 but not in no_fixef?
    R0 <- as.numeric(param == c("(Intercept)", names(object$coefficients)))
  } else if(!is.null(numb_fe) & demean == FALSE){
    model_frame_fe <- model.frame(fml_only_fe, data = fixed_effects)
    X_fe <- model.matrix(model_frame_fe, data = fixed_effects)
    # update: get rid of intercept (because of fe)
    model_frame <- model.frame(update(fml_exclude_fe, ~ . +  0), data = data)
    X <- model.matrix(model_frame, data = data)
    X <- cbind(X, X_fe)
    Y <- model.response(model_frame)
    #fe_dummies <- fastDummies::dummy_cols(fixed_effects, remove_first_dummy = TRUE, ignore_na = TRUE)
  } else {
    # case where is.null(numb_fe) == TRUE
    model_frame <- model.frame(fml_exclude_fe, data = data)
    X <- model.matrix(model_frame, data = data)
    Y <- model.response(model_frame)
  }
  
  R0 <- as.numeric(param == colnames(X))
  
  # model_frame <- model.frame(formula, data = data)
  # Y <- model.response(model_frame)
  # X <- model.matrix(model_frame, data = data)
  #}
  
  N <- length(Y)
  k <- ncol(X)
  
  res_preprocess <- list(fixed_effects = fixed_effects, 
                         param = param, 
                         data = data, 
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




boottest.fixest  <- function(object, 
                           clustid, 
                           param, 
                           B,
                           data,
                           alpha = NULL, 
                           fixed_effects = NULL, 
                           weights = NULL,
                           conf_int = NULL, 
                           debug = FALSE, 
                           seed = NULL, 
                           beta0 = 0, 
                           demean = NULL){
  
  
  #' Computes wild cluster bootstrap for object of class fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param demean If TRUE, fixed effects are projected out prior to the bootstrap. FALSE by default
  #'@return An object of class boottest
  #'@export
  #'@method boottest fixest


  # Step 1: check arguments of feols call
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
  
  
 
  preprocess <- preprocess.fixest(object = object, 
                                  param = param,
                                  clustid = clustid,
                                  beta0 = beta0,
                                  alpha = alpha,
                                  demean = demean)
  
  res <- boot_algo(preprocess)
 
  point_estimate <- object$coefficients[param]
  
  
  if(is.null(conf_int) || conf_int == TRUE){
    # calculate sandwich cluster se
    coefs <- fixest:::coeftable(object, se = "cluster", cluster = clustid)
    se_guess <- coefs[param, "Std. Error"]
    point_estimate <- coefs[param, "Estimate"]
    
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
