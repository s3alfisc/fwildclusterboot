preprocess_felm <- function(object, param, clustid, beta0, alpha, demean){
  
  #' function that pre-processes regression objects of type felm
  #'@param object An object of class felm
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param demean If TRUE, fixed effects are projected out prior to the bootstrap. FALSE by default
  #'@return preprocessed object of class boottest_preprocessed
  
  # object = felm_fit1
  # param = "treatment"
  # clustid = voters$group_id
  # beta0 = 0
  # alpha = 0.05
  # demean = FALSE 
  
  data <- get_model_frame(object)
  #try_fe <- suppressWarnings(try(get_model_fe(object)))
  
  if(length(object$fe) == 0){
    fixed_effects <- NULL
    numb_fe <- NULL
  } else {
    fixed_effects <- get_model_fe(object)
    # fixed_effects object$fe
    numb_fe <- ncol(fixed_effects)
    fml_only_fe <- formula(paste0("~",names(fixed_effects), collapse = "+"))
  }
  
  # if(!is.null(demean) || !is.logical(demean)){
  #   stop("demean needs to be null or a logical.")
  # }
  # 
  # if(is.null(numb_fe) & (is.null(demean) | demean != FALSE)){
  #   warning("The model does not include any fixed effects - in consequence, the 
  #           estimation proceeds without projecting out the (missing) fixed effects.")
  # }

  #fixed_effects <- suppressWarnings(try(get_model_fe(object), TRUE))
  #numb_fe <- ncol(fixed_effects)
  
  # if non demeaning is chosen: set demean to zero
  if(is.null(demean)){
    demean <- FALSE
  }
  
  if(is.null(numb_fe) || numb_fe == 0){
    fixed_effects <- NULL
    numb_fe <- NULL
  }
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  if(!is.numeric(alpha) || alpha > 1 || alpha < 0 || length(alpha) > 1){
    stop("The level of significance alpha must be a numeric between 0 and 1")
  }
  
  
  weights <- object$call$weights
  if(!is.null(weights)){
    stop("Currently, boottest does not support weighted least squares. weights 
         must be NULL.")
  }
  
  N_G <- length(unique(clustid)) #number of clusters
  if(N_G > 200){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  }
  
  fml <- Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment")))
  fml_exclude_fe <- suppressWarnings(formula(fml, lhs = 1, rhs = 1))
  fml_only_fe <- suppressWarnings(formula(fml, lhs = 0, rhs = 2))
  fml_cluster <- formula(fml,lhs = 0, rhs = 4)
  
  # if(fml_cluster == ~0){
  #   stop("In contrast to objects of class lm and fixest, you need to specify the desired level 
  #        of clustering for objects of type felm in the felm() estimation command")
  # }
  #use_fixed_effects <- suppressWarnings(formula(fml, lhs = 0, rhs = 2) == "~0")
  
  fml_test_iv <- suppressWarnings(formula(fml, lhs = 0, rhs = 3))
  if(fml_test_iv != ~0){
    stop("The boottest() function currently does not support instrumental variables
         estimation.")
  }
  
  if(!is.null(seed)){
    set.seed(seed)
  } else if(is.null(seed)){
    set.seed(2)
  }
  
  # retrieve clusters / multiple clusters
  if(inherits(clustid, "formula")) {
    clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
    clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
  } else {
    clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
  }
  
  if(!(param %in% c(rownames(object$coefficients)))){
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

  if(is.null(beta0)){
    beta0 <- 0
  }
  
  # Factors in our clustiding variables can potentially cause problems
  # Blunt fix is to force conversion to characters
  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)

  N_G <- nrow(unique(clustid)) #number of clusters
  if(N_G > 200){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  }
  
  # groupvars <- names(coef(object))
  # depvar <- names(object$response)
  
  #formula <- formula(Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment"))), lhs = 1, rhs = 1)
  
  #fe_formula <- formula(paste0("~", names(fixed_effects), collapse = "+"))
  
  
  # if fixed effects are specified, demean: 
  if(!is.null(numb_fe) & demean == TRUE){
    #if(!is.null(numb_fe)){
    demean_data <- fixest::demean(data, fixed_effects)
    data <- as.data.frame(demean_data) 
    model_frame <- model.frame(fml_exclude_fe, data = data)
    X <- model.matrix(fml_exclude_fe, data = data)
    Y <- model.response(model_frame)
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
  
  #if(demean == TRUE){

  #} else {
  #  model_frame <- model.frame(formula(paste("~", names(data), "+ 0")),
  #                             data = data)
  #  fe <- model.frame(formula(paste("~", names(fixed_effects), "+ 0")), 
  #                    fixed_effects)
  #  X <- model.matrix(fml_exclude_fe, data = data)
  #  X_fe <- model.matrix(fe, data = fixed_effects)
  #  X <- cbind(X, X_fe)
  #}


  R0 <- as.numeric(param == colnames(X))
  
  N <- length(Y)
  k <- ncol(X)

  res_preprocess <- list(fixed_effects = fixed_effects, 
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


boottest.felm  <- function(object, 
                           clustid, 
                           param, 
                           B,
                           weights = NULL,
                           conf_int = NULL, 
                           debug = FALSE, 
                           seed = NULL, 
                           beta0 = 0, 
                           alpha = NULL, 
                           demean = NULL){
  
  
  #' Function that runs boottest for object of class felm
  #'@import Formula
  #'@import data.table
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param B number of bootstrap iterations
  #'@param weights Regression weights. Currently, WLS is not supported, and weights needs to be NULL 
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a random seed
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@return An object of class boottest
  #'@export
  #'@method boottest felm
  
  
  preprocess <- preprocess_felm(object = object,
                                param = param,
                                clustid = clustid,
                                beta0 = beta0,
                                alpha = alpha,
                                demean = demean)

  res <- boot_algo(preprocess)
  
  # Invert p-value
  point_estimate <- felm_fit$coefficients[ param,]
  
  if(is.null(conf_int) || conf_int == TRUE){
    # 
    #se <- object$se[param]
    #point_estimate <- object$coefficients[param, ]
    #vcv <- felm_fit$robustvcv
    
    vcov <- sandwich::vcovCL(felm_fit, cluster = clustid)
    coefs <- lmtest::coeftest(object, vcov)
    se_guess <- coefs[param, "Std. Error"]
    #point_estimate <- coefs[param, "Estimate"]
    
    
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
    
    res_final <- list(p_val = res[["p_val"]], 
                      point_estimate = point_estimate,
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
    res_final <- list(p_val = res[["p_val"]], 
                      point_estimate = point_estimate,
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
  #res
  
} 

