preprocess.lm <- function(object, param, clustid, beta0, alpha){
  
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
  
  if(clustid_dims == 2){
    #clustid_1 <- names(clustid)[1]
    #clustid_2 <- names(clustid)[2]
    names(clustid) <- c("clustid_1", "clustid_2")
    #clustid <- paste0(clustid_1, "-", clustid_2)
    clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
  }
  
  
  
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
                         clustid_dims = clustid_dims,
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
  if(clustid_dims == 1){
    class(res_preprocess) <- "oneclust"
  } else if(clustid_dims > 1){
    class(res_preprocess) <- "multclust"
  }
  res_preprocess
  
}



preprocess.felm <- function(object, param, clustid, beta0, alpha, demean){
  
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
  
  fml <- Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment")))
  fml_exclude_fe <- suppressWarnings(formula(fml, lhs = 1, rhs = 1))
  fml_only_fe <- suppressWarnings(formula(fml, lhs = 0, rhs = 2))
  fml_cluster <- suppressWarnings(formula(fml,lhs = 0, rhs = 4))
  
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
  
  N_G <- nrow(unique(clustid)) #number of clusters
  if(N_G > 200){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  }
  
  
  if(!(param %in% c(rownames(object$coefficients)))){
    stop("Parameter to test not in model or all. Please specify appropriate parameters to test.")
  }
  
  # how many clustids? uniway/multiway?
  clustid_dims <- ncol(clustid)
  if(clustid_dims == 2){
    #clustid_1 <- names(clustid)[1]
    #clustid_2 <- names(clustid)[2]
    names(clustid) <- c("clustid_1", "clustid_2")
    #clustid <- paste0(clustid_1, "-", clustid_2)
    clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
  }
  
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
                         clustid_dims = clustid_dims, 
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
  
  if(clustid_dims == 1){
    class(res_preprocess) <- "oneclust"
  } else if(clustid_dims > 1){
    class(res_preprocess) <- "multclust"
  }
  res_preprocess  
}


preprocess.fixest <- function(object, param, clustid, beta0, alpha, demean){
  
  #' function that pre-processes regression objects of type fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param demean If TRUE, fixed effects are projected out prior to the bootstrap. FALSE by default
  #'@return preprocessed object of class boottest_preprocessed
  
  # object <- feols(proposition_vote ~ treatment + ideology1 + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
  # param <- "treatment"
  # clustid <- ~ group_id1
  # beta0 = 0
  # alpha = 0.05
  demean = FALSE
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
  N_G <- nrow(unique(clustid)) #number of clusters
  
  if(N_G > 200){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  }
  
  if(numb_clusters == 2){
    clustid[, clustid12 := paste0(get(names(clustid)[1]), "-", get(names(clustid)[2]))]
  }
  
  if(!(param %in% c(names(object$coefficients)))){
    stop("Parameter to test not in model or all. Please specify appropriate parameters to test.")
  }
  
  # how many clustids? uniway/multiway?
  clustid_dims <- ncol(clustid)
  
  if(clustid_dims == 2){
    #clustid_1 <- names(clustid)[1]
    #clustid_2 <- names(clustid)[2]
    names(clustid) <- c("clustid_1", "clustid_2")
    #clustid <- paste0(clustid_1, "-", clustid_2)
    clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
  }
  
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
                         clustid_dims = clustid_dims, 
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
  
  if(clustid_dims == 1){
    class(res_preprocess) <- "oneclust"
  } else if(clustid_dims > 1){
    class(res_preprocess) <- "multclust"
  }
  res_preprocess  
  
}
