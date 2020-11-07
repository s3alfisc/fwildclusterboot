preprocess.fixest <- function(object, param, clustid, beta0){
  
  data <- get_model_frame(object)
  #try_fe <- suppressWarnings(try(get_model_fe(object)))
  fixed_effects <- try(get_model_fe(object), TRUE)
  numb_fe <- ncol(fixed_effects)
  
  if(is.null(numb_fe)){
    fixed_effects <- NULL
  }

  N_G <- length(unique(clustid)) #number of clusters
  if(N_G > 2000){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  }
  
  # if fixed effects are specified, demean: 
  if(!is.null(numb_fe)){
    demean_data <- fixest::demean(data, 
                                  fixed_effects, 
                                  tol = 1e-06)
    names(fixed_effects) <- paste0("fixed_effect_", 1:ncol(fixed_effects))
    # data is the demeaned data if fe are used
    data <- as.data.frame(demean_data)
    # note: why do I take out (Intercept) from fixed effcts R0 but not in no_fixef?
    R0 <- as.numeric(param == c("(Intercept)", names(object$coefficients)))
  } else{
    R0 <- as.numeric(param == c(names(object$coefficients)))
  }
  
  if(!is.null(object$call$weights)){
    stop("Function currently does not allow weights.")
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
  
  numb_clusters <- ncol(clustid)
  
  if(numb_clusters == 2){
    clustid[, clustid12 := paste0(get(names(clustid)[1]), "-", get(names(clustid)[2]))]
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
  
  #groupvars <- names(coef(object))
  
  # if(object_type == "felm"){
  #   
  #   depvar <- names(object$response)
  #   Y <- object$response
  #   X <- lfe:::model.matrix.felm(felm_fit) 
  # }
  
  
  #depvar <- all.vars(as.formula(object$call$fml))[1]
  
  #depvar <- names(object$response)
  
  formula <- object$call$fml
  model_frame <- model.frame(formula, data = data)
  Y <- model.response(model_frame)
  X <- model.matrix(model_frame, data = data)
  #}
  
  N <- length(Y)
  k <- ncol(X)
  
  res_preprocess <- list(fixed_effects = fixed_effects, 
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
                         N_G = N_G)
  
  res_preprocess
  
}




boottest.fixest  <- function(object, 
                           clustid, 
                           param, 
                           B,
                           data,
                           fixed_effects = NULL, 
                           weights = NULL,
                           conf_int = NULL, 
                           debug = FALSE, 
                           seed = NULL, 
                           beta0 = 0){
  
  
  #' Computes wild cluster bootstrap for object of class fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param data A data.frame containing the data
  #'@param conf_int A logical vector. If TRUE, boottest computes confidence intervals by p-value inversion
  #'@param seed An integer. Allows the user to set a seed
  #'@param beta0 A numeric. Shifts the null hypothesis
  #'@return An object of class boottest
  #'@export


  
  
  
  #boottest.lm(lm_fit, 1:2000, B = 1000, seed = 1, param = "x2", beta0 = NULL)
  # object <- feols_fit_1
  # clustid = voters$group_id
  # B <- 10000
  # seed <- 1
  # param <- "treatment"
  # beta0 <- 0
  # conf_int <- TRUE
  # 
 #  data <- get_model_frame(object)
 #  
 #  # if fixed effects are specified, demean: 
 #  if(!is.null(object$call$fixef)){
 #    use_fixef <- TRUE
 #    fixed_effects <- get_model_fe(object)
 #    demean_data <- fixest::demean(data, 
 #                                  fixed_effects, 
 #                                  tol = 1e-06)
 #    names(fixed_effects) <- paste0("fixed_effect_", 1:ncol(fixed_effects))
 #    # data is the demeaned data if fe are used
 #    data <- as.data.frame(demean_data)
 #    # note: why do I take out (Intercept) from fixed effcts R0 but not in no_fixef?
 #    R0 <- as.numeric(param == c("(Intercept)", names(object$coefficients)))
 #  } else{
 #    use_fixef <- FALSE
 #    R0 <- as.numeric(param == c(names(object$coefficients)))
 #  }
 #  
 #  if(!is.null(object$call$weights)){
 #    stop("Function currently does not allow weights.")
 #  }
 #  
 #  
 #  
 #  
 #  if(!is.null(seed)){
 #    set.seed(seed)
 #  } else if(is.null(seed)){
 #    set.seed(2)
 #  }
 #  
 #  # retrieve clusters / multiple clusters
 #  if(inherits(clustid, "formula")) {
 #    clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
 #    clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
 #  } else {
 #    clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
 #  }
 #  
 #  numb_clusters <- ncol(clustid)
 #  
 # if(numb_clusters == 2){
 #    clustid[, clustid12 := paste0(get(names(clustid)[1]), "-", get(names(clustid)[2]))]
 #  }
 #  
 #  if(!(param %in% c(names(object$coefficients)))){
 #    warning("Parameter to test not in model or all. Please specify appropriate parameters to test.")
 #  }
 #  
 #  # how many clustids? uniway/multiway?
 #  clustid_dims <- ncol(clustid)
 #  
 #  
 #  # Handle omitted or excluded observations
 #  if(!is.null(object$na.action)) {
 #    if(class(object$na.action) == "exclude") {
 #      clustid <- clustid[-object$na.action,]
 #    } else if(class(object$na.action) == "omit") {
 #      clustid <- clustid[-object$na.action,]
 #    }
 #    clustid <- as.data.frame(clustid)  # silly error somewhere
 #  }
 #  #if(debug) print(class(clustid))
 #  
 #  if(is.null(beta0)){
 #    beta0 <- 0
 #  }
 #  
 #  # Factors in our clustiding variables can potentially cause problems
 #  # Blunt fix is to force conversion to characters
 #  i <- !sapply(clustid, is.numeric)
 #  clustid[i] <- lapply(clustid[i], as.character)
 #  
 #  # Make all combinations of clustid dimensions
 #  # if(clustid_dims > 1) {
 #  #   for(i in acc) {
 #  #     clustid <- cbind(clustid, Reduce(paste0, clustid[,i]))
 #  #   }
 #  # }
 #  
 #  
 #  # start estimation here: 
 #  
 #  groupvars <- names(coef(object))
 #  
 #  # if(object_type == "felm"){
 #  #   
 #  #   depvar <- names(object$response)
 #  #   Y <- object$response
 #  #   X <- lfe:::model.matrix.felm(felm_fit) 
 #  # }
 #  
 #  
 #  #depvar <- all.vars(as.formula(object$call))[1]
 #  
 #  depvar <- names(object$response)
 #  
 #  formula <- object$call$fml
 #  model_frame <- model.frame(formula, data = data)
 #  Y <- model.response(model_frame)
 #  X <- model.matrix(model_frame, data = data)
 #  #}
 #  
 #  N <- length(Y)
 #  k <- ncol(X)
  
  preprocess <- preprocess.fixest(object = object, param = param, clustid = clustid, beta0 = beta0)
  
  # lapply(names(preprocess), function(i){
  #   name <- i
  #   assign(name, preprocess[[i]])}
  # )
  X <- preprocess$X
  Y <- preprocess$Y
  R0 <- preprocess$R0
  data <- preprocess$data
  N <- preprocess$N
  k <- preprocess$k
  clustid <- preprocess$clustid
  fixed_effects <- preprocess$fixed_effects
  beta0 <- preprocess$beta0
  N_G <- preprocess$N_G
  
    
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  #Xr1 <- X
  #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
  
  
  
  #clustid <- as.vector(clustid)
  #clustid <- rep(1:20, 100)
  #clustid <- clustid$clustid
  
  # error under the null hypothesis
  #u_hat <- Y - Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr) %*% Y # N x 1 matrix 
  u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
  
  #(rep(1, N) - Yr) %*% (Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr))
  
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
  numer <- SXinvXXRu %*% v 
  
  # if(use_fixef == FALSE){
    
    # "old" code - if no fixed effects used in calculation
    
    SXinvXXRX_prep <- data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)
    SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    
    SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
    SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    
    J <- (diag(SXinvXXRu) - SXinvXXRX  %*% invXX %*% t(SXu)) %*% v  
    
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
  conf_int <- invert_p_val_fwc(object, data, clustid, X, Y, param, R0, B, N, k, seed, N_g, invXX, v, Xr, XinvXXr, SXinvXXRX)
  res_final <- list(p_val = res[["p_val"]], 
                    conf_int = conf_int, 
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
