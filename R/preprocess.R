preprocess.lm <- function(object, param, clustid, beta0, alpha, seed, ...){
  
  #' function that pre-processes regression objects of type lm
  #'@param object An object of class lm
  #'@param clustid A character vector containing the names of the cluster variables
  #'@param param A scalar character. The coefficient for which a hypothesis is to be tested
  #'@param beta0 A numeric scalar. Shifts the null hypothesis  
  #'@param alpha A numeric scalar between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param seed An integer. Sets the seed
  #'@param ... Further arguments passed to or from other methods.
  #'@importFrom stats formula model.frame coef as.formula model.matrix model.response 
  #'@method preprocess lm
  #'@export
  #'@return preprocessed object of class boottest_preprocessed

  # print warnings as they occur
  #options(warn=1)
  
  check_arg(clustid, "character scalar | character vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(alpha, "numeric scalar | NULL")
  
  if(!is.null(seed)){
    seed <- seed
  } else if(is.null(seed)){
    seed <- 2
  }
  
  set.seed(seed)
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  
  

  clustid_update <- paste("~ . +",paste(clustid, collapse = " + "))
  
  fml <- as.formula(object$call$formula)
  fml_all_clustid <- update(fml, clustid_update)
  
  data <- model.frame(formula = fml, 
                      data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                      drop.unused.levels = TRUE) 
  data_clustid <- model.frame(formula = fml_all_clustid, 
                      data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                      drop.unused.levels = TRUE) 
  
  data_diff <- nrow(data) - nrow(data_clustid)
  
  if(data_diff == 1){
    warning(paste(data_diff, 
                  "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."),
            call. = FALSE)
  } else if(data_diff > 1){
    warning(paste(data_diff, 
                  "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."), 
            call. = FALSE)
  } else if(data_diff < 0){
    stop("nrow(data_cluster) < nrow(data_clustid) - this cannot be correct.", 
         call. = FALSE)
  }
  
  clustid <- as.data.frame(data_clustid[, clustid])
  
  #formula <- object$call$fml
  weights <- object$call$weights
  
  if(!is.null(weights)){
    stop("The boottest function currently does not allow for regression weights. The argument weights needs to be NULL.")
  }
  

  
  # # retrieve clusters / multiple clusters
  # if(inherits(clustid, "formula")) {
  #   clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
  #   clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
  # } else {
  #   clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
  # }
  
  if(!(param %in% c(names(object$coefficients)))){
    stop(paste("The parameter", param, "is not included in the estimated model. Maybe you are trying to test for an interaction parameter? To see all model parameter names, run names(coef(model))."))
  }
  
  if(sum(!(names(clustid)) %in% c(names(object$coefficients)))>1){
    clustid_not_model <- TRUE
  }
  
  # how many clustids? uniway/multiway?
  clustid_dims <- ncol(clustid)
  
  
  # # Handle omitted or excluded observations
  # if(!is.null(object$na.action)) {
  #   if(class(object$na.action) == "exclude") {
  #     clustid <- clustid[-object$na.action,]
  #   } else if(class(object$na.action) == "omit") {
  #     clustid <- clustid[-object$na.action,]
  #   }
  #   clustid <- as.data.frame(clustid)  # silly error somewhere
  # }
  
  # clustid_na <- is.na(clustid)
  # delete_clustid <- rowSums(clustid_na)
  # delete_clustid_sum <- sum(rowSums(clustid_na))
  # 
  # if(delete_clustid_sum > 0){
  #   warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
  # }
  # 
  # data <- data[-delete_clustid, ]
  
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
  
  if(clustid_dims == 1){
    names(clustid) <- c("clustid")
  } else if(clustid_dims == 2){
    #clustid_1 <- names(clustid)[1]
    #clustid_2 <- names(clustid)[2]
    names(clustid) <- c("clustid_1", "clustid_2")
    #clustid <- paste0(clustid_1, "-", clustid_2)
    clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
    clustid_dims <- 3
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
  
  
  #depvar <- all.vars(as.formula(object$call))[1]
  #measurevar <- "y"
  #formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  
  model_frame <- model.frame(fml, data_clustid)
  X <- model.matrix(model_frame, data = data_clustid)
  Y <- model.response(model_frame)

  #res <- boottest_fun(Y = Y, X = X, R0 = R0, clustid = clustid, B = B, param = param)
  
  N <- length(Y)
  k <- ncol(X)
  
  #Xr1 <- X
  #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
  
  # fixed_effect <- NULL
  # W <- NULL
  
  #clustid <- as.vector(clustid)
  #clustid <- rep(1:20, 100)
  N_G <- sapply(clustid, function(x) length(unique(x)))
  
  #numb_clusters <- ncol(clustid)
  # if(clustid_dims == 1){
  #   if(max(N_G) > 200){
  #     warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  #   }
  # } else if(clustid_dims > 1){
  #   if(max(N_G) > 200){
  #     warning(paste("You are estimating a model with", max(N_G), "clusters. Note that the speed gains from the algorithm underlying fwildclusterboot mainly apply for small number of clusters."))
  #   }
  # }

  
  
  res_preprocess <- list(fixed_effect = NULL, 
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
                         alpha = alpha, 
                         W = NULL, 
                         n_fe = NULL, 
                         seed = seed)
  if(clustid_dims == 1){
    class(res_preprocess) <- "oneclust"
  } else if(clustid_dims > 1){
    class(res_preprocess) <- "multclust"
  }
  res_preprocess
  
}


