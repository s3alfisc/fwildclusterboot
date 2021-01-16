preprocess.lm <- function(object, param, clustid, bootcluster, beta0, alpha, seed, ...){
  
  #' function that pre-processes regression objects of type lm
  #'@param object An object of class lm
  #'@param clustid A character vector containing the names of the cluster variables
  #'@param bootcluster The bootstrap sampling cluster. 
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
  
  model_param_names <- c(names(object$coefficients))
  cluster_names <- clustid
  
  if(!(param %in% model_param_names)){
    stop(paste("The parameter", param, "is not included in the estimated model. Maybe you are trying to test for an interaction parameter? To see all model parameter names, run names(coef(model))."))
  }
  
  if(sum(!(names(clustid)) %in% model_param_names)>1){
    clustid_not_model <- TRUE
  }
  
  if(is.null(beta0)){
    beta0 <- 0
  }
  
  # ---------------------------------------------------------------------------- # 
  # preprocess clusters
  # ---------------------------------------------------------------------------- #
  clustid <- as.data.frame(data_clustid[, clustid], stringsAsFactors = FALSE)
  clustid_dims <- ncol(clustid)
  
  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)
  
  # taken from multiwayvcov::cluster.boot
  acc <- list()
  for (i in 1:clustid_dims) {
    acc <- append(acc, combn(1:clustid_dims, i, simplify = FALSE))
  }
  
  vcov_sign <- sapply(acc, function(i) (-1)^(length(i) + 1))
  acc <- acc[-1:-clustid_dims]
  
  if (clustid_dims > 1) {
    for (i in acc) {
      clustid <- cbind(clustid, Reduce(paste0, clustid[,i]))
      names(clustid)[length(names(clustid))] <- Reduce(paste0, names(clustid[, i]))
      #cluster_names <- cbind(cluster_names, Reduce(paste0, clustid[,i]))
    }
  }
  
  N_G <- sapply(clustid, function(x) length(unique(x)))
  
  # create a bootcluster vector
  if(bootcluster == "max"){
    bootcluster <- clustid[which.max(N_G)]
  } else if(bootcluster == "min"){
    bootcluster <- clustid[which.min(N_G)]
  } else if(length(bootcluster == 1) && bootcluster %in% c(model_param_names, cluster_names)){
    bootcluster <- clustid[which(names(clustid) == bootcluster)] 
  } else if(length(bootcluster) > 1 ){
    bootcluster <- Reduce(paste0, data_boot[, bootcluster])
  }
  
  # if(!(is.null(clustid_dims) || clustid_dims == 1 || clustid_dims == 2)){
  #   stop("boottest() currently only works for two-dimensional clustering.")
  # }
  

  # ---------------------------------------------------------------------------- # 
  # preprocess design matrix X and dependent variable y
  # ---------------------------------------------------------------------------- #
  
  model_frame <- model.frame(fml, data_clustid)
  X <- model.matrix(model_frame, data = data_clustid)
  Y <- model.response(model_frame)

  N <- length(Y)
  k <- ncol(X)

  R0 <- as.numeric(param == names(object$coefficients))
  
  
  # ---------------------------------------------------------------------------- # 
  # collect everything
  # ---------------------------------------------------------------------------- # 
  
  
  res_preprocess <- list(fixed_effect = NULL, 
                         data = data, 
                         param = param, 
                         clustid = clustid, 
                         clustid_dims = clustid_dims,
                         N = N, 
                         k = k, 
                         Y = Y, 
                         X = X, 
                         beta0 = beta0, 
                         clustid_dims, 
                         R0 = R0, 
                         N_G = N_G, 
                         alpha = alpha, 
                         W = NULL, 
                         n_fe = NULL, 
                         seed = seed, 
                         bootcluster = bootcluster, 
                         vcov_sign = vcov_sign)
  
     class(res_preprocess) <- "preprocess"

  res_preprocess
  
}


