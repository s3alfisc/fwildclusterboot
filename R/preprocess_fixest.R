preprocess.fixest <- function(object, param, clustid, bootcluster, beta0, alpha, fe, seed, ...){
  
  #' function that pre-processes regression objects of type fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param bootcluster The bootstrap sampling cluster. 
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param fe A character scalar - fixed effect to be projected out, or NULL
  #'@param seed Integer. Sets the seed
  #'@param ... Further arguments passed to or from other methods.
  #'@importFrom stats formula model.frame model.matrix model.response 
  #'@method preprocess fixest
  #'@export
  #'@return preprocessed object of class boottest_preprocessed
  
  
  # Part 1) Check Arguments
  check_arg(clustid, "character scalar | character vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(alpha, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL")
  
  
  if(!is.null(seed)){
    seed <- seed
  } else if(is.null(seed)){
    seed <- 2
  }
  
  set.seed(seed)
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  
  
  if(!(param %in% c(names(object$coefficients)))){
    stop(paste("The parameter", param, "is not included in the estimated model. Maybe you are trying to test for an interaction parameter? To see all model parameter names, run names(coef(model))."))
  }
  
  # if(!(bootcluster %in% c(names(object$coefficients), clustid))){
  #   stop(paste("The parameter", param, "is not included in the estimated model. Maybe you are trying to test for an interaction parameter? To see all model parameter names, run names(coef(model))."))
  # }
  
  if(is.null(beta0)){
    beta0 <- 0
  }
  

  # Part 3) preprocess the covariates, depvar and fixed effects
  # 4 different cases for fixed effect:
  # - only one fixed effect specified in feols() and fe = NULL
  # - only one fixed effect specified in feols() and fe specified
  # - more than one fixed effect specified in feols() and fe = NULL
  # - more than one fixed effect specified in feols() and fe != NULL
  # Handling of missing values in cluster variables: 
  # - boottest allows for on the fly adjustment of clustering variables
  # - Case1: the clustering variables in feols() and boottest() are the same -> no problem with NAs
  # - Case2: the clustering variables in feols() and boottest() are different
  #       - a) boottest has clustering variable not in feols clusters but covs -> Sol: no problem, NA's already accounted for due to covariate
  #       - b) boottest has clustering var not in feols clusters not in covs -> Sol: drop missing and add warning
  #       - c) feols has clustering var that is also a covariate which is not a cluster var in boottest -> no problem: NAs already accounted for due to covariate
  #       - d) feols has clustering var that is not also a covariate which is not a cluster var in boottest -> Sol: work with smaller sample
  
  model_coef_names <- names(object$coefficients)
  #model_clustid_names <- names()
  model_fe_names <- object$fixef_vars
  numb_fe <- length(model_fe_names)
  model_param_names <- c(model_coef_names, model_fe_names)
  
  if(numb_fe == 0 & !is.null(fe)){
    stop("Your model does not containg a fixed effect. Therefore, no fixed effect can be projected out during the bootstrap.", 
         call. = FALSE)
  }
  
  # add fixed effect to formula 
  fml <- object$fml
  if(is.null(model_fe_names)){
    fml_fe <- fml
  } else {
    fe_update <- paste("~ . +",paste(model_fe_names, collapse = " + "))
    fml_fe <- update(fml, fe_update)
  }
  
  # this is needed for fixest as if cluster = NULL, this is not output of the feols object
  model_clustid_names <- eval(object$call$cluster)
  if(!is.null(model_clustid_names)){
    update_model_cluster <- paste("~ . +",paste(model_clustid_names, collapse = " + "))
    fml_all <- update(fml_fe, update_model_cluster)
  } else {
    fml_all <- fml_fe
  }
  
  model_names <- c(model_clustid_names, model_param_names)
  boot_names <- c(clustid, model_param_names)
  
  add_clusters <- setdiff(clustid, model_names)
  drop_clusters <- setdiff(model_clustid_names, boot_names)
  
  add_any_clusters <- ifelse(length(add_clusters) > 0, 1, 0)
  drop_any_clusters <- ifelse(length(drop_clusters) > 0, 1, 0)
  
  
  if(add_any_clusters == 0 & drop_any_clusters == 0){
    data_boot <- model.frame(formula = fml_all, 
                             data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                             drop.unused.levels = TRUE)
  } else if(add_any_clusters == 1 & drop_any_clusters == 0){
    data_model <- model.frame(formula = fml_all, 
                              data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                              drop.unused.levels = TRUE)
    
    add_update <- paste("~ . +",paste(add_clusters, collapse = " + "))
    fml_all <- update(fml_all, add_update)
    
    data_boot <- model.frame(formula = fml_all, 
                             data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                             drop.unused.levels = TRUE)

    data_diff <- nrow(data_model) - nrow(data_boot)
    
    # print a warning if observations needed to be deleted
    if(data_diff > 0){
        if(length(diff) == 1){
          warning(paste(data_diff, "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."), 
                  call. = FALSE)
        } else if(length(diff) > 1){
          warning(paste(data_diff, "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."), 
                  call. = FALSE)
        }
    }
    
  } else if(add_any_clusters == 0 & drop_any_clusters == 1){
    drop_update <- paste("~ . +",paste(drop_clusters, collapse = " + "))
    fml_all <- update(fml_all, drop_update)
    data_boot <- model.frame(formula = fml_all, 
                             data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                             drop.unused.levels = TRUE)
    
  } else if(add_any_clusters == 1 & drop_any_clusters == 1){
    # here: no rows are dropped, only columns
    drop_update <- paste("~ . +",paste(drop_clusters, collapse = " + "))
    fml_all <- update(fml_all, drop_update)
    data_model <- model.frame(formula = fml_all, 
                              data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                              drop.unused.levels = TRUE)
    
    add_update <- paste("~ . +",paste(add_clusters, collapse = " + "))
    fml_all <- update(fml_all, add_update)
    
    data_boot <- model.frame(formula = fml_all, 
                             data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                             drop.unused.levels = TRUE)
    
    data_diff <- nrow(data_model) - nrow(data_boot)   
    
    # print a warning if observations needed to be deleted
    if(data_diff > 0){
      if(length(diff) == 1){
        warning(paste(data_diff, "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."), 
                call. = FALSE)
      } else if(length(diff) > 1){
        warning(paste(data_diff, "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."), 
                call. = FALSE)
      }
    }
    
  }
  

  # ---------------------------------------------------------------------------- # 
  # preprocess clusters
  # ---------------------------------------------------------------------------- #
  
  cluster_names <- clustid
  clustid <- as.data.frame(data_boot[, clustid], stringsAsFactors = FALSE)
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
  
  if(!is.null(fe)){
    
    if(numb_fe == 1){
      # no need for clustering variables in fe formulas
      #fe_update2 <- paste("~ . -",paste(model_fe_names, collapse = " + "))
      # fml without fixed effect
      fml_design <- fml
      # get rid of constant due to fe out-projection
    } else if(numb_fe > 1){
      # delete fe to be projected out from fml_design but keep other fe in formula
      fe_update2 <- paste("~ . +",paste(model_fe_names[model_fe_names != fe], collapse = " + "))
      fml_design <- update(fml, fe_update2)
      # get not rid of constant
    }
    
    model_frame <- model.frame(update(fml_design, . ~ . - 1), data_boot)
    #model_frame <- model.frame(fml_fe, data_boot)
 
    X <- model.matrix(model_frame, data = data_boot)
    Y <- model.response(model_frame)
    
    N <- nrow(X)
    k <- ncol(X)
    
    
    fixed_effect <- as.data.frame(data_boot[, fe])
    #names(fixed_effect) <- "fe"
    
    # demean X and Y 
    X <- collapse::fwithin(X, fixed_effect[, 1])#
    #X <- collapse::fwithin(model_frame[names(model_frame) != fe], fixed_effect$fe)
    #X_fixest <- fixest::demean(X, fixed_effect[, 1])
    Y <- collapse::fwithin(Y, fixed_effect[, 1])
    fixed_effect_W <- fixed_effect[, 1]
    levels(fixed_effect_W) <- 1 / table(fixed_effect)
    W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
    n_fe <- length(unique(fixed_effect[, 1]))
  } else{
    model_frame <- model.frame(fml_fe, data_boot)
    X <- model.matrix(model_frame, data = data_boot)
    Y <- model.response(model_frame)
    
    N <- nrow(X)
    k <- ncol(X)
    W <- NULL
    n_fe <- NULL
    fixed_effect <- NULL
  }
  
  R0 <- as.numeric(param == colnames(X))
  
  # ---------------------------------------------------------------------------- # 
  # collect everything
  # ---------------------------------------------------------------------------- # 
  
  
  res_preprocess <- list(fixed_effect = fixed_effect, 
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
                         alpha = alpha, 
                         n_fe = n_fe, 
                         W = W, 
                         seed = seed, 
                         bootcluster = bootcluster, 
                         vcov_sign = vcov_sign)
  
  class(res_preprocess) <- "preprocess"
  res_preprocess  
  
}
