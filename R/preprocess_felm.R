preprocess.felm <- function(object, param, clustid, bootcluster, beta0, alpha, fe, seed, ...){
  
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
  #'@export
  #'@method preprocess felm
  #'@return preprocessed object of class boottest_preprocessed

  
  # Part 1) Check Arguments
  check_arg(clustid, "character scalar | character vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(alpha, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL")
  
  # PArt 2: Check arguments further
  # if(!(fe %in% object$fixef_vars) | !is.null(fe)){
  #   stop("The fixed effect in boottest() is not a fixed effect in the model.")
  # }
  
  if(!is.null(seed)){
    seed <- seed
  } else if(is.null(seed)){
    seed <- 2
  }
  
  set.seed(seed)
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  

  if(!(param %in% rownames(object$coefficients))){
    stop(paste("The parameter", param, "is not included in the estimated model. Maybe you are trying to test for an interaction parameter? To see all model parameter names, run rownames(coef(model))."))
  }
  
  
  
  if(is.null(beta0)){
    beta0 <- 0
  }
  
  
  # Part 3) preprocess the covariates, depvar and fixed effects
  # 4 different cases:
  # - only one fixed effect specified in feols() and fe = NULL
  # - only one fixed effect specified in feols() and fe specified
  # - more than one fixed effect specified in feols() and fe = NULL
  # - more than one fixed effect specified in feols() and fe != NULL
  
  model_coef_names <- rownames(object$coefficients)
  model_fe_names <- names(object$fe)
  numb_fe <- length(model_fe_names)
  
  if(numb_fe == 0 & !is.null(fe)){
    stop("Your model does not containg a fixed effect. Therefore, no fixed effect can be projected out during the bootstrap.", 
         call. = FALSE)
  }
  
  #model_covariate_names <- c(model_coef_names, model_fe_names)

  model_param_names <- c(model_coef_names, model_fe_names)
  model_clustid_names <- names(object$clustervar)
  
  fml_felm <- Formula::Formula(eval(object$call$formula))
  
  fml_wo_fe <- suppressWarnings(formula(fml_felm, lhs = 1, rhs = 1))
  #fml_fe <- suppressWarnings(formula(fml_felm, lhs = 0, rhs = 2))
  fml_cluster <- suppressWarnings(formula(fml_felm, lhs = 0, rhs = 4))
  fml_iv <-  suppressWarnings(formula(fml_felm, lhs = 0, rhs = 3))
  
  fml_fe <- suppressWarnings(formula(fml_felm, lhs = 1, rhs = c(1, 2), collapse = TRUE))
  fml_all <- suppressWarnings(formula(fml_felm, lhs = 1, rhs = c(1, 2, 4), collapse = TRUE))
  
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
  
  
  
  numb_fe <- length(fe)
  # now create fixed effects
  if(!is.null(fe)){
    
    # create covariates X and dependent variable Y
    if(numb_fe == 1){
      fml_design <- fml_wo_fe
      # because fe is projected out, data matrix should not contain constant
      model_frame <- model.frame(update(fml_design, . ~ . - 1), data_boot)
    } else if(numb_fe > 1){
      fe_update2 <- paste("~ . +",paste(model_fe_names[model_fe_names != fe], collapse = " + "))
      fml_design <- update(fml_wo_fe, fe_update2)
      # don't drop constant
      model_frame <- model.frame(fml_design, data_boot)
    }

    X <- model.matrix(model_frame, data = data_boot)
    Y <- model.response(model_frame)
    
    N <- nrow(X)
    k <- ncol(X)
    
    fixed_effect <- as.data.frame(data_boot[, fe])
    # demean X and Y 
    X <- collapse::fwithin(X, fixed_effect[, 1])#
    Y <- collapse::fwithin(Y, fixed_effect[, 1])
    fixed_effect_W <- fixed_effect[, 1]
    levels(fixed_effect_W) <- 1 / table(fixed_effect)
    W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
    n_fe <- length(unique(fixed_effect[, 1]))
    #n_fe <- length(unique(fixed_effect))
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
