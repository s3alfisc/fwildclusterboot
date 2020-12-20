preprocess.felm <- function(object, param, clustid, beta0, alpha, fe){
  
  #' function that pre-processes regression objects of type fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@return preprocessed object of class boottest_preprocessed
  
  # print warnings as they occur
  #options(warn=1)
  
  # object <- feols(proposition_vote ~ treatment + ideology1 + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
  # param <- "treatment"
  # clustid <- ~ group_id1
  # beta0 = 0
  # alpha = 0.05
  
  # Part 1) Check Arguments
  #check_arg(clustid, "character scalar | character vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(alpha, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL")
  
  # PArt 2: Check arguments further
  # if(!(fe %in% object$fixef_vars) | !is.null(fe)){
  #   stop("The fixed effect in boottest() is not a fixed effect in the model.")
  # }
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  
  if(!is.numeric(alpha) || alpha > 1 || alpha < 0 || length(alpha) > 1){
    stop("The level of significance alpha must be a numeric between 0 and 1")
  }
  
  
  if(!is.null(object$call$weights)){
    stop("Function currently does not allow weights.")
  }
  
  
  if(!(param %in% rownames(object$coefficients))){
    stop("Parameter to test not in model or all. Please specify appropriate parameters to test.")
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
  model_clustid_names <- names(object$clustervar)
  model_covariate_names <- c(model_coef_names, model_fe_names)
  
  # do not allow for drop of variable that is only used as cluster bot not covariate
  not_in <- sum(!model_clustid_names %in% clustid) 
  if(not_in != 0){
    not_in_clustvar <- model_clustid_names[!model_clustid_names %in% clustid]
    not_in_covariates <- ! not_in_clustvar %in% model_covariate_names
    if(not_in_covariates == TRUE){
      stop(paste("The cluster variable", not_in_clustvar, "has been specified as a clustering variable in the felm() model, is further not part of the covariates, but has not been listed as a clustering variable in boottest(). boottest() does not allow for such a case -  "))
    }
  } 

  
  fml_wo_fe <- formula(Formula::Formula(object$formula), lhs = 1, rhs = 1)
  fml_fe <- formula(Formula::Formula(object$formula), lhs = 0, rhs = 2)
  fml_cluster <- formula(Formula::Formula(object$formula), lhs = 0, rhs = 4)
  fml_iv <-  formula(Formula::Formula(object$formula), lhs = 0, rhs = 3)
  
  numb_fe <- length(model_fe_names)
  
  
  if(fml_fe == ~0){
    fml_all_clustid <- formula(paste0(as.character(fml_wo_fe), "+", paste0(c(unique(clustid, model_clustid_names)), collapse = "+")))
    if(fml_cluster == ~0){
      fml_all_cluster <- fml_wo_fe
    } else{
      fml_all_cluster <- formula(paste0(as.character(fml_wo_fe), "+", paste0(c(model_clustid_names), collapse = "+")))
    }
  } else {
    # depvar, covariates, fe and cluster vars from felm()
    fml_all_cluster <- formula(paste0(as.character(fml_wo_fe), "+", paste0(c(model_fe_names, model_clustid_names), collapse = "+")))
    # depvar, covariates, fe and cluster vars from felm() and boottest
    fml_all_clustid <- formula(paste0(as.character(fml_wo_fe), "+", paste0(c(model_fe_names, unique(clustid, model_clustid_names)), collapse = "+")))
  }

  depvar <- colnames(object$coefficients)
  
  
  data_cluster <- model.frame(formula = fml_all_cluster, 
                      data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                      drop.unused.levels = TRUE) 
  
  data_clustid <- model.frame(formula = fml_all_clustid, 
                          data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                          drop.unused.levels = TRUE)  
  
  data_diff <- nrow(data_cluster) - nrow(data_clustid)
  
  if(data_diff == 1){
    warning(paste(data_diff, "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
  } else if(data_diff > 1){
    warning(paste(data_diff, "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
  } else if(data_diff < 0){
    stop("nrow(data_all) < nrow(data_diff) - this cannot be correct.")
  }
  
  # not needed - no post-estimation allowed
  #data_diff <- nrow(data) - nrow(data_all)
  # 
  # if(data_diff == 1){
  #   warning(paste(data_diff, "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
  # } else if(data_diff > 1){
  #   warning(paste(data_diff, "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
  # }
  
  # now create clusters 
  clustid <- as.data.frame(data_clustid[, clustid])
  
  clustid_dims <- ncol(clustid)
  if(is.null(clustid_dims)){clustid_dims <- 1}
  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)
  if(clustid_dims == 2){
    names(clustid) <- c("clustid_1", "clustid_2")
    clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
  }
  
  
  
  
  N_G <- sapply(clustid, function(x) length(unique(x)))
  
  if(clustid_dims == 1){
    if(max(N_G) > 200){
      warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
    }
  } else if(clustid_dims > 1){
    if(max(N_G) > 200){
      warning(paste("You are estimating a model with more than 200 clusters. The more clusters in the data, the longer the estimation process."))
    }
  }
  

  
  
  # now create fixed effects
  if(!is.null(fe)){
    
    # create covariates X and dependent variable Y
    if(numb_fe == 1){
      fml_design <- fml_wo_fe
    } else if(numb_fe > 1){
      fml_design <- formula(paste0(as.character(fml_wo_fe), "+", paste0(model_fe_names[names(model_fe_names) != fe], collapse = "+")))
    }

    model_frame <- model.frame(fml_design, data_clustid)
    X <- model.matrix(model_frame, data = data_clustid)
    Y <- model.response(model_frame)
    
    N <- nrow(X)
    k <- ncol(X)
    
    fixed_effect <- as.data.frame(data_clustid[, fe])
    # demean X and Y 
    X <- collapse::fwithin(X, fixed_effect)#
    Y <- collapse::fwithin(Y, fixed_effect)
    fixed_effect_W <- fixed_effect[, 1]
    levels(fixed_effect_W) <- 1 / table(fixed_effect)
    W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
    n_fe <- length(unique(fixed_effect))
  } else{
    
    model_frame <- model.frame(fml_all_clustid, data_clustid)
    X <- model.matrix(model_frame, data = data_clustid)
    Y <- model.response(model_frame)
    
    N <- nrow(X)
    k <- ncol(X)
    
    W <- NULL
    n_fe <- NULL
    fixed_effect <- NULL
  }
  
  R0 <- as.numeric(param == colnames(X))
  
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
                         W = W)
  
  if(clustid_dims == 1){
    class(res_preprocess) <- "oneclust"
  } else if(clustid_dims > 1){
    class(res_preprocess) <- "multclust"
  }
  res_preprocess  
  
}
