preprocess.fixest <- function(object, param, clustid, beta0, alpha, fe, seed, ...){
  
  #' function that pre-processes regression objects of type fixest
  #'@param object An object of class fixest
  #'@param clustid A vector with the clusters
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
  
  # print warnings as they occur
  #options(warn=1)
  
  # object <- feols(proposition_vote ~ treatment + ideology1 + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
  # param <- "treatment"
  # clustid <- ~ group_id1
  # beta0 = 0
  # alpha = 0.05
  
  # Part 1) Check Arguments
  check_arg(clustid, "character scalar | character vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(alpha, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL")
  
  # PArt 2: Check arguments further
  #if(!(fe %in% object$fixef_vars) | !is.null(fe)){
  #  stop("The fixed effect in boottest() is not a fixed effect in the model.")
  #}
  
  if(!is.null(seed)){
    seed <- seed
  } else if(is.null(seed)){
    seed <- 2
  }
  
  set.seed(seed)
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  
  if(!is.numeric(alpha) || alpha > 1 || alpha < 0 || length(alpha) > 1){
    stop("The level of significance alpha must be a numeric between 0 and 1")
  }
  
  if(!is.null(object$call$weights)){
    stop("The boottest function currently does not allow for regression weights. The argument weights needs to be NULL.")
  }
  
  
  if(!(param %in% c(names(object$coefficients)))){
    stop(paste("The parameter", param, "is not included in the estimated model. Maybe you are trying to test for an interaction parameter? To see all model parameter names, run names(coef(model))."))
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
  
  model_coef_names <- names(object$coefficients)
  #model_clustid_names <- names()
  model_fe_names <- object$fixef_vars
  numb_fe <- length(model_fe_names)
  model_param_names <- c(model_coef_names, model_fe_names)
  
  clustid_update <- paste("~ . +",paste(clustid, collapse = " + "))
  fe_update <- paste("~ . +",paste(model_fe_names, collapse = " + "))
  
  fml <- object$fml
  if(is.null(model_fe_names)){
    fml_fe <- fml
    fml_all <- update(fml, clustid_update)
  } else {
    fml_fe <- update(fml, fe_update)
    #model_fe_clustid <- c(model_fe_names, clustid)
    fml_all <- update(fml_fe, clustid_update)
  }

  #depvar <- as.character(formula.tools::lhs(object$fml))
  
  
  
  data <- model.frame(formula = fml_fe, 
                      data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                      drop.unused.levels = TRUE) 
  
  data_all <- model.frame(formula = fml_all, 
                      data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                      drop.unused.levels = TRUE)  
 
  data_diff <- nrow(data) - nrow(data_all)
  
  if(data_diff == 1){
    warning(paste(data_diff, "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."), 
            call. = FALSE)
  } else if(data_diff > 1){
    warning(paste(data_diff, "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."), 
            call. = FALSE)
  }
  
  # now create clusters 
  clustid <- as.data.frame(data_all[, clustid])
  clustid_dims <- ncol(clustid)
  

  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)
  
  if(is.null(clustid_dims) | clustid_dims == 1){
    clustid_dims <- 1
    names(clustid) <- "clustid"
  } else if(clustid_dims == 2){
    names(clustid) <- c("clustid_1", "clustid_2")
    clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
    clustid_dims <- 3
  }
  
  N_G <- sapply(clustid, function(x) length(unique(x)))
  
  # if(clustid_dims == 1){
  #   if(max(N_G) > 200){
  #     warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  #   }
  # } else if(clustid_dims > 1){
  #   if(max(N_G) > 200){
  #     warning(paste("You are estimating a model with more than 200 clusters. The more clusters in the data, the longer the estimation process."))
  #   }
  # }
  
  
  
  # now create fixed effects
  if(!is.null(fe)){
    
    if(numb_fe == 1){
      fml_design <- fml
    } else if(numb_fe > 1){
      fe_update2 <- paste("~ . +",paste(model_fe_names[model_fe_names != fe], collapse = " + "))
      fml_design <- update(fml, fe_update2)
    }
    
    # get rid of constant due to fe out-projection
    model_frame <- model.frame(update(fml_design, . ~ . - 1), data_all)
    X <- model.matrix(model_frame, data = data_all)
    Y <- model.response(model_frame)
    
    N <- nrow(X)
    k <- ncol(X)
    
    
    fixed_effect <- as.data.frame(data_all[, fe])

    # demean X and Y 
    X <- collapse::fwithin(X, fixed_effect[, 1])#
    Y <- collapse::fwithin(Y, fixed_effect[, 1])
    fixed_effect_W <- fixed_effect[, 1]
    levels(fixed_effect_W) <- 1 / table(fixed_effect)
    W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
    n_fe <- length(unique(fixed_effect[, 1]))
  } else{
    model_frame <- model.frame(fml_fe, data_all)
    X <- model.matrix(model_frame, data = data_all)
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
                         W = W, 
                         seed = seed)
  
  if(clustid_dims == 1){
    class(res_preprocess) <- "oneclust"
  } else if(clustid_dims > 1){
    class(res_preprocess) <- "multclust"
  }
  res_preprocess  
  
}
