preprocess2 <- function(object, cluster, fe, param, bootcluster) {
  
  
  #' function that pre-processes regression objects of type lm, fixest and feols
  #'@param object An object of class fixest
  #'@param cluster A vector with the names of the clusters
  #'@param fe A character scalar - fixed effect to be projected out, or NULL
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param bootcluster The bootstrap sampling cluster. 
  #'@importFrom stats formula model.frame coef as.formula model.matrix model.response model.weights
  #'@export
  #'@return List containing preprocessed data for boottest estimation
  
  # ---------------------------------------------------------------------------- # 
  # Step 1: preprocessing of call
  
  check_arg(cluster, "character scalar | character vector")
  check_arg(fe, "character scalar | NULL")
  check_arg(param, "character scalar | NULL")
  check_arg(bootcluster, "character vector | NULL")
  check_arg(param, "character scalar")
  
  
  
  
  if(class(object) == "fixest"){
    of <- object$call

    o <- match(c("fml","data", "weights", "cluster", "fixef"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    # add argument to function
    of$drop.unused.levels <- TRUE 
    
    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    formula <- eval(of$fml)
    
    # combine fixed effects in formula with main formula
    # note: you get a warning here if rhs = 2 is empty (no fixed effect specified via fml)
    formula_coef_fe <- suppressWarnings(formula(Formula::as.Formula(formula), lhs = 1, rhs = c(1, 2), collapse = TRUE))

    formula <- formula_coef_fe
    
    if(!is.null(eval(of$fixef))){
      # add additional fixed effects specified in fixef argument of feols()
      formula_coef_fe <- update(formula_coef_fe, paste("~ . + ", eval(of$fixef)))
      formula <- formula_coef_fe
    }
    # add cluster variables specified in feols-cluster and boottest-cluster arguments
    if(!is.null(eval(of$cluster))){
      formula <- update(formula, paste("~ . +",paste(eval(of$cluster), collapse = "+")))
    }
    if(!is.null(cluster)){
      formula <- update(formula, paste("~ . +",paste(cluster, collapse = "+")))
    }
    
    # add fixed effects to formula - needed for model.matrix
    # note: contains cluster variable if cluster variable are also a covariate of fixed effects
    # further: gets rid of fixed effect specified as fe
  
    # if(!is.null(eval(of$fixef))){
    #   formula_coef_fe <- update(formula_coef_fe, paste("~ . +",paste(eval(of$fixef), collapse = "+")))
    # }
    if(!is.null(fe)){
      formula_coef_fe <- update(formula_coef_fe, paste("~ . -", fe))
    }
    
    
    # if there is at least one fixed effect, get rid of intercept
    # note: length(NULL) == 0
    # if(length(object$fixef_vars) >= 1){
    #   formula_coef_fe <- update(formula_coef_fe, "~. - 1")
    # }

    of$formula <- as.call(formula)
    
    o <- match(c("formula","data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    of[[1L]] <- quote(stats::model.frame)
    # of is a data.frame that contains all variables: depvar, X, fixed effects and clusters specified 
    # in feols and via fe argument
    of <- eval(of, parent.frame())
    
    N_model <- object$nobs
    
  } else if(class(object) == "felm"){
    
    of <- object$call
    o <- match(c("formula","data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    of$drop.unused.levels <- TRUE 
    
    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    if(formula(Formula::as.Formula(eval(of$formula)),lhs = 0,  rhs = 3) != "~0"){
      stop("IV estimation is currently not supported by boottest()")
    }
    
    # formula: model formula plus additional additional cluster variables specified via boottest()
    
    formula <- formula(Formula::as.Formula(eval(of$formula)), lhs = 1, rhs = c(1, 2, 4), collapse = TRUE)
    # add a cluster to formula to get full model.frame
    if(!is.null(cluster)){
      formula <- update(formula, paste("~ . +",paste(cluster, collapse = "+")))
      #formula <- update(formula, paste("~ . -",fe))
    }
    
    # formula_coef_fe: model_formula specified in felm: depvar, covariates + fe
    formula_coef_fe <- formula(Formula::as.Formula(eval(of$formula)), lhs = 1, rhs = c(1, 2), collapse = TRUE)
    
    # of !is.null(fe), delte fe from formula_coef_fe
    if(!is.null(fe)){
      formula_coef_fe <- update(formula_coef_fe, paste("~ . -",fe))
    }
    
    # if there is no other factor in formula_coef_fe but fe specified, delete intercept
    
    # if there is at least one fixed effect, get rid of intercept
    # note: length(NULL) == 0
    # if(length(names(object$fe)) >= 1){
    #   formula_coef_fe <- update(formula_coef_fe, "~. - 1")
    # }
    
    of$formula <- as.call(formula)
    
    o <- match(c("formula","data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    
    of[[1L]] <- quote(stats::model.frame)
    #names(of$fml) <- "formula"
    of <- eval(of, parent.frame())
    
    N_model <- object$N
    
  } else if(class(object) == "lm"){
    
    of <- object$call
    o <- match(c("formula","data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    # add argument to function
    #of$formula <- of$fml
    of$drop.unused.levels <- TRUE 
    
    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    formula_coef_fe <- eval(of$formula)
    
    # add cluster variables to formula
    if(!is.null(cluster)){
      formula <- update(formula_coef_fe, paste("~ . +",paste(cluster, collapse = "+")))
      #formula <- update(formula, paste("~ . -",fe))
    }
    
    of$formula <- as.call(formula)

    o <- match(c("formula","data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    
    of[[1L]] <- quote(stats::model.frame)
    of <- eval(of, parent.frame())
    # print(of)
    
    N_model <- length(residuals(object))
    # fe argument not allowed with boottest.lm
    fe <- NULL
  }
 

  
  # ---------------------------------------------------------------------------- # 
  # From here on: everything the same, independent of model class
  # Step 2: Add warning / error if cluster variables contain NAs
  
  N <- dim(of)[1]
  N_diff <- (N - N_model)
  
  # add a warning if missing values are deleted due to NA values in cluster variables
  na_omit <- TRUE
  if(na_omit == TRUE){
    if(N_diff == 1){
      warning(paste(N_diff, 
                    "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."),
              call. = FALSE)    
    } else if(N_diff > 1){
      warning(paste(N_diff, 
                    "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."),
              call. = FALSE)
    }
  } else if(no_omit == FALSE){
    if(N_diff >= 1){
      stop(paste(N_diff, 
                 "NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest(). If you are fine with deleting missing values, set na_omit = TRUE."),
           call. = FALSE) 
      }
  }
  
  
  # ---------------------------------------------------------------------------- # 
  # Step 3: assign Y, X, weights, fixed_effects, W etc.

  model_frame <- of
  Y <- model.response(model_frame)
  
  # check if there are no factor variables in the covariates and fixed effects after deletion of fe variable
  

  # no_factor <- sum(sapply(model_frame[, !(names(model_frame) %in% c(cluster, names(Y), fe))], is.factor)) == 0
  # if(no_factor == TRUE){
  #   # if there is not a single factor variable in covs and fe's then delete intercept
  #   formula_coef_fe <- update(formula_coef_fe, "~. -1")
  # }
  # 
  # X: need to delete clusters
  X <- model.matrix(formula_coef_fe, model_frame)
  if(!is.null(fe)){
    # note: simply update(..., -1) does not work - intercept is dropped, but all levels of other fe are kept
    X <- X[,-which(colnames(X) == "(Intercept)")]
  }
  k <- dim(X)[2]
  weights <- as.vector(model.weights(of))

  # all null if fe = NULL
  fixed_effect <- NULL
  W <- NULL 
  n_fe <- NULL
  
  if(!is.null(fe)){
    
    fixed_effect <- as.data.frame(model_frame[, fe])
    X <- collapse::fwithin(X, fixed_effect[, 1])
    Y <- collapse::fwithin(Y, fixed_effect[, 1])
    
    fixed_effect_W <- fixed_effect[, 1]
    if(is.null(weights)){
      levels(fixed_effect_W) <- 1 / table(fixed_effect)
    } else if(!is.null(weights)){
      stop("Currently, boottest() does not jointly support regression weights / WLS and fixed effects. If you want to use
           boottest() for inference based on WLS, please set fe = NULL.")
      #levels(fixed_effect_W) <- 1 / table(fixed_effect) 
    }
    W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
    n_fe <- length(unique(fixed_effect[, 1]))
  }
  
  if(is.null(weights)){
    weights <- rep(1, N)
  }
  
  # ---------------------------------------------------------------------------- # 
  # Step 4: preprocess clusters
  
  clustid <- cluster
  cluster_names <- clustid
  clustid <- as.data.frame(model_frame[, clustid], stringsAsFactors = FALSE)
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
    bootcluster <- Reduce(paste0, model_frame[, bootcluster])
  }
  
  
  # --------------------------------------------------------------------------------------- #
  # collect output
  
  R0 <- as.numeric(param == colnames(X))
  
  
  
  res <- list(Y = Y, 
              X = X, 
              weights = weights, 
              fixed_effect = fixed_effect, 
              W = W, 
              n_fe = n_fe, 
              N = N, 
              k = k, 
              clustid = clustid, 
              vcov_sign = vcov_sign, 
              clustid_dims = clustid_dims, 
              N_G = N_G, 
              bootcluster = bootcluster, 
              R0 = R0)
  
  res

  # extract_model_frame.fixest(object, cluster = "group_id2")  
}



