#' function that pre-processes regression objects of type lm, fixest and feols
#' @param object An object of class lm, fixest or felm
#' @param cluster A vector with the names of the clusters
#' @param fe A character scalar - fixed effect to be projected out, or NULL
#' @param param The univariate coefficients for which a hypothesis is to be tested
#' @param bootcluster The bootstrap sampling cluster.
#' @param na_omit Logical. If TRUE, `boottest()` omits rows with missing variables that are added to the model via the `cluster` argument in `boottest()`
#' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
#' @param fweights Should the regression weights be treated as frequency or probability weights?
#' @return List containing preprocessed data for boottest estimation
#' @importFrom dreamerr check_arg
#' @importFrom Formula as.Formula
#' @importFrom collapse fwithin
#' @noRd

preprocess2 <- function(object, cluster, fe, param, bootcluster, na_omit, R, fweights) {
  
  # ---------------------------------------------------------------------------- #
  # Step 1: preprocessing of call
  
  check_arg(cluster, "character scalar | character vector")
  check_arg(fe, "character scalar | NULL")
  check_arg(param, "character vector | character vector | NULL")
  check_arg(bootcluster, "character vector | NULL")
  check_arg(R, "numeric vector | numeric scalar")
  check_arg(fweights, "logical scalar")
  
  if (class(object) == "fixest") {
    of <- object$call
    
    o <- match(c("fml", "data", "weights", "cluster", "fixef"), names(of), 0L)
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
    
    if (!is.null(eval(of$fixef))) {
      # add additional fixed effects specified in fixef argument of feols()
      formula_coef_fe <- update(formula_coef_fe, paste("~ . +", paste(eval(of$fixef), collapse = " + ")))
      formula <- formula_coef_fe
    }
    # add cluster variables specified in feols-cluster and boottest-cluster arguments
    if (!is.null(eval(of$cluster))) {
      formula <- update(formula, paste("~ . +", paste(eval(of$cluster), collapse = "+")))
    }
    if (!is.null(cluster)) {
      formula <- update(formula, paste("~ . +", paste(cluster, collapse = "+")))
    }
    
    # add fixed effects to formula - needed for model.matrix
    # note: contains cluster variable if cluster variable are also a covariate of fixed effects
    # further: gets rid of fixed effect specified as fe
    
    # if(!is.null(eval(of$fixef))){
    #   formula_coef_fe <- update(formula_coef_fe, paste("~ . +",paste(eval(of$fixef), collapse = "+")))
    # }
    if (!is.null(fe)) {
      formula_coef_fe <- update(formula_coef_fe, paste("~ . -", fe))
    }
    
    
    # if there is at least one fixed effect, get rid of intercept
    # note: length(NULL) == 0
    # if(length(object$fixef_vars) >= 1){
    #   formula_coef_fe <- update(formula_coef_fe, "~. - 1")
    # }
    
    of$formula <- as.call(formula)
    
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    of[[1L]] <- quote(stats::model.frame)
    # of is a data.frame that contains all variables: depvar, X, fixed effects and clusters specified
    # in feols and via fe argument
    
    of <- eval(of, parent.frame())
    
    # check if one of the fixed effects or cluster variables is not of 
    # type character or factor
    #sapply(of, class)
    fixedid <- object$fixef_vars
    # are fixed effects neither character nor factor?
    j <- which(!(sapply(data.frame(of[,c(fixedid)]), class) %in%  c("factor")))
    # j <- (sapply(data.frame(of[,c(fixedid)]), class) %in%  c("character", "factor"))
    
    # if only one fixed effect & if it is not a factor, `of[,c(fixedid)]` is not a data.frame but a vector
    if(length(j) == 1){
      of[, c(fixedid)] <- factor(of[, c(fixedid)])
    } else if(length(j) > 1) {
      of[, c(fixedid)][,j] <- lapply(j, function(x) factor(of[, c(fixedid)][,x]))
    }
    #sapply(of, class)
    
    # are all integer/ numeric variables now characters? 
    j <- !(sapply(data.frame(of[,c(fixedid)]), class) %in%  c("factor"))
    
    # if at least one j evaluates to "TRUE" 
    # not covered by test coverage - case should never occur
    if(sum(j) > 0){
      stop(paste("The fixed effects variable(s)", paste(fixedid[j], collapse = " & "), "is/are not factor variables. This should have been fixed internally but apparently wasn't. Please report the bug!"))
    }
    N_model <- object$nobs
    model_param_names <- c(names(coef(object)), object$fixef)
  } else if (class(object) == "felm") {
    
    of <- object$call
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    of$drop.unused.levels <- TRUE
    
    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    if (suppressWarnings(formula(Formula::as.Formula(eval(of$formula)), lhs = 0, rhs = 3)) != "~0") {
      stop("IV estimation is currently not supported by boottest()")
    }
    
    # formula: model formula plus additional additional cluster variables specified via boottest()
    
    formula <- suppressWarnings(formula(Formula::as.Formula(eval(of$formula)), lhs = 1, rhs = c(1, 2, 4), collapse = TRUE))
    # add a cluster to formula to get full model.frame
    if (!is.null(cluster)) {
      formula <- update(formula, paste("~ . +", paste(cluster, collapse = "+")))
      # formula <- update(formula, paste("~ . -",fe))
    }
    
    # formula_coef_fe: model_formula specified in felm: depvar, covariates + fe
    formula_coef_fe <- suppressWarnings(formula(Formula::as.Formula(eval(of$formula)), lhs = 1, rhs = c(1, 2), collapse = TRUE))
    
    # of !is.null(fe), delte fe from formula_coef_fe
    if (!is.null(fe)) {
      formula_coef_fe <- update(formula_coef_fe, paste("~ . -", fe))
    }
    
    of$formula <- as.call(formula)
    
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    
    of[[1L]] <- quote(stats::model.frame)
    # names(of$fml) <- "formula"
    of <- eval(of, parent.frame())
    
    # check if one of the fixed effects or cluster variables is not of 
    # type character or factor
    fixedid <- names(object$fe)
    # are fixed effects neither character nor factor?
    j <- which(!(sapply(data.frame(of[,c(fixedid)]), class) %in%  c("character", "factor")))
    # if only one fixed effect & if it is not a factor, `of[,c(fixedid)]` is not a data.frame but a vector
    if(length(j) == 1){
      of[, c(fixedid)] <- factor(of[, c(fixedid)])
    } else if(length(j) > 1) {
      of[, c(fixedid)][,j] <- lapply(j, function(x) factor(of[, c(fixedid)][,x]))
    }    
    N_model <- object$N
    model_param_names <- rownames(coef(object))
  } else if (class(object) == "lm") {
    of <- object$call
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    # add argument to function
    # of$formula <- of$fml
    of$drop.unused.levels <- TRUE
    
    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    formula_coef_fe <- eval(of$formula)
    
    # add cluster variables to formula
    if (!is.null(cluster)) {
      formula <- update(formula_coef_fe, paste("~ . +", paste(cluster, collapse = "+")))
      # formula <- update(formula, paste("~ . -",fe))
    }
    
    of$formula <- as.call(formula)
    
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    
    of[[1L]] <- quote(stats::model.frame)
    of <- eval(of, parent.frame())
    
    N_model <- length(residuals(object))
    model_param_names <- names(coef(object))
    
    # fe argument not allowed with boottest.lm
    fe <- NULL
  }
  
  
  
  # ---------------------------------------------------------------------------- #
  # From here on: everything the same, independent of model class
  # Step 2: Add warning / error if cluster variables contain NAs
  
  N <- dim(of)[1]
  N_diff <- abs(N - N_model)
  
  if(na_omit == FALSE && N_diff != 0){
    stop("One or more cluster variables set in boottest() contain 
         NA values. This is not allowed if na_omit == FALSE.
         Please either delete the missing values from your model prior
         to estimating the regression model and conducting inference with 
         boottest(), or set the boottest() argument na_omit = TRUE. Note that in
         the second case, parameter estimation and 
         bootstrap inference will be conducted on two different samples.")
  }
  # add a warning if missing values are deleted due to NA values in cluster variables
  
  if (na_omit == TRUE) {
    if (N_diff == 1) {
      warning(paste(
        N_diff,
        "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."
      ),
      call. = FALSE, 
      noBreaks. = TRUE
      )
    } else if (N_diff > 1) {
      warning(paste(
        N_diff,
        "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."
      ),
      call. = FALSE, 
      noBreaks. = TRUE
      )
    }
    # this part of the code is superfluous, right?  
  } else if (na_omit == FALSE) {
    if (N_diff >= 1) {
      stop(paste(
        N_diff,
        "NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest(). If you are fine with deleting missing values, set na_omit = TRUE."
      ),
      call. = FALSE, 
      noBreaks. = TRUE
      )
    }
  }
  
  
  # ---------------------------------------------------------------------------- #
  # Step 3: assign Y, X, weights, fixed_effects, W etc.
  
  model_frame <- of
  
  Y <- model.response(model_frame)
  # X: need to delete clusters
  X <- model.matrix(formula_coef_fe, model_frame)
  
  if (!is.null(fe)) {
    # note: simply update(..., -1) does not work - intercept is dropped, but all levels of other fe are kept
    X <- X[, -which(colnames(X) == "(Intercept)")]
  }
  
  k <- dim(X)[2]
  weights <- as.vector(model.weights(of))
  
  # all null if fe = NULL
  fixed_effect <- NULL
  W <- NULL
  n_fe <- NULL
  
  if (!is.null(fe)) {
    
    fixed_effect <- as.data.frame(model_frame[, fe])
    # set use.g.names to FALSE?
    g <- collapse::GRP(fixed_effect, call = FALSE)
    X <- collapse::fwithin(X, g)
    Y <- collapse::fwithin(Y, g)
    
    fixed_effect_W <- fixed_effect[, 1]
    if (is.null(weights)) {
      levels(fixed_effect_W) <- (1 / table(fixed_effect)) # because duplicate levels are forbidden
    } else if (!is.null(weights)) {
      stop("Currently, boottest() does not jointly support regression weights / WLS and fixed effects. If you want to use
            boottest() for inference based on WLS, please set fe = NULL.")
      # levels(fixed_effect_W) <- 1 / table(fixed_effect)
    }
    
    W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
    n_fe <- length(unique(fixed_effect[, 1]))
  }
  
  if (is.null(weights)) {
    weights <- rep(1, N)
  }
  
  if(fweights == TRUE){
    # this does not work if weights is not a matrix of integers
    if(!is.integer(weights)){
      stop(paste("When fweights == TRUE, the weights column needs to be of type 'integer', but it is", class(weights), "."))
    }
    N <- sum(weights)
  }
  
  # ---------------------------------------------------------------------------- #
  # Step 4: preprocess clusters
  # Note: a large part of the following code was taken and adapted from the 
  # sandwich R package, which is distributed under GPL-2 | GPL-3
  # Zeileis A, Köll S, Graham N (2020). "Various Versatile Variances: An Object-Oriented
  # Implementation of Clustered Covariances in R." _Journal of Statistical Software_,
  # *95*(1), 1-36. doi: 10.18637/jss.v095.i01 (URL: https://doi.org/10.18637/jss.v095.i01).
  
  # changes by Alexander Fischer: 
  # no essential changes, but slight reorganization of pieces of code
  
  # ---------------------------------------------------------------------------- #
  # Start Sandwich code 
  # ---------------------------------------------------------------------------- #
  
  clustid <- cluster
  cluster_names <- clustid
  clustid <- as.data.frame(model_frame[, clustid], stringsAsFactors = FALSE)
  clustid_dims <- ncol(clustid)
  
  
  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)
  
  # taken from multiwayvcov::cluster.boot
  acc <- list()
  for (i in 1:clustid_dims) {
    acc <- append(acc, utils::combn(1:clustid_dims, i, simplify = FALSE))
  }
  
  vcov_sign <- sapply(acc, function(i) (-1)^(length(i) + 1))
  acc <- acc[-1:-clustid_dims]
  
  if (clustid_dims == 1) {
    names(clustid) <- cluster_names
  }
  
  if (clustid_dims > 1) {
    for (i in acc) {
      clustid <- cbind(clustid, Reduce(paste0, clustid[, i]))
      names(clustid)[length(names(clustid))] <- Reduce(paste0, names(clustid[, i]))
      # cluster_names <- cbind(cluster_names, Reduce(paste0, clustid[,i]))
    }
  }
  
  # ---------------------------------------------------------------------------- #
  # End Sandwich code 
  # ---------------------------------------------------------------------------- #
  
  
  N_G <- sapply(clustid, function(x) length(unique(x)))
  
  # create a bootcluster vector
  if (length(bootcluster) == 1) {
    if (bootcluster == "max") {
      bootcluster <- as.matrix(clustid[which.max(N_G)])
    } else if (bootcluster == "min") {
      bootcluster <- as.matrix(clustid[which.min(N_G)])
    } else if (length(bootcluster) == 1 && bootcluster %in% c(model_param_names, cluster_names)) {
      # no comma - then bootcluster is a data.frame. this is required later.
      bootcluster <- as.matrix(clustid[which(names(clustid) == bootcluster)])
    }
  } else if (length(bootcluster) > 1) {
    # if a character vector of length > 1 is used to specify the bootcluster
    # same as above: model_frame[bootcluster] -> bootcluster will be a data.frame
    bootcluster <- as.matrix(Reduce(paste0, model_frame[bootcluster]))
  }
  
  
  
  # --------------------------------------------------------------------------------------- #
  # collect output
  
  R0 <- rep(0, length(colnames(X)))
  R0[match(param, colnames(X))] <- R
  names(R0) <- colnames(X)
  
  res <- list(
    Y = Y,
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
    R0 = R0
  )
  
  res
  
}