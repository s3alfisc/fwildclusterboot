#' function that pre-processes regression objects of type lm, fixest and feols
#' @param object An object of class lm, fixest or felm
#' @param cluster A vector with the names of the clusters
#' @param fe A character scalar - fixed effect to be projected out, or NULL
#' @param param The univariate coefficients for which a hypothesis is to be tested
#' @param bootcluster The bootstrap sampling cluster.
#' @param na_omit Logical. If TRUE, `boottest()` omits rows with missing variables that are added to the model via the `cluster` argument in `boottest()`
#' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
#' @return List containing preprocessed data for boottest estimation
#' @importFrom dreamerr check_arg
#' @importFrom Formula as.Formula
#' @importFrom collapse fwithin
#' @noRd

preprocess <- function(object, cluster, fe, param, bootcluster, na_omit, R, boot_algo) {

  # ---------------------------------------------------------------------------- #
  # Step 1: preprocessing of call

  check_arg(cluster, "NULL | character scalar | character vector")
  check_arg(fe, "character scalar | NULL")
  check_arg(param, "character vector | character vector | NULL")
  check_arg(bootcluster, "character vector | NULL")
  check_arg(R, "numeric matrix | numeric vector | numeric scalar")
  check_arg(boot_algo, "charin(R, R-lean, WildBootTests.jl)")


  # ---------------------------------------------------------------------------- #
  # From here on: everything the same, independent of model class
  # Step 2: Add warning / error if cluster variables contain NAs

  of <- object$call

  create_formulas <-
    get_formulas(
      object = object,
      cluster = cluster,
      fe = fe,
      param = param,
      bootcluster = bootcluster
    )

  # formula with fixed effects and clusters
  formula <- create_formulas$formula
  # formula with params & fixed effects
  formula_coef_fe <- create_formulas$formula_coef_fe
  N_model <- create_formulas$N_model
  model_param_names <- create_formulas$model_param_names
  fe <- create_formulas$fe
  weights_fml <- create_formulas$weights_fml

  model_frame <- create_formulas$of
  N <- dim(model_frame)[1]

  check_deleted_obs(
    N_model = N_model,
    na_omit = na_omit,
    N = N
  )

  # ---------------------------------------------------------------------------- #
  # Step 3: assign Y, X, weights, fixed_effects, W etc.

  Y <- model.response(model_frame)
  # X: need to delete clusters
  X <- model.matrix(formula_coef_fe, model_frame)

  if (!is.null(fe)) {
    # note: simply update(..., -1) does not work - intercept is dropped, but all levels of other fe are kept
    X <- X[, -which(colnames(X) == "(Intercept)")]
    fixed_effect <- as.data.frame(model_frame[, fe])
  } else {
    fixed_effect <- NULL
  }

  k <- dim(X)[2]
  #fedfadj <- ifelse(!is.null(fixed_effect), length(unique(fixed_effect)), 0)
  #k_all <- k + fedfadj

  # fixed effect demeaning for boot_algo = "R"
  if (boot_algo == "R" & !is.null(fe)) {
    demean_list <-
      demean_fe(
        fixed_effect = fixed_effect,
        model_frame = model_frame,
        X = X,
        Y = Y,
        weights_fml = weights_fml,
        N = N
      )

    W <- demean_list$W
    n_fe <- demean_list$n_fe
    Y <- demean_list$Y
    X <- demean_list$X
  } else {
    # all null if fe = NULL
    W <- NULL
    n_fe <- NULL
  }


  # create weights, is WLS
  if (!is.null(weights_fml)) {
    weights <- as.vector(model.matrix(weights_fml, model_frame))
  } else {
    weights <- rep(1, N)
  }

  k2 <- dim(X)[2]

  # ------------------------------------------------------------------- #
  # Step 4: clean clusters, bootcluster

  if (!is.null(cluster)) {
    # get clusters
    clustid_list <- get_cluster(
      cluster = cluster,
      model_frame = model_frame
    )
    vcov_sign <- clustid_list$vcov_sign
    clustid_dims <- clustid_list$clustid_dims
    clustid <- clustid_list$clustid
    N_G <- clustid_list$N_G
    cluster_names <- clustid_list$cluster_names
    # get bootcluster
    bootcluster <- get_bootcluster(
      bootcluster = bootcluster,
      clustid = clustid,
      N_G = N_G,
      model_param_names = model_param_names,
      cluster_names = cluster_names,
      model_frame = model_frame
    )
  } else {
    clustid <- vcov_sign <- clustid_dims <- N_G <- bootcluster <- NULL
  }


  # extra pre-processing for IVs
  if (inherits(object, "ivreg")) {
    X_exog <- X[, names(object$exogenous)]
    X_endog <- X[, names(object$endogenous)]
    instruments <- X[, names(object$instruments)]
    n_exog <- length(names(object$exogenous))
    n_endog <- length(names(object$endogenous))
    if (!is.matrix(R)) {
      R0 <- rep(0, n_exog + n_endog)
      R0[match(param, c(names(object$exogenous), names(object$endogenous)))] <- R
      # R0[1:n_exog][match(param, colnames(X_exog))] <- R
      # R0[(n_exog +1):(n_exog + n_endog)][match(param, colnames(X_endog))] <- R
      names(R0) <- c(names(object$exogenous), names(object$endogenous))
    } else {
      R0 <- R
    }
  } else {
    instruments <- X_exog <- X_endog <- NULL
    if (!is.matrix(R)) {
      R0 <- rep(0, length(colnames(X)))
      R0[match(param, colnames(X))] <- R
      names(R0) <- colnames(X)
    } else {
      R0 <- R
    }
  }

  # --------------------------------------------------------------------------------------- #
  # collect output


  res <- list(
    Y = Y,
    X = X,
    weights = weights,
    fixed_effect = fixed_effect,
    W = W,
    n_fe = n_fe,
    N = N,
    k = k,
    k2 = k2,
    clustid = clustid,
    vcov_sign = vcov_sign,
    clustid_dims = clustid_dims,
    N_G = N_G,
    bootcluster = bootcluster,
    N_G_bootcluster = length(unique(bootcluster[, 1])),
    R0 = R0,
    model_frame = model_frame,
    X_exog = X_exog,
    X_endog = X_endog,
    instruments = instruments
  )

  if(inherits(object, "ivreg")){
    class(res) <- c("preprocess", "iv")
  } else {
    class(res) <- c("preprocess", "ols")
  }
  res
}



# --------------------------------------------------------------------- #
# other functions

check_deleted_obs <- function(N_model, N, na_omit) {

  #' Function checks if addition of new clustering variable leads to
  #' deletion of rows
  #' @param N_model the number of observations in the original model
  #' @param N number of observations
  #' @param na_omit Logical - if TRUE, deletion of rows is enabled

  N_diff <- abs(N - N_model)

  if (na_omit == FALSE && N_diff != 0) {
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
}



get_cluster <- function(cluster, model_frame) {

  # ---------------------------------------------------------------------------- #
  # Note: a large part of the following code was taken and adapted from the
  # sandwich R package, which is distributed under GPL-2 | GPL-3
  # Zeileis A, Köll S, Graham N (2020). "Various Versatile Variances: An Object-Oriented
  # Implementation of Clustered Covariances in R." _Journal of Statistical Software_,
  # *95*(1), 1-36. doi: 10.18637/jss.v095.i01 (URL: https://doi.org/10.18637/jss.v095.i01).

  # changes by Alexander Fischer:
  # no essential changes, but slight reorganization of pieces of code


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
      clustid <- cbind(clustid, Reduce(paste, clustid[, i]))
      names(clustid)[length(names(clustid))] <- Reduce(paste, names(clustid[, i]))
      # cluster_names <- cbind(cluster_names, Reduce(paste, clustid[,i]))
    }
  }

  N_G <- sapply(clustid, function(x) length(unique(x)))

  res <- list(
    vcov_sign = vcov_sign,
    clustid_dims = clustid_dims,
    clustid = clustid,
    N_G = N_G,
    cluster_names = cluster_names
  )

  res
}


get_bootcluster <- function(bootcluster, clustid, N_G, model_param_names, cluster_names, model_frame) {

  # create a bootcluster vector
  if (length(bootcluster) == 1) {
    if (bootcluster == "max") {
      bootcluster <- clustid[which.max(N_G)]
    } else if (bootcluster == "min") {
      bootcluster <- clustid[which.min(N_G)]
    } else if (length(bootcluster) == 1 && bootcluster %in% c(model_param_names, cluster_names)) {
      # no comma - then bootcluster is a data.frame. this is required later.
      bootcluster <- clustid[which(names(clustid) == bootcluster)]
    }
  } else if (length(bootcluster) > 1) {
    # if a character vector of length > 1 is used to specify the bootcluster
    # same as above: model_frame[bootcluster] -> bootcluster will be a data.frame
    bootcluster <- as.data.frame(Reduce(paste, model_frame[bootcluster]))
  }
}


demean_fe <- function(fixed_effect, model_frame, X, Y, weights_fml, N) {

  # set use.g.names to FALSE?
  g <- collapse::GRP(fixed_effect, call = FALSE)
  X <- collapse::fwithin(X, g)
  Y <- collapse::fwithin(Y, g)

  fixed_effect_W <- fixed_effect[, 1]
  if (is.null(weights_fml)) {
    levels(fixed_effect_W) <- (1 / table(fixed_effect)) # because duplicate levels are forbidden
  } else if (!is.null(weights_fml)) {
    stop("Currently, boottest() does not jointly support regression weights / WLS and fixed effects. If you want to use
            boottest() for inference based on WLS, please set fe = NULL.")
    # levels(fixed_effect_W) <- 1 / table(fixed_effect)
  }

  W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
  n_fe <- length(unique(fixed_effect[, 1]))

  res <- list(
    W = W,
    n_fe = n_fe,
    X = X,
    Y = Y,
    fixed_effect = fixed_effect
  )
}


get_formulas <- function(object, cluster, fe, param, bootcluster) {
  of <- object$call

  if (inherits(object, "fixest")) {
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

    # formula: formula_coef_fe + additional cluster variables and weights. Only
    # used to construct model.frame
    formula <- formula_coef_fe

    if (!is.null(eval(of$fixef))) {
      # add additional fixed effects specified in fixef argument of feols()
      formula_coef_fe <- update(formula_coef_fe, paste("~ . +", paste(eval(of$fixef), collapse = " + ")))
      formula <- formula_coef_fe
    }

    # add cluster variables specified in feols-cluster and boottest-cluster arguments
    if (!is.null(eval(of$cluster))) {
      if (inherits(eval(of$cluster), "formula")) {
        add_cluster <- unlist(strsplit(deparse(of$cluster), "[~]"))[2]
        formula <- update(formula, paste("~ . +", paste(add_cluster, collapse = "+")))
      } else if (inherits(eval(of$cluster),"character")) {
        formula <- update(formula, paste("~ . +", paste(eval(of$cluster), collapse = "+")))
      } else {
        deparse_data <- unlist(strsplit(deparse(of$data), "[$]"))
        deparse_cluster <- unlist(strsplit(deparse(of$cluster), "[$]"))
        add_cluster <- deparse_cluster[-(which(deparse_cluster %in% deparse_data))]
        formula <- update(formula, paste("~ . +", paste(add_cluster, collapse = "+")))
      }
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

    # add weights
    weights_formulas <- add_weights(of = of, formula = formula)
    formula <- weights_formulas$formula
    weights_fml <- weights_formulas$weights_fml

    # if there is at least one fixed effect, get rid of intercept
    # note: length(NULL) == 0
    # if(length(object$fixef_vars) >= 1){
    #   formula_coef_fe <- update(formula_coef_fe, "~. - 1")
    # }

    of$formula <- as.call(formula)

    o <- match(c("formula", "data"), names(of), 0L)
    of <- of[c(1L, o)]
    of[[1L]] <- quote(stats::model.frame)
    # of is a data.frame that contains all variables: depvar, X, fixed effects and clusters specified
    # in feols and via fe argument

    of <- eval(of, parent.frame())

    # check if one of the fixed effects or cluster variables is not of type character or factor
    fixedid <- object$fixef_vars
    of <- check_set_fixef_types(of = of, fixedid = fixedid)


    N_model <- object$nobs
    model_param_names <- c(names(coef(object)), object$fixef)
  } else if (inherits(object, "felm")) {
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
    # drop any potential 0s in formula - no effect if no zero in formula
    formula <- update(formula, "~ . - 0")

    # add a cluster to formula to get full model.frame
    if (!is.null(cluster)) {
      formula <- update(formula, paste("~ . +", paste(cluster, collapse = "+")))
      # formula <- update(formula, paste("~ . -",fe))
    }

    # formula_coef_fe: model_formula specified in felm: depvar, covariates + fe
    formula_coef_fe <- suppressWarnings(formula(Formula::as.Formula(eval(of$formula)), lhs = 1, rhs = c(1, 2), collapse = TRUE))
    formula_coef_fe <- update(formula_coef_fe, "~ . - 0")
    # of !is.null(fe), delte fe from formula_coef_fe
    if (!is.null(fe)) {
      formula_coef_fe <- update(formula_coef_fe, paste("~ . -", fe))
    }

    # add weights
    weights_formulas <- add_weights(of = of, formula = formula)
    formula <- weights_formulas$formula
    weights_fml <- weights_formulas$weights_fml

    of$formula <- as.call(formula)

    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]

    of[[1L]] <- quote(stats::model.frame)
    # names(of$fml) <- "formula"
    of <- eval(of, parent.frame())

    # check if one of the fixed effects or cluster variables is not of
    # type character or factor
    fixedid <- names(object$fe)
    of <- check_set_fixef_types(of = of, fixedid = fixedid)

    N_model <- object$N
    model_param_names <- rownames(coef(object))
  } else if (inherits(object, "lm")) {
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    # add argument to function
    # of$formula <- of$fml
    of$drop.unused.levels <- TRUE

    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    formula_coef_fe <- eval(of$formula)

    formula <- formula_coef_fe
    # add cluster variables to formula
    if (!is.null(cluster)) {
      formula <- add_boottest_cluster(formula_coef_fe = formula_coef_fe, cluster = cluster)
      formula <- add_bootcluster(formula = formula, bootcluster = bootcluster)
    }


    # add weights
    weights_formulas <- add_weights(of = of, formula = formula)
    formula <- weights_formulas$formula
    weights_fml <- weights_formulas$weights_fml


    of$formula <- as.call(formula)

    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]

    of[[1L]] <- quote(stats::model.frame)
    of <- eval(of, parent.frame())

    N_model <- length(residuals(object))
    model_param_names <- names(coef(object))

    # fe argument not allowed with boottest.lm
    fe <- NULL
  } else if (inherits(object, "ivreg")) {

    # create formula objects by adding fixed effects and clusters from call
    # add potential other cluster variables from cluster argument
    formula <- eval(of$formula)

    o <- match(c("formula", "data", "weights"), names(of), 0L)
    # keep only required arguments
    of <- of[c(1L, o)]
    # add argument to function
    of$drop.unused.levels <- TRUE

    # formula with only depvar and covariates, needed to construct design matrix X
    formula <- formula(Formula::Formula(formula), collapse = TRUE)
    formula_coef_fe <- formula

    fml <- Formula::as.Formula(of$formula)
    fml_linear <- formula(fml, lhs = 1, rhs = 1)
    fml_iv <- formula(fml, lhs = 0, rhs = 2)
    fml_linear_cluster <- update(fml_linear, paste("~ . +", paste(cluster, collapse = "+")))
    fml_cluster <- Formula::as.Formula(fml_linear_cluster, fml_iv)
    fml_iv_cluster <- formula(fml_cluster, collapse = TRUE, update = TRUE)

    formula <- formula(Formula::as.Formula(fml_linear, fml_iv), collapse = TRUE, update = TRUE)

    formula <- add_bootcluster(formula = formula, bootcluster = bootcluster)
    # add weights
    weights_formulas <- add_weights(of = of, formula = formula)
    formula <- weights_formulas$formula
    weights_fml <- weights_formulas$weights_fml

    of$formula <- as.call(fml_iv_cluster)
    o <- match(c("formula", "data", "weights"), names(of), 0L)
    of <- of[c(1L, o)]
    of[[1L]] <- quote(stats::model.frame)
    of <- eval(of, parent.frame())

    N_model <- object$nobs
    model_param_names <- names(coef(object))

    fe <- NULL
  }





  res <- list(
    fe = fe,
    model_param_names = model_param_names,
    formula = formula,
    formula_coef_fe = formula_coef_fe,
    N_model = N_model,
    of = of,
    weights_fml = weights_fml
  )

  res
}

add_boottest_cluster <- function(cluster, formula_coef_fe) {
  if (!is.null(cluster)) {
    formula <- update(formula_coef_fe, paste("~ . +", paste(cluster, collapse = "+")))
  }
  formula
}

add_bootcluster <- function(formula, bootcluster) {
  if (sum(bootcluster %in% c(NULL, "max", "min")) == 0) {
    formula <- update(formula, paste("~ . +", paste(bootcluster, collapse = "+")))
  }
  formula
}

add_weights <- function(of, formula) {
  # add weights
  if (!is.null(eval(of$weights))) {
    if (inherits(eval(of$weights), "formula")) {
      add_weights <- unlist(strsplit(deparse(of$weights), "[~]"))[2]
      formula <- update(formula, paste("~ . +", paste(add_weights, collapse = "+")))
      weights_fml <- formula(paste("~ -1 + ", paste(add_weights, collapse = "+")))
    } else {
      deparse_data <- unlist(strsplit(deparse(of$data), "[$]"))
      deparse_weights <- unlist(strsplit(deparse(of$weights), "[$]"))
      add_weights <- deparse_weights[-(which(deparse_weights %in% deparse_data))]
      if (length(add_weights) == 0) {
        stop(paste0("Currently, boottest() accepts regression weights only if they are specified as a formula as weights = ", paste0("~", deparse_weights), " or in reference to the input data.frame as as weights = ", paste0(deparse_data, "$", deparse_weights), "."))
      }
      formula <- update(formula, paste("~ . +", paste(add_weights, collapse = "+")))
      weights_fml <- formula(paste("~ -1 + ", paste(add_weights, collapse = "+")))
    }
  } else {
    weights_fml <- NULL
  }
  res <- list(
    formula = formula,
    weights_fml = weights_fml
  )
  res
}

check_set_fixef_types <- function(of, fixedid) {
  j <- which(!(sapply(data.frame(of[, c(fixedid)]), class) %in% c("character", "factor")))
  # if only one fixed effect & if it is not a factor, `of[,c(fixedid)]` is not a data.frame but a vector
  if (length(j) == 1) {
    of[, c(fixedid)] <- factor(of[, c(fixedid)])
  } else if (length(j) > 1) {
    of[, c(fixedid)][, j] <- lapply(j, function(x) factor(of[, c(fixedid)][, x]))
  }
  of
}
# get_iv <- function(){
#     X_exog <- X[, names(object$exogenous)]
#     X_endog <- X[, names(object$endogenous)]
#     instruments <- X[, names(object$instruments)]
#     n_exog <- length(names(object$exogenous))
#     n_endog <- length(names(object$endogenous))
#     res <- list(X_exog = X_exog,
#                 X_endog = X_endog,
#                 instruments = instruments,
#                 n_exog,
#                 n_endog)
#     res
# }
#
# clean_R <- function(){
#
# }
#
#     n_exog <- length(names(object$exogenous))
#     n_endog <- length(names(object$endogenous))
#     if(!is.matrix(R)){
#       R0 <- rep(0, n_exog + n_endog)
#       R0[match(param,c(names(object$exogenous), names(object$endogenous)))] <- R
#       #R0[1:n_exog][match(param, colnames(X_exog))] <- R
#       #R0[(n_exog +1):(n_exog + n_endog)][match(param, colnames(X_endog))] <- R
#       names(R0) <- c(names(object$exogenous), names(object$endogenous))
#     } else {
#       R0 <- R
#     }
#   } else {
#     instruments <- X_exog <- X_endog <- NULL
#     if(!is.matrix(R)){
#       R0 <- rep(0, length(colnames(X)))
#       R0[match(param, colnames(X))] <- R
#       names(R0) <- colnames(X)
#     } else {
#       R0 <- R
#     }
#   }
#
# }
#
#
# get_R <- function(){
#
# }

# paste_ <- function(...) paste(..., sep = "_")
