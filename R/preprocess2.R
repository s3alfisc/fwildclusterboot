#' `preprocess2` is a S3 method that fetches data from several model
#' objectects for use with `boottest()`.
#'
#' @param object An objectect of type lm, fixest, felm or ivreg
#' @param ... other arguments
#' @noRd
#'
#' @return An object of class \code{preprocess2}.

preprocess2 <- function(object, ...) {
  UseMethod("preprocess2")
}



preprocess2.fixest <-
  function(object,
           clustid,
           R,
           param,
           fe,
           engine,
           bootcluster) {
    #' preprocess data for objects of type fixest
    #'
    #' @param object an object of type fixest
    #' @param clustid a character string containing the name(s) of the
    #'  cluster variables
    #' @param R Hypothesis Vector giving linear combinations of coefficients.
    #' @param fe character vector. name of the fixed effect to be projected
    #'  out in the bootstrap
    #' @param param character vector. names of the parameter(s) to test
    #' @param engine The bootstrap algorithm to run. Either "R" or
    #'  "WildBootTests.jl"
    #' @param bootcluster a character string containing the name(s) of the
    #'  bootcluster variables. Alternatively, "min" or "max"
    #'
    #' @noRd
    #'
    #' @method preprocess2 fixest

    call <- object$call
    call_env <- object$call_env
    fml <- formula(object)
    fml <- Formula::as.Formula(fml)

    fml_full <- formula(fml, collapse = TRUE)

    N <- nobs(object)
    # lm and felm don't drop NAs due to multicollinearity, while fixest does
    k <- length(na.omit(coef(object)))

    method <- object$family
    # fixest specific checks
    if (object$method != "feols") {
      stop(
        "boottest() only supports OLS estimation via fixest::feols() - it
        does not support non-linear models computed via e.g. fixest::fepois()
        or fixest::feglm."
      )
    }
    
    # if (!is.null(object$is_sunab)) {
    #   if(object$is_sunab == TRUE){
    #     stop(
    #       "boottest() does not support the Sun-Abrams
    #       estimator via `sunab()`."
    #     )
    #   }
    # }

    is_iv <- ifelse(!is.null(object$fml_all$iv), TRUE, FALSE)
    has_fe <- ifelse(!is.null(object$fml_all$fixef), TRUE, FALSE)

    if (!is_iv) {
      X <-
        model.matrix(object,
          type = "rhs",
          na.rm = TRUE,
          collin.rm = TRUE
        )
    } else {
      X_endog <-
        model.matrix(object,
          type = "iv.endo",
          na.rm = TRUE,
          collin.rm = TRUE
        )
      X_exog <-
        model.matrix(object,
          type = "iv.exo",
          na.rm = TRUE,
          collin.rm = TRUE
        )
      instruments <-
        model.matrix(object,
          type = "iv.inst",
          na.rm = TRUE,
          collin.rm = TRUE
        )
    }

    Y <- model.matrix(object, type = "lhs")

    weights <- weights(object)
    if (is.null(weights)) {
      has_weights <- FALSE
      weights <- rep(1, N)
    } else {
      has_weights <- TRUE
    }

    if (has_weights) {
      if (!is.null(fe)) {
        stop(
          "boottest() unfortunately currently does not support WLS and fixed
          effects. Please set fe = NULL to run a bootstrap with WLS."
        )
      }
    }

    fixed_effect <- NULL
    k2 <- 0
    W <- n_fe <- NULL


    if (has_fe) {
      # if(!is.null(fe)){
      get_fe <- transform_fe(
        object = object,
        X = X,
        Y = Y,
        fe = fe,
        N = N,
        has_weights = has_weights,
        engine = engine
      )


      X <- get_fe$X
      Y <- get_fe$Y
      fixed_effect <- get_fe$fixed_effect
      W <- get_fe$W
      n_fe <- get_fe$n_fe
      k2 <- get_fe$k2
      # }
    }


    # get cluster variable
    if (!is.null(clustid)) {
      clustid_list <- get_cluster(
        object = object,
        clustid_char = clustid,
        N = N,
        bootcluster = bootcluster,
        call_env = call_env
      )

      vcov_sign <- clustid_list$vcov_sign
      clustid <- clustid_list$clustid
      clustid_dims <- ncol(clustid)
      N_G <- clustid_list$N_G
      cluster_names <- clustid_list$cluster_names

      cluster_bootcluster <- clustid_list$cluster_bootcluster
      bootcluster <- clustid_list$bootcluster
      all_c <- clustid_list$all_c
    } else {
      vcov_sign <-
        clustid_dims <-
        clustid <- bootcluster <- N_G <- cluster_names <- NULL
      cluster_bootcluster <- bootcluster <- all_c <- NULL
    }


    # iv prep
    instruments <- X_exog <- X_endog <- NULL
    # if(is_iv){
    #  R0 <- rep(0, n_exog + n_endog)
    #  R0[
    # match(param, c(names(object$exogenous), names(object$endogenous)))] <- R
    #  names(R0) <- c(names(object$exogenous), names(object$endogenous))
    # } else {
    if (!is.matrix(R)) {
      R0 <- rep(0, length(colnames(X)))
      R0[match(param, colnames(X))] <- R
      names(R0) <- colnames(X)
    } else {
      q <- nrow(R)
      p <- ncol(R)
      R0 <- matrix(0, q, ncol(X))
      R0[, 1:p] <- R
    }
    # }



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
      cluster_bootcluster = cluster_bootcluster,
      bootcluster = bootcluster,
      N_G_bootcluster = length(unique(bootcluster[[1]])),
      R0 = R0,
      # model_frame = model_frame,
      X_exog = X_exog,
      X_endog = X_endog,
      instruments = instruments,
      has_fe = has_fe,
      all_c = all_c
    )

    if (is_iv) {
      class(res) <- c("preprocess", "iv")
    } else {
      class(res) <- c("preprocess", "ols")
    }

    res
  }


preprocess2.felm <-
  function(object,
           clustid,
           R,
           param,
           fe,
           engine,
           bootcluster) {
    #' preprocess data for objects of type felm
    #'
    #' @param object an object of type felm
    #' @param clustid a character string containing the name(s) of the
    #' cluster variables
    #' @param R Hypothesis Vector giving linear combinations of coefficients.
    #' @param fe character vector. name of the fixed effect to be projected
    #'  out in the bootstrap
    #' @param param character vector. names of the parameter(s) to test
    #' @param engine The bootstrap algorithm to run. Either "R" or
    #'  "WildBootTests.jl"
    #' @param bootcluster a character string containing the name(s) of the
    #'  bootcluster variables. Alternatively, "min" or "max"
    #'
    #' @noRd
    #'
    #' @method preprocess2 felm

    call <- object$call
    call_env <- environment(formula(object))
    fml <- formula(object)
    fml <- Formula::as.Formula(fml)

    N <- nobs(object)
    # lm and felm don't drop NAs due to multicollinearity, while fixest does
    k <- length(na.omit(coef(object)))
    p <- object$p

    is_iv <- FALSE
    if (
      suppressWarnings(
        formula(
          Formula::as.Formula(
            eval(
              object$formula
            )
          ),
          lhs = 0,
          rhs = 3
        )
      ) != "~0") {
      stop(
        "IV regression is currently not supported by boottest() for
        objects of type 'felm'. You can either use 'fixest::feols()'
        or 'ivreg::ivreg' for IV-regression."
      )
      is_iv <- TRUE
    }

    X <- model_matrix(object, type = "rhs", collin.rm = TRUE)
    Y <- model.response(model.frame(object))
    has_fe <- ifelse(length(names(object$fe)) > 0, TRUE, FALSE)

    weights <- weights(object)
    if (is.null(weights)) {
      has_weights <- FALSE
      weights <- rep(1, N)
    } else {
      has_weights <- TRUE
    }

    if (has_weights) {
      if (!is.null(fe)) {
        stop(
          "boottest() unfortunately currently does not support WLS and
          fixed effects. Please set fe = NULL to run a bootstrap with WLS."
        )
      }
    }

    if (has_fe) {
      get_fe <- transform_fe(
        object = object,
        X = X,
        Y = Y,
        fe = fe,
        N = N,
        has_weights = has_weights,
        engine = engine
      )
      X <- get_fe$X
      Y <- get_fe$Y
      fixed_effect <- get_fe$fixed_effect
      W <- get_fe$W
      n_fe <- get_fe$n_fe
      k2 <- get_fe$k2
    } else {
      fixed_effect <- NULL
      k2 <- 0
      W <- n_fe <- NULL
      W <- n_fe <- NULL
    }

    # get cluster variable
    if (!is.null(clustid)) {
      clustid_list <- get_cluster(
        object = object,
        clustid_char = clustid,
        N = N,
        bootcluster = bootcluster,
        call_env = call_env
      )

      vcov_sign <- clustid_list$vcov_sign
      clustid <- clustid_list$clustid
      clustid_dims <- ncol(clustid)
      N_G <- clustid_list$N_G
      cluster_names <- clustid_list$cluster_names

      cluster_bootcluster <- clustid_list$cluster_bootcluster
      bootcluster <- clustid_list$bootcluster
      all_c <- clustid_list$all_c
    } else {
      vcov_sign <-
        clustid_dims <-
        clustid <- bootcluster <- N_G <- cluster_names <- NULL
      cluster_bootcluster <- bootcluster <- all_c <- NULL
    }

    # iv prep
    instruments <- X_exog <- X_endog <- NULL
    # if(is_iv){
    #  R0 <- rep(0, n_exog + n_endog)
    #  R0[
    # match(param, c(names(object$exogenous), names(object$endogenous)))] <- R
    #  names(R0) <- c(names(object$exogenous), names(object$endogenous))
    # } else {
    if (!is.matrix(R)) {
      R0 <- rep(0, length(colnames(X)))
      R0[match(param, colnames(X))] <- R
      names(R0) <- colnames(X)
    } else {
      # need matrix input for mboottest
      q <- nrow(R)
      p <- ncol(R)
      R0 <- matrix(0, q, ncol(X))
      R0[, 1:p] <- R
    }
    # }

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
      cluster_bootcluster = cluster_bootcluster,
      bootcluster = bootcluster,
      N_G_bootcluster = length(unique(bootcluster[[1]])),
      R0 = R0,
      X_exog = NULL,
      X_endog = NULL,
      instruments = NULL,
      has_fe = has_fe,
      all_c = all_c
    )

    if (is_iv) {
      class(res) <- c("preprocess", "iv")
    } else {
      class(res) <- c("preprocess", "ols")
    }

    res
  }


preprocess2.lm <-
  function(object,
           clustid,
           R,
           param,
           engine,
           bootcluster) {
    #' preprocess data for objects of type lm
    #'
    #' @param object an object of type lm
    #' @param clustid a character string containing the name(s) of the
    #'  cluster variables
    #' @param R Hypothesis Vector giving linear combinations of coefficients.
    #' @param param character vector. names of the parameter(s) to test
    #' @param engine The bootstrap algorithm to run. Either "R" or
    #' "WildBootTests.jl"
    #' @param bootcluster a character string containing the name(s) of
    #' the bootcluster variables. Alternatively, "min" or "max"
    #'
    #' @noRd
    #'
    #' @method preprocess2 lm

    call <- object$call
    call_env <- environment(formula(object))
    fml <- formula(object)
    fml <- Formula::as.Formula(fml)

    N <- nobs(object)
    # lm and felm don't drop NAs due to multicollinearity, while fixest does
    k <- length(na.omit(coef(object)))
    p <- object$p

    is_iv <- FALSE

    X <- model_matrix(object, collin.rm = TRUE)
    Y <- model.response(model.frame(object))
    has_fe <- FALSE

    weights <- weights(object)
    if (is.null(weights)) {
      has_weights <- FALSE
      weights <- rep(1, N)
    } else {
      has_weights <- TRUE
    }

    # get cluster variable
    if (!is.null(clustid)) {
      clustid_list <- get_cluster(
        object = object,
        clustid_char = clustid,
        N = N,
        bootcluster = bootcluster,
        call_env = call_env
      )

      vcov_sign <- clustid_list$vcov_sign
      clustid <- clustid_list$clustid
      clustid_dims <- ncol(clustid)
      N_G <- clustid_list$N_G
      cluster_names <- clustid_list$cluster_names

      cluster_bootcluster <- clustid_list$cluster_bootcluster
      bootcluster <- clustid_list$bootcluster
      all_c <- clustid_list$all_c
    } else {
      vcov_sign <-
        clustid_dims <-
        clustid <- bootcluster <- N_G <- cluster_names <- NULL
      cluster_bootcluster <- bootcluster <- all_c <- NULL
    }


    instruments <- X_exog <- X_endog <- NULL
    if (!is.matrix(R)) {
      R0 <- rep(0, length(colnames(X)))
      R0[match(param, colnames(X))] <- R
      names(R0) <- colnames(X)
    } else {
      q <- nrow(R)
      p <- ncol(R)
      R0 <- matrix(0, q, ncol(X))
      R0[, 1:p] <- R
    }



    res <- list(
      Y = Y,
      X = X,
      weights = weights,
      fixed_effect = NULL,
      W = NULL,
      n_fe = NULL,
      N = N,
      k = k,
      k2 = 0,
      clustid = clustid,
      vcov_sign = vcov_sign,
      clustid_dims = clustid_dims,
      N_G = N_G,
      cluster_bootcluster = cluster_bootcluster,
      bootcluster = bootcluster,
      N_G_bootcluster = length(unique(bootcluster[[1]])),
      R0 = R0,
      X_exog = NULL,
      X_endog = NULL,
      instruments = NULL,
      has_fe = has_fe,
      all_c = all_c
    )

    class(res) <- c("preprocess", "ols")

    res
  }



preprocess2.ivreg <-
  function(object,
           clustid,
           R,
           param,
           engine,
           bootcluster) {
    #' preprocess data for objects of type ivreg
    #'
    #' @param object an object of type ivreg
    #' @param clustid a character string containing the name(s) of the
    #'  cluster variables
    #' @param R Hypothesis Vector giving linear combinations of coefficients.
    #' @param param character vector. names of the parameter(s) to test
    #' @param engine The bootstrap algorithm to run. Either "R" or
    #'  "WildBootTests.jl"
    #' @param bootcluster a character string containing the name(s) of
    #' the bootcluster variables. Alternatively, "min" or "max"
    #'
    #' @noRd
    #'
    #' @method preprocess2 ivreg

    call <- object$call
    call_env <- environment(formula(object))
    fml <- formula(object)
    fml <- Formula::as.Formula(fml)
    is_iv <- TRUE

    N <- nobs(object)
    # lm and felm don't drop NAs due to multicollinearity, while fixest does
    k <- length(na.omit(coef(object)))
    p <- object$p

    is_iv <- FALSE
    has_fe <- FALSE

    X_endog <-
      model.matrix(
        object,
        component = "regressors", na.rm = TRUE
      )[, object$endogenous, drop = FALSE]
    X_exog <-
      model.matrix(
        object,
        component = "instruments", na.rm = TRUE
      )[, object$exogenous, drop = FALSE]
    instruments <-
      model.matrix(
        object,
        component = "instruments", na.rm = TRUE
      )[, object$instruments, drop = FALSE]
    Y <- model.response(model.frame(object))

    n_exog <- length(object$exogenous)
    n_endog <- length(object$endogenous)
    n_instruments <- length(object$instruments)

    weights <- weights(object)
    if (is.null(weights)) {
      has_weights <- FALSE
      weights <- rep(1, N)
    } else {
      has_weights <- TRUE
    }

    # get cluster variable
    if (!is.null(clustid)) {
      clustid_list <- get_cluster(
        object = object,
        clustid_char = clustid,
        N = N,
        bootcluster = bootcluster,
        call_env = call_env
      )

      vcov_sign <- clustid_list$vcov_sign
      clustid <- clustid_list$clustid
      clustid_dims <- ncol(clustid)
      N_G <- clustid_list$N_G
      cluster_names <- clustid_list$cluster_names

      cluster_bootcluster <- clustid_list$cluster_bootcluster
      bootcluster <- clustid_list$bootcluster
      all_c <- clustid_list$all_c
    } else {
      vcov_sign <-
        clustid_dims <-
        clustid <- bootcluster <- N_G <- cluster_names <- NULL
      cluster_bootcluster <- bootcluster <- all_c <- NULL
    }


    # iv prep
    R0 <- rep(0, n_exog + n_endog)
    R0[
      match(
        param, 
        c(
          names(object$exogenous), names(object$endogenous)
          )
        )
      ] <- R
    names(R0) <- c(
      names(object$exogenous), 
      names(object$endogenous)
    )

    res <- list(
      Y = Y,
      X = NULL,
      weights = weights,
      fixed_effect = NULL,
      W = NULL,
      n_fe = NULL,
      N = N,
      k = k,
      k2 = 0,
      clustid = clustid,
      vcov_sign = vcov_sign,
      clustid_dims = clustid_dims,
      N_G = N_G,
      cluster_bootcluster = cluster_bootcluster,
      bootcluster = bootcluster,
      N_G_bootcluster = length(unique(bootcluster[[1]])),
      R0 = R0,
      # model_frame = model_frame,
      X_exog = X_exog,
      X_endog = X_endog,
      instruments = instruments,
      has_fe = has_fe,
      all_c = all_c
    )


    class(res) <- c("preprocess", "iv")

    res
  }


demean_fe <- function(X, Y, fe, has_weights, N) {
  #' function deamens design matrix X and depvar Y if fe != NULL
  #'
  #' @param X the design matrix X
  #' @param Y the dependent variable Y as a numeric vector
  #' @param fe the name of the fixed effect to be projected out
  #' @param has_weights logical - have regression weights been used in the
  #' original model?
  #' @param N the number of observations
  #'
  #' @return A list that includes - among other things - the demeaned
  #' design matrix X and depvar Y
  #'
  #' @noRd

  g <- collapse::GRP(fe, call = FALSE)
  X <- collapse::fwithin(X, g)
  Y <- collapse::fwithin(Y, g)

  fixed_effect_W <- as.factor(fe[, 1])

  if (!has_weights) {
    levels(fixed_effect_W) <-
      (1 / table(fe)) # because duplicate levels are forbidden
  } else {
    stop(
      "Currently, boottest() does not jointly support regression weights /
      WLS and fixed effects. If you want to use
      boottest() for inference based on WLS, please set fe = NULL."
    )
    # levels(fixed_effect_W) <- 1 / table(fixed_effect)
  }

  W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
  n_fe <- length(unique(fe[, 1]))

  res <- list(
    X = X,
    Y = Y,
    fixed_effect_W,
    W = W,
    n_fe = n_fe
  )

  res
}


transform_fe <-
  function(object, X, Y, fe, has_weights, N, engine) {
    #' preprocess the model fixed effects
    #'
    #'  If is.null(fe) == TRUE, all
    #' fixed effects specified in the fixest or felm model are simply added
    #' as dummy variables to the design matrix X. If !is.null(fe), the fixed
    #' effect specified via fe is projected out in the bootstrap - all other
    #' fixed effects are added as dummy variables
    #' @param object the regression object
    #' @param X the design matrix of the regression object
    #' @param fe character vector, name of the fe to be projected out, or NULL
    #' @param has_weights logical - have regression weights been used in the
    #'  original model?
    #' @param N the number of observations
    #' @param engine bootstrap algorithm to run. Either "R" or
    #' "WildBootTests.jl"
    #'
    #' @return a list containing X - the design matrix plus additionally
    #'  attached fixed effects, and
    #'         fixed_effect - a data.frame containing the fixed effect to
    #'         be projected out
    #'
    #' @noRd

    all_fe <- model_matrix(object, type = "fixef", collin.rm = TRUE)
    # make sure all fixed effects variables are characters

    n_fe <- ncol(all_fe)
    all_fe_names <- names(all_fe)
    k2 <- Reduce("+", lapply(all_fe, function(x) {
      length(unique(x))
    }))

    fe_df <- W <- n_fe <- NULL

    # if a fe is to be projected out in the bootstrap
    if (!is.null(fe)) {
      # add all fe except for fe to data frame
      fe_df <- all_fe[, fe, drop = FALSE]
      add_fe <- all_fe[, all_fe_names != fe, drop = FALSE]
      add_fe_names <- names(add_fe)

      # nothing to add if only one fixed effect in model
      if (length(add_fe_names) != 0) {
        fml_fe <- reformulate(add_fe_names, response = NULL)

        add_fe_dummies <-
          model.matrix(
            fml_fe, model.frame(
              fml_fe,
              data = as.data.frame(
                add_fe
              )
            )
          )
        # drop the intercept
        add_fe_dummies <-
          add_fe_dummies[, -which(colnames(add_fe_dummies) == "(Intercept)")]
        X <-
          as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
      }

      # project out fe
      if (engine == "R") {
        # WildBootTests.jl does demeaning internally
        prep_fe <- demean_fe(X, Y, fe_df, has_weights, N)
        X <- prep_fe$X
        Y <- prep_fe$Y
        W <- prep_fe$W
        n_fe <- prep_fe$n_fe
      }
    } else {
      add_fe <- all_fe
      add_fe_names <- names(add_fe)
      fml_fe <- reformulate(add_fe_names, response = NULL)
      add_fe_dummies <-
        model.matrix(fml_fe, model.frame(fml_fe, data = as.data.frame(add_fe)))
      X <-
        as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
    }

    res <- list(
      X = X,
      Y = Y,
      W = W,
      n_fe = n_fe,
      k2 = k2,
      fixed_effect = fe_df
    )

    res
  }


get_cluster <-
  function(object,
           clustid_char,
           bootcluster,
           N,
           call_env) {
    #' function creates a data.frame with cluster variables
    #'
    #' @param object An object of type lm, fixest, felm or ivreg
    #' @param clustid_char the name of the cluster variable(s) as
    #' a character vector
    #' @param bootcluster the name of the bootcluster variable(s)
    #'  as a character vector, or "min" or "max" for multiway clustering
    #' @param N the number of observations used in the bootstrap
    #' @param call_env the environment in which the 'object' was evaluated
    #'
    #' @noRd
    #'
    #' @return a list, containing, among other things, a data.frame of the
    #'  cluster variables,
    #'         a data.frame of the bootcluster variable(s), and a helper
    #'         matrix, all_c, used in `engine_julia()`
    
    # ----------------------------------------------------------------------- #
    # Note: a large part of the following code was taken and adapted from the
    # sandwich R package, which is distributed under GPL-2 | GPL-3
    # Zeileis A, Köll S, Graham N (2020). "Various Versatile Variances:
    # An object-Oriented Implementation of Clustered Covariances in R."
    # _Journal of Statistical Software_,  *95*(1), 1-36.
    # doi: 10.18637/jss.v095.i01 (URL: https://doi.org/10.18637/jss.v095.i01).
    
    # changes by Alexander Fischer:
    # no essential changes, but slight reorganization of pieces of code
    
    dreamerr::check_arg(clustid_char, "character scalar|charakter vector")
    dreamerr::check_arg(bootcluster, "character scalar | character vector")
    
    clustid_fml <- reformulate(clustid_char)
    # Step 1: create cluster df
    
    cluster_tmp <-
      if ("Formula" %in% loadedNamespaces()) {
        ## FIXME to suppress potential warnings due to | in Formula
        suppressWarnings(
          expand.model.frame(
            model = 
              if(inherits(object, "fixest")){
                if(!is.null(object$fixef_vars)){
                  update(object, . ~ + 1 | . + 1)
                } else {
                  update(object, . ~ + 1 )
                }
              } else {
                object
              },
            extras = clustid_fml,
            na.expand = FALSE,
            envir = call_env
          )
        )
      } else {
        expand.model.frame(
          model = 
            if(inherits(object, "fixest")){
              if(!is.null(object$fixef_vars)){
                update(object, . ~ + 1 | . + 1)
              } else {
                update(object, . ~ + 1 )
              }
            } else {
              object
            },
          extras = clustid_fml,
          na.expand = FALSE,
          envir = call_env
        )
      }
    
    # if(inherits(cluster_tmp, "try-error")){
    #   if(inherits(object, "fixest") || inherits(object, "felm")){
    #     if(grepl("non-numeric argument to binary operator$", 
    #              attr(cluster_tmp, "condition")$message)){
    #       stop("In your model, you have specified multiple fixed effects,
    #            none of which are of type factor. While `fixest::feols()` and
    #            `lfe::felm()` handle this case without any troubles,  
    #            `boottest()` currently cannot handle this case - please 
    #            change the type of (at least one) fixed effect(s) to factor.
    #            If this does not solve the error, please report the issue
    #            at https://github.com/s3alfisc/fwildclusterboot.")
    #     }
    #     if(grepl("operations are possible only for numeric, logical
    #              or complex types$", 
    #              attr(cluster_tmp, "condition")$message)){
    #       stop("Either a fixed effect or a cluster variable in your fixest()
    #            or felm() model is currently specified as a character. 
    #            'boottest()' relies on 'expand.model.frame',
    #            which can not handle these variable types in models.
    #            Please change these character variables to factors. ")
    #     }
    #   }
    # }
    
    cluster_df <-
      model.frame(clustid_fml, cluster_tmp, na.action = na.pass)
    # without cluster intersection
    N_G <-
      vapply(cluster_df, function(x) {
        length(unique(x))
      }, numeric(1))
    
    # Step 1: decode bootcluster variable
    
    # create a bootcluster vector
    if (length(bootcluster) == 1) {
      if (bootcluster == "max") {
        # use both vars
        bootcluster_char <- clustid_char
      } else if (bootcluster == "min") {
        # only minimum var
        bootcluster_char <- clustid_char[which.min(N_G)]
      } else {
        bootcluster_char <- bootcluster
      }
    } else {
      bootcluster_char <- bootcluster
    }
    
    # add bootcluster variable to formula of clusters
    cluster_bootcluster_fml <-
      update(
        clustid_fml, paste(
          "~ . +", paste(
            bootcluster_char,
            collapse = " + "
          )
        )
      )
    
    
    cluster_bootcluster_tmp <-
      if ("Formula" %in% loadedNamespaces()) {
        ## FIXME to suppress potential warnings due to | in Formula
        suppressWarnings(
          expand.model.frame(
            model = 
              if(inherits(object, "fixest")){
                if(!is.null(object$fixef_vars)){
                  update(object, . ~ + 1 | . + 1)
                } else {
                  update(object, . ~ + 1 )
                }
              } else {
                object
              },
            extras = cluster_bootcluster_fml,
            na.expand = FALSE,
            envir = call_env
          )
        )
      } else {
        expand.model.frame(
          model = 
            if(inherits(object, "fixest")){
              if(!is.null(object$fixef_vars)){
                update(object, . ~ + 1 | . + 1)
              } else {
                update(object, . ~ + 1 )
              }
            } else {
              object
            },
          extras = cluster_bootcluster_fml,
          na.expand = FALSE,
          envir = call_env
        )
      }
    
    # data.frame as needed for WildBootTests.jl
    cluster_bootcluster_df <- model.frame(
      cluster_bootcluster_fml,
      cluster_bootcluster_tmp,
      na.action = na.pass
    )
    
    # if(inherits(cluster_tmp, "try-error")){
    #   if(inherits(object, "fixest") || inherits(object, "felm")){
    #     if(
    #       grepl(
    #         "non-numeric argument to binary operator$",
    #         attr(
    #           cluster_tmp, "condition"
    #         )$message
    #       )
    #     ){
    #       stop(
    #         "In your model, you have specified multiple fixed effects, 
    #       none of which are of type factor. While `fixest::feols()` and 
    #       `lfe::felm()` handle this case without any troubles,  `boottest()`
    #       currently cannot handle this case - please change the type of
    #       (at least one) fixed effect(s) to factor. If this does not solve
    #       the error, please report the issue at 
    #       https://github.com/s3alfisc/fwildclusterboot."
    #       )
    #     }
    #     if(
    #       grepl(
    #         "operations are possible only for numeric, logical or complex types$",
    #         attr(
    #           cluster_tmp,
    #           "condition")$message
    #       )
    #     ){
    #       stop(
    #         "Either a fixed effect or a cluster variable in your fixest() or
    #       felm() model is currently specified as a character. 'boottest()' 
    #       relies on 'expand.model.frame', which can not handle these variable
    #       types in models. Please change these character variables to factors."
    #       )
    #     }
    #   }
    # }
    
    # data.frames with clusters, bootcluster
    cluster <- cluster_bootcluster_df[, clustid_char, drop = FALSE]
    bootcluster <-
      cluster_bootcluster_df[, bootcluster_char, drop = FALSE]
    
    if (!any(bootcluster_char %in% clustid_char)) {
      is_subcluster <- TRUE
      if (!(any(names(bootcluster) %in% c(clustid_char, names(coef(
        object
      )))))) {
        stop(
          "A bootcluster variable is neither contained in the cluster
          variables nor in the model coefficients."
        )
      }
    } else {
      is_subcluster <- FALSE
    }
    
    ## handle omitted or excluded observations (works for lfe, lm)
    if ((N != NROW(cluster)) &&
        !is.null(object$na.action) &&
        (class(object$na.action) %in% c("exclude", "omit"))) {
      cluster <- cluster[-object$na.action, , drop = FALSE]
    }
    
    if ((N != NROW(bootcluster)) &&
        !is.null(object$na.action) &&
        (class(object$na.action) %in% c("exclude", "omit"))) {
      bootcluster <- bootcluster[-object$na.action, , drop = FALSE]
    }
    
    if (N != nrow(cluster) && inherits(object, "fixest")) {
      cluster <- cluster[unlist(object$obs_selection), , drop = FALSE]
      bootcluster <-
        bootcluster[unlist(object$obs_selection), , drop = FALSE]
    }
    
    if (NROW(cluster) != N) {
      stop("number of observations in 'cluster' and 'nobs()' do not match")
    }
    if (NROW(bootcluster) != N) {
      stop("number of observations in 'bootcluster' and 'nobs()' do not match")
    }
    
    if (any(is.na(cluster))) {
      stop(
        "`boottest()` cannot handle NAs in `clustid` variables that are not
        part of the estimated model object."
      )
    }
    if (any(is.na(bootcluster))) {
      stop(
        "`boottest()` cannot handle NAs in `bootcluster` variables that are
        not part of the estimated model object."
      )
    }
    
    
    clustid_dims <- length(clustid_char)
    
    i <- !vapply(cluster, is.numeric, logical(1))
    cluster[i] <- lapply(cluster[i], as.character)
    
    # taken from multiwayvcov::cluster.boot
    acc <- list()
    for (i in 1:clustid_dims) {
      acc <-
        append(acc, utils::combn(1:clustid_dims, i, simplify = FALSE))
    }
    
    vcov_sign <- vapply(acc, function(i) {
      (-1)^(length(i) + 1)
    }, numeric(1))
    acc <- acc[-1:-clustid_dims]
    
    if (clustid_dims > 1) {
      for (i in acc) {
        cluster <- cbind(cluster, Reduce(paste, cluster[, i]))
        names(cluster)[length(names(cluster))] <-
          Reduce(paste, names(cluster[, i]))
      }
    }
    
    N_G <- vapply(cluster, function(x) {
      length(unique(x))
    }, numeric(1))
    
    # now do all the other bootcluster things
    c1 <-
      bootcluster_char[which(!(bootcluster_char %in% clustid_char))]
    # both bootstrapping and error cluster: all variables in clustid_char that
    # are also in bootcluster
    c2 <- clustid_char[which(clustid_char %in% bootcluster_char)]
    # only error cluster: variables in clustid_char not in c1, c2
    c3 <- clustid_char[which(!(clustid_char %in% c(c1, c2)))]
    all_c <- c(c1, c2, c3)
    
    if (length(bootcluster_char) > 1) {
      bootcluster <- as.data.frame(Reduce(paste, bootcluster))
      names(bootcluster) <- Reduce(paste, bootcluster_char)
    }
    
    
    res <- list(
      vcov_sign = vcov_sign,
      clustid_dims = clustid_dims,
      clustid = cluster,
      N_G = N_G,
      cluster_names = names(cluster),
      all_c = all_c,
      bootcluster = bootcluster,
      cluster_bootcluster = cluster_bootcluster_df
    )
    
    res
  }

