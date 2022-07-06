#' run the wild cluster bootstrap via WildBootTests.jl and the 
#' JuliaConnectoR package
#' 
#' @param preprocess A list: output of the preprocess2 function.
#' @param impose_null A logical - should the null hypothesis be imposed or not?
#' @param r Shifts the null hypothesis.
#' @param B number of bootstrap iterations
#' @param bootcluster A data.frame containing the bootcluster
#' @param clustid A data.frame containing the cluster variables
#' @param sign_level The significance level.
#' @param conf_int Logical. Should confidence intervals be calculated
#'  (by test inversion)?
#' @param tol Numeric vector of length 1. The desired accuracy
#'        (convergence tolerance) used in the root finding procedure to
#'        find the confidence interval.
#'        1e-6 by default.
#' @param p_val_type type Type of p-value. By default "two-tailed".
#' Other options: "equal-tailed", ">", "<"
#' @param type character or function. The character string specifies
#'  the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm"
#'        and "webb". Alternatively, type can be a function(n) for drawing
#'        wild bootstrap factors. "rademacher" by default.
#' @param floattype Should floating
#' @param bootstrapc Logical scalar. TRUE  to request
#' bootstrap-c instead of bootstrap-t. Only relevant when 'boot_algo =
#' "WildBootTests.jl"'
#' @param getauxweights Logical. Whether to save auxilliary weight matrix (v)
#' @param fweights Should frequency weight or probability weights be used for 
#' WLS? Currently, only frequency weights are supported
#' @param internal_seed an integer. sets the seed used in Julia
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set
#' the maximum size of auxilliary weight matrix (v), in gigabytes. Only
#'  relevant when 'boot_algo = "WildBootTests.jl"'
#' @param small Logical Should a small sample correction (N-1)/(N-k) be used?
#' @param clusteradj Logical. Should a ssc G / (G-1) be used?
#' @param clustermin Logical. Should G be computed as min(G) in case of multi-
#' way clustering?
#' @param fe Character string. The fixed effect to be projected out in the 
#' bootstrap
#' @param fedfadj Should fixed effects be included or excluded in k? Currently, 
#' they are always excluded
#' @param liml Logical. Should liml estimation be employed? Currntly always 
#' FALSE
#' @param arubin Shoud the Anderson-Rubin test be performed? Currently always
#' FALSE
#' @param fuller numeric - fuller parameter
#' @param kappa numeric - kappa parameter

#'  point numbers in Julia be represented as 32 or 64 bit? Only relevant when
#'  'boot_algo = "WildBootTests.jl"'

#' @importFrom JuliaConnectoR juliaEval juliaImport
#' @noRd

boot_algo_julia <- function(preprocess,
                            impose_null,
                            r,
                            B,
                            bootcluster,
                            clustid,
                            sign_level,
                            conf_int,
                            tol,
                            p_val_type,
                            type,
                            floattype,
                            bootstrapc,
                            getauxweights,
                            fweights,
                            internal_seed,
                            maxmatsize,
                            small,
                            clusteradj,
                            clustermin,
                            fe = NULL,
                            fedfadj = NULL,
                            liml = NULL,
                            arubin = NULL,
                            fuller = NULL,
                            kappa = NULL) {
  
  resp <- as.numeric(preprocess$Y)

  if (inherits(preprocess, "iv")) {
    predexog <- preprocess$X_exog
    predendog <- preprocess$X_endog
    inst <- preprocess$instruments
  } else if (inherits(preprocess, "ols")) {
    predexog <- preprocess$X
  }

  if (is.matrix(preprocess$R)) {
    R <- preprocess$R
  } else {
    R <- matrix(preprocess$R, 1, length(preprocess$R))
  }
  r <- r
  reps <- as.integer(B) # WildBootTests.jl demands integer

  all_c <- preprocess$all_c
  clustid_df <-
    preprocess$cluster_bootcluster[, all_c, drop = FALSE]

  # note that c("group_id1", NULL) == "group_id1"
  # clustid_mat <- data.frame(preprocess$model_frame[, all_c])
  # names(clustid_mat) <- all_c
  nrow_df <- nrow(clustid_df)
  clustid_mat <- vapply(clustid_df, to_integer, integer(nrow_df))

  # `nbootclustvar::Integer=1`: number of bootstrap-clustering variables
  # `nerrclustvar::Integer=nbootclustvar`: number of error-clustering variables
  nbootclustvar <-
    ifelse(bootcluster == "max", length(clustid), length(bootcluster))
  nerrclustvar <- length(clustid)

  obswt <- preprocess$weights
  getci <-
    ifelse(is.null(conf_int) || conf_int == TRUE, TRUE, FALSE)
  imposenull <-
    ifelse(is.null(impose_null) || impose_null == TRUE, TRUE, FALSE)
  rtol <- tol

  JuliaConnectoR::juliaEval("using WildBootTests")

  suppressWarnings(WildBootTests <-
    JuliaConnectoR::juliaImport("WildBootTests"))

  ptype <- switch(p_val_type,
    "two-tailed" = "symmetric",
    "equal-tailed" = "equaltail",
    "<" = "lower",
    ">" = "upper",
    ptype
  )

  auxwttype <- switch(type,
    "rademacher" = "rademacher",
    "mammen" = "mammen",
    "norm" = "normal",
    "webb" = "webb",
    "gamma" = "gamma",
    auxwttype
  )

  eval_list <- list(
    floattype,
    R,
    r,
    resp = resp,
    predexog = predexog,
    clustid = clustid_mat,
    nbootclustvar = nbootclustvar,
    nerrclustvar = nerrclustvar,
    nbootclustvar = nbootclustvar,
    nerrclustvar = nerrclustvar,
    obswt = obswt,
    getci = getci,
    imposenull = imposenull,
    rtol = rtol,
    # rng = internal_seed,
    auxwttype = auxwttype,
    ptype = ptype,
    reps = reps,
    fweights = FALSE,
    bootstrapc = bootstrapc
  )

  if (!is.null(internal_seed)) {
    eval_list[["rng"]] <- internal_seed
  }

  if (!is.null(small)) {
    eval_list[["small"]] <- small
  }

  if (!is.null(clusteradj)) {
    eval_list[["clusteradj"]] <- clusteradj
  }

  if (!is.null(clustermin)) {
    eval_list[["clustermin"]] <- clustermin
  }

  if (!is.null(fe)) {
    feid <- as.integer(preprocess$fixed_effect[, 1])
    eval_list[["feid"]] <- feid
    eval_list[["fedfadj"]] <- fedfadj
  }

  if (!is.null(maxmatsize)) {
    eval_list[["maxmatsize"]] <- maxmatsize
  }

  if (!is.null(sign_level)) {
    eval_list[["level"]] <- 1 - sign_level
  }

  if (inherits(preprocess, "iv")) {
    eval_list[["predendog"]] <- predendog
    eval_list[["inst"]] <- inst
    if (!is.null(liml)) {
      eval_list[["liml"]] <- liml
    }
    if (!is.null(arubin)) {
      eval_list[["arubin"]] <- arubin
    }
    if (!is.null(fuller)) {
      eval_list[["fuller"]] <- fuller
    }
    if (!is.null(kappa)) {
      eval_list[["kappa"]] <- kappa
    }
  }

  wildboottest_res <- do.call(WildBootTests$wildboottest, eval_list)

  # collect results:
  p_val <- WildBootTests$p(wildboottest_res)
  if (getci == TRUE) {
    conf_int <- WildBootTests$ci(wildboottest_res)
  } else {
    conf_int <- NA
  }
  t_stat <- WildBootTests$teststat(wildboottest_res)
  t_boot <- FALSE
  if (t_boot == TRUE) {
    t_boot <- WildBootTests$dist(wildboottest_res)
  }

  if (getauxweights == TRUE) {
    getauxweights <- WildBootTests$auxweights(wildboottest_res)
  }

  plotpoints <- WildBootTests$plotpoints(wildboottest_res)
  plotpoints <- cbind(plotpoints$X[[1]], plotpoints$p)

  res_final <- list(
    p_val = p_val,
    conf_int = conf_int,
    t_stat = t_stat,
    t_boot = t_boot,
    auxweights = getauxweights,
    grid_vals = plotpoints[, 1],
    p_grid_vals = plotpoints[, 2]
  )

  res_final
}
