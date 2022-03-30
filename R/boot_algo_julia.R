boot_algo_julia <- function(preprocess, impose_null, r, B, bootcluster, clustid, sign_level, conf_int, tol, small_sample_adjustment, p_val_type, type,
                            floattype, bootstrapc, getauxweights, fweights, internal_seed, maxmatsize, small, fe = NULL, fedfadj = NULL, LIML = NULL, ARubin = NULL, Fuller = NULL, kappa = NULL) {
  
  resp <- as.numeric(preprocess$Y)
  
  if(inherits(preprocess, "iv")){
    predexog <- preprocess$X_exog
    predendog <- preprocess$X_endog
    inst <- preprocess$instruments
  } else if(inherits(preprocess, "ols")){
    predexog <- preprocess$X
  }

  if (is.matrix(preprocess$R)) {
    R <- preprocess$R
  } else {
    R <- matrix(preprocess$R, 1, length(preprocess$R))
  }
  r <- r
  reps <- as.integer(B) # WildBootTests.jl demands integer

  # Order the columns of `clustid` this way:
  # 1. Variables only used to define bootstrapping clusters, as in the subcluster bootstrap.
  # 2. Variables used to define both bootstrapping and error clusters.
  # 3. Variables only used to define error clusters.
  # In the most common case, `clustid` is a single column of type 2.

  if (length(bootcluster == 1) && bootcluster == "max") {
    bootcluster_n <- clustid
  } else if (length(bootcluster == 1) && bootcluster == "min") {
    bootcluster_n <- names(preprocess$N_G[which.min(preprocess$N_G)])
  }

  # only bootstrapping cluster: in bootcluster and not in clustid
  c1 <- bootcluster_n[which(!(bootcluster_n %in% clustid))]
  # both bootstrapping and error cluster: all variables in clustid that are also in bootcluster
  c2 <- clustid[which(clustid %in% bootcluster_n)]
  # only error cluster: variables in clustid not in c1, c2
  c3 <- clustid[which(!(clustid %in% c(c1, c2)))]
  all_c <- c(c1, c2, c3)
  # all_c <- lapply(all_c , function(x) ifelse(length(x) == 0, NULL, x))

  # note that c("group_id1", NULL) == "group_id1"
  clustid_mat <- data.frame(preprocess$model_frame[, all_c])
  names(clustid_mat) <- all_c
  clustid_df <- base::as.matrix(sapply(clustid_mat, to_integer))

  # `nbootclustvar::Integer=1`: number of bootstrap-clustering variables
  # `nerrclustvar::Integer=nbootclustvar`: number of error-clustering variables
  nbootclustvar <- ifelse(bootcluster == "max", length(clustid), length(bootcluster))
  nerrclustvar <- length(clustid)

  obswt <- preprocess$weights
  getCI <- ifelse(is.null(conf_int) || conf_int == TRUE, TRUE, FALSE)
  imposenull <- ifelse(is.null(impose_null) || impose_null == TRUE, TRUE, FALSE)
  rtol <- tol

  JuliaConnectoR::juliaEval("using WildBootTests")
  # JuliaConnectoR::juliaEval('using StableRNGs')

  WildBootTests <- JuliaConnectoR::juliaImport("WildBootTests")

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

  eval_list <- list(floattype,
    R,
    r,
    resp = resp,
    predexog = predexog,
    clustid = clustid_df,
    nbootclustvar = nbootclustvar,
    nerrclustvar = nerrclustvar,
    nbootclustvar = nbootclustvar,
    nerrclustvar = nerrclustvar,
    obswt = obswt,
    getCI = getCI,
    imposenull = imposenull,
    rtol = rtol,
    small = small,
    rng = internal_seed,
    auxwttype = auxwttype,
    ptype = ptype,
    reps = reps,
    fweights = FALSE,
    bootstrapc = bootstrapc
  )

  if (!is.null(fe)) {
    feid <- as.integer(preprocess$fixed_effect[, 1])
    eval_list[["feid"]] <- feid
    eval_list[["fedfadj"]] <- fedfadj
  }

  if (!is.null(maxmatsize)) {
    eval_list[["maxmatsize"]] <- maxmatsize
  }
  
  if(!is.null(sign_level)){
    eval_list[["level"]] <- 1 - sign_level
  }

  if(inherits(preprocess, "iv")){
      eval_list[["predendog"]] <- predendog
      eval_list[["inst"]] <- inst
      if(!is.null(LIML)){
        eval_list[["LIML"]] <- LIML
      }
      if(!is.null(ARubin)){
        eval_list[["ARubin"]] <- ARubin
      }
      if(!is.null(Fuller)){
        eval_list[["Fuller"]] <- Fuller
      }
      if(!is.null(kappa)){
        eval_list[["kappa"]] <- kappa
      }
      
  }
  
  wildboottest_res <- do.call(WildBootTests$wildboottest, eval_list)

  # collect results:
  p_val <- WildBootTests$p(wildboottest_res)
  if (getCI == TRUE) {
    conf_int <- WildBootTests$CI(wildboottest_res)
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
    plotpoints = plotpoints
  )

  res_final
}