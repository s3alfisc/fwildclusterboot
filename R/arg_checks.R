#' several checks on the param input variable for 'boottest()'
#' @param object an object of type lm, fixest or felm
#' @param param character vector - boottest() function arg 'param'
#'
#' @noRd

check_params_in_model <- function(object, param) {

  # for lm and fixest
  if (inherits(object, "lm")) {
    if (mean(param %in% c(names(coef(object)))) != 1) {
      param_not_in_model_error(param)
    }
  }

  # for lm and fixest
  if (inherits(object, "fixest")) {
    if (mean(param %in% c(names(coef(object)))) != 1) {
      param_not_in_model_error(param)
      }
  }

  # for felm
  if (inherits(object, "felm")) {
    # check if param(s) is (are) in model
    if (mean(param %in% c(rownames(object$coefficients))) != 1) {
      param_not_in_model_error(param)
    }
  }

  if (inherits(object, "ivreg")) {
    # which parametrs can be tested?
    if (mean(param %in% names(c(object$exogenous, object$endogenous)) != 1)) {
      param_not_in_model_error(param)
    }
  }
}

#' several checks on the input args for 'boottest()'
#' @param object an object of type lm, fixest or felm
#' @param R Numeric vector defining the test
#' @param param character vector - the parameters to be tested
#' @param sign_level The significance level
#' @param B the number of bootstrap iterations
#' @param fe NULL or numeric scalar - the fixed effect to be projected
#' out in the bootstrap
#' @noRd
#'
check_boottest_args_plus <- function(
    object, R, param, sign_level, B, fe = NULL) {

  if (inherits(object, "ivreg")) {
    if (object$method != "OLS") {
      only_2SLS_for_IV_error()
    }
  }

  if (inherits(object, "felm")) {
    if (!is.null(fe)) {
      if (fe %in% param) {
        fe_in_test_error(fe)
      }
      if (!(fe %in% names(object$fe))) {
        fixef_not_fixef_error(fe)      }
    }
  }

  #' @srrstats {G2.4c} *explicit conversion to character via `as.character()`
  #' (and not `paste` or `paste0`)* Done

  if (inherits(object, "fixest")) {
    deparse_fml <- Reduce(
      paste, as.character(as.formula(object$fml_all$linear))
    )

    if (
      # '^' illegal in fixef argument, but legal in main formula -
      # e.g. fml = y ~ x1 + I(x2^2) shold be possible
      #' @srrstats {G2.4c} *explicit conversion to character via `as.character()`
      #' (and not `paste` or `paste0`)* Done

      ("fixef_vars" %in% names(object) &&
       grepl("^",
             Reduce(paste, as.character(as.formula(object$fml_all$fixef))),
             fixed = TRUE
       ))
      # note: whitespace ~ - for IV
      # grepl("~", deparse_fml, fixed = TRUE)
    ) {
      advanced_formula_error()
    }

    if (
      # '^' illegal in fixef argument, but legal in main formula -
      # e.g. fml = y ~ x1 + I(x2^2) shold be possible
      #' @srrstats {G2.4c} *explicit conversion to character via `as.character()`
      #' (and not `paste` or `paste0`)* Done

      ("fixef_vars" %in% names(object) &&
       grepl("[",
             Reduce(paste, as.character(as.formula(object$fml_all$fixef))),
             fixed = TRUE
       ))
      # note: whitespace ~ - for IV
      # grepl("~", deparse_fml, fixed = TRUE)
    ) {
      variyng_slopes_error()
    }



    if (!is.null(fe)) {
      if (fe %in% param) {
        fe_in_test_error(fe)
      }
      if (!(fe %in% object$fixef_vars)) {
        fixef_not_fixef_error(fe)      }
    }
  }

  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(
      paste(
        "Note: The bootstrap usually performs best when the confidence",
        "level (here,", 
        1 - sign_level, 
        "%) times the number of replications",
        "plus 1 (", B, "+ 1 = ", B + 1, ") is an integer."
      )
    )
  }
}


#' input argument tests for 'mboottest()'
#'
#' @param object an object of type lm, fixest or felm
#' @param R Numeric vector defining the test
#' @param r Numeric scalar, shifting the test
#' @param fe NULL or numeric scalar - the fixed effect to be projected
#' out in the bootstrap
#' @noRd


check_mboottest_args_plus <- function(object, R, r, fe) {
  if (inherits(object, "felm")) {
    if (!is.null(fe)) {
      if (!(fe %in% names(object$fe))) {
        fixef_not_fixef_error(fe)      }
    }
  }

  #' @srrstats {G2.4c} *explicit conversion to character via `as.character()`
  #' (and not `paste` or `paste0`)* Done

  if (inherits(object, "fixest")) {
    deparse_fml <- Reduce(
      paste, as.character(as.formula(object$fml_all$linear))
    )

    if (
      # '^' illegal in fixef argument, but legal in main formula -
      # e.g. fml = y ~ x1 + I(x2^2) shold be possible
      ("fixef_vars" %in% names(object) &&
       grepl("^",
             Reduce(paste, as.character(as.formula(object$fml_all$fixef))),
             fixed = TRUE
       ))

    ) {
      advanced_formula_error()
    }


    if (!is.null(fe)) {
      if (!(fe %in% object$fixef_vars)) {
        fixef_not_fixef_error(fe)
      }
    }
  }

  if (nrow(R) != length(r)) {
    test_dimension_error()
  }
}




#' some checks when 'engine = R-lean"
#' @param weights NULL or numeric vector
#' @param clustid character vector
#' @param fe NULL or character scalar
#' @param impose_null Logical, whether the null is imposed
#' on the bootstrap dgp or not
#' @noRd


check_r_lean <- function(weights, clustid, fe, impose_null){

  if(length(clustid) > 1){
    no_clustering_for_rlean_error()
  }

  if (!is.null(fe)) {
    no_fixef_for_rlean_error()
  }

  if(!is.null(weights)){
    no_weights_for_rlean_error()
  }

  if(impose_null != TRUE){
    no_wcu_for_rlean_error()
  }

}


check_boot_algo_fastnreliable <- function(
    weights,
    clustid,
    bootcluster,
    fe,
    bootstrap_type,
    R,
    r){

  #' function to check input arguments passed to `boot_algo3()`
  #' @param weights NULL or numeric vector
  #' @param clustid character vector
  #' @param bootcluster character vector with bootclusters
  #' @param fe NULL or character scalar
  #' @param bootstrap_type either '11', '13', '31, '33'
  #' @param R the constraints vector as specified in the 'boottest()' call
  #' @param r parameter in hypothesis test, as set in 'boottest()' call
  #' @param param character vector. the parameters included in the hypothesis
  #' @noRd


  if(R != 1L || r != 0){

    non_scalar_test_error()
  
  }

  if(length(clustid) > 1){
    only_onweway_clustering_for_fast_reliable
  }

  if (!is.null(fe)) {
    if(bootstrap_type %in% c("11", "31")){
      if(!(fe == clustid)){
        only_cluster_fixef_for_11_31_error()
      }
    }

    if (bootstrap_type %in% c("13", "33")) {
      no_fixef_for_13_33_error()
    }
  }

  if(!is.null(weights)){
    no_weights_for_fast_reliable_error()
  }

  if(length(bootcluster) > 1){
    no_subcluster_bootstrap_fast_reliable()
  }

  if(!(bootcluster %in% c("max", "min", clustid))){
    bootcluster_not_max_min_clustid()
  }



}


#' check if full enumeration should be employed, provide message when it is. 
#' in this case, overwrite the number of bootstrap iterations to the number of
#' enumerations.
#' @param heteroskedastic Logical. Is the heteroskedastic or a wild cluster
#' bootstrap being run?
#' @param preprocess A list created via the preprocess2 function
#' @param B Integer. The number of bootstrap iterations
#' @param type. The type of test to be run
#' @param engine. Character scalar, either "R", "WildBootTests.jl" or
#' "R-lean"
#' @noRd

check_set_full_enumeration <-
  function(heteroskedastic = FALSE,
           preprocess,
           B,
           type,
           engine) {
    full_enumeration <- FALSE

    if (heteroskedastic == FALSE) {
      N_G_bootcluster <- preprocess$N_G_bootcluster
      N_G_2 <- 2^N_G_bootcluster
      if (type == "rademacher") {
        if (N_G_2 <= B) {
          full_enumeration_warning(N_G_2, N_G_bootcluster)
          full_enumeration <- TRUE
          if (engine != "WildBootTests.jl") {
            # this is handled internally by WildBootTests.jl, so don't update B
            B <- N_G_2
          }
        }
      }
    }

    res <- list(
      B = B,
      full_enumeration = full_enumeration
    )

    res
  }

#' some checks on the chosen algorithm
#' @param R constraints vector
#' @param p_val_type type of pvalue
#' @param conf_int logical - TRUE or FALSE
#' @param B number of bootstrap iterations
#' @noRd

r_algo_checks <- function(R, p_val_type, conf_int, B) {
  if (!is.null(R)) {
    if (length(nrow(R)) != 0) {
      multi_hypothesis_only_via_julia()
    }
  }

  if (p_val_type %in% c(">", "<")) {
    if (conf_int == TRUE) {
      conf_int <- FALSE
      one_sided_cis_only_via_julia()
    }
  }


  if (conf_int == TRUE || is.null(conf_int)) {
    if (B < 100) {
      B_too_small_error()
    }
  }
}

#' some preprocessing of the constraint vector-matrix R
#' @param R constraints vector
#' @param param character vector - name of the parameters in the hypothesis
#' test
#' @noRd

process_R <- function(R, param) {
  # check R & param
  if (is.null(R)) {
    R <- rep(1, length(param))
  } else {
    if (length(R) != length(param)) {
      R_must_be_NULL_or_param()
    }
  }
  R
}


check_engine_btype <- function(
    engine,
    bootstrap_type){

  if(engine == "WildBootTests.jl"){
    if(!(bootstrap_type %in% c("11", "fnw11", "31"))){
      no_13_33_bootstap_via_julia(bootstrap_type)
    }
  } else if(engine == "R-lean"){
    if(bootstrap_type != "fnw11"){
      if(bootstrap_type == "31"){
        no_bootstrap_type_for_rlean_error()
      } else {
        no_bootstrap_type_for_rlean_error()
      }

    }
  }

}


check_bootstrap_types <- function(param, bootstrap_type){

  if(length(param) > 1){
    if(bootstrap_type != "fnw11"){
      non_scalar_test_error()
    }
  }

}


