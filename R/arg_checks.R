#' several checks on the param input variable for 'boottest()'
#' @param object an object of type lm, fixest or felm
#' @param param character vector - boottest() function arg 'param'
#' 
#' @importFrom rlang abort warn inform
#' 
#' @noRd

check_params_in_model <- function(object, param) {
  
  # for lm and fixest
  if (inherits(object, "lm")) {
    if (mean(param %in% c(names(coef(object)))) != 1) {
      rlang::abort(paste("The parameter", param, "is not included in the estimated
                  model. Maybe you are trying to test for an interaction 
                  parameter? To see all model parameter names,
                  run names(coef(object))."), 
                   use_cli_format = TRUE)
    }
  }
  
  # for lm and fixest
  if (inherits(object, "fixest")) {
    if (mean(param %in% c(names(coef(object)))) != 1) {
      rlang::abort(paste("The parameter", param, "is not included in the estimated
                  model. Maybe you are trying to test for an interaction 
                  parameter? To see all model parameter names,
                  run names(coef(model))."), 
                  use_cli_format = TRUE)    
      }
  }
  
  # for felm
  if (inherits(object, "felm")) {
    # check if param(s) is (are) in model
    if (mean(param %in% c(rownames(object$coefficients))) != 1) {
      rlang::abort(paste("The parameter", param, "is not included
               in the estimated model. Maybe you are trying to
               test for an interaction parameter? To see all model
               parameter names, run names(coef(model))."), 
               use_cli_format = TRUE)
    }
  }
  
  if (inherits(object, "ivreg")) {
    # which parametrs can be tested?
    if (mean(param %in% names(c(object$exogenous, object$endogenous)) != 1)) {
      rlang::abort(paste("The parameter", param, "is not included in the estimated
                  model. Maybe you are trying to test for an interaction 
                  parameter? To see all model parameter names,
                  run names(coef(model))."))    }
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
      rlang::abort("Currently, only 2SLS is supported. Please set the `ivreg`
           function argument `method` to `OLS`.")
    }
  }
  
  
  if (inherits(object, "felm")) {
    if (!is.null(fe)) {
      if (fe %in% param) {
        rlang::abort(paste("The function argument fe =", fe, "is included in the
                   hypothesis (via the `param` argument). This is not allowed.
                   Please set fe to another factor variable or NULL."),
             call. = FALSE
        )
      }
      if (!(fe %in% names(object$fe))) {
        rlang::abort(paste(
          "The fixed effect to be projected out in the bootstrap,",
          fe, "is not included as a dedicated fixed effect in the
                   estimated model."
        ))
      }
    }
  }
  
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
      # note: whitespace ~ - for IV
      # grepl("~", deparse_fml, fixed = TRUE)
    ) {
      rlang::abort("Advanced formula notation in fixest / fixest via ^ to interact
          fixed effects is currently not supported in boottest().")
    }
    
    
    if (!is.null(fe)) {
      if (fe %in% param) {
        rlang::abort(paste("The function argument fe =", fe, "is included in the
                   hypothesis (via the `param` argument). This is not allowed.
                   Please set fe to another factor variable or NULL."),
             call. = FALSE
        )
      }
      if (!(fe %in% object$fixef_vars)) {
        rlang::abort(paste(
          "The fixed effect to be projected out in the bootstrap,",
          fe, "is not included as a dedicated fixed effect
                   in the estimated model."
        ))
      }
    }
  }
  
  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    rlang::inform(
      paste("Note: The bootstrap usually performs best when the confidence", 
      "level (here,", 1 - sign_level, "% times the number of replications", 
      "plus 1 (", B, "+ 1 = ", B + 1, ") is an integer."), 
      use_cli_format = TRUE
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
        rlang::abort(paste(
          "The fixed effect to be projected out in the bootstrap,",
          fe, "is not included as a dedicated fixed effect in the
                   estimated model."
        ))
      }
    }
  }
  
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
      rlang::abort("Advanced formula notation in fixest / fixest via ^ to interact
          fixed effects is currently not supported in boottest().")
    }
    
    
    if (!is.null(fe)) {
      if (!(fe %in% object$fixef_vars)) {
        rlang::abort(paste(
          "The fixed effect to be projected out in the bootstrap,",
          fe, "is not included as a dedicated fixed effect in the
                   estimated model."
        ))
      }
    }
  }
  
  if (nrow(R) != length(r)) {
    rlang::abort(paste(
      "The dimensions of func args R and r do not match. The number
               of rows of R is ", nrow(R), ", but the length of r is",
      length(r), "."
    ))
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
    rlang::abort("The R-lean algorithm currently only supports oneway clustering.")
  }
  
  if (!is.null(fe)) {
    rlang::abort("boottest() currently does not support 
         fixed effects with engine = 'R-lean'.")
  }
  
  if(!is.null(weights)){
    rlang::abort("boottest() currently does not support regression
         weights with engine = 'R-lean'.")
  }
  
  if(impose_null != TRUE){
    rlang::abort("boottest() currently does not support the 'WCU' bootstrap 
    - which does not impose the null on the 
          bootstrap dgp - for engine = 'R-lean'.")
  }
  
}


check_boot_algo_fastnreliable <- function(
    weights,
    clustid, 
    fe,
    bootstrap_type,
    R, 
    r){
  
  #' function to check input arguments passed to `boot_algo3()`
  #' @param weights NULL or numeric vector
  #' @param clustid character vector 
  #' @param fe NULL or character scalar
  #' @param bootstrap_type either '11', '13', '31, '33'
  #' @param R the constraints vector as specified in the 'boottest()' call
  #' @param r parameter in hypothesis test, as set in 'boottest()' call
  #' @param param character vector. the parameters included in the hypothesis
  #' @noRd
  
  
  if(R != 1L || r != 0){
    
    rlang::abort("Bootstraps of type '11', '13', '31', '33' currently
           only support hypotheses of the form R * beta = 0 vs beta <> 0, 
           where R is a scalar equal to 1.")
  }
  
  if(length(clustid) > 1){
    rlang::abort("The '13', '31', and '33'
         bootstrap variants currently only
         support oneway clustering when 'boot_engine' == 'R'."
    )
  }
  
  if (!is.null(fe)) {
    rlang::abort("The '13', '31', and '33'
         bootstrap variants currently don't support fixed 
         effects in the bootstrap. Please set 'fe = NULL'.")
  }
  
  if(!is.null(weights)){
    rlang::abort("The '13', '31', and '33'
         bootstrap variants currently only
         support oneway clustering when 'boot_engine' == 'R'.")
  }
  
  
}


#' check if full enumeration should be employed, provide message when it is
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
          rlang::warn(
            paste(
              "There are only",
              N_G_2,
              "unique draws from the rademacher distribution for",
              N_G_bootcluster,
              "bootstrap clusters. Therefore, B = ",
              N_G_2,
              " with full enumeration. Consider using webb weights instead.
              Further, note that under full enumeration and with B =",
              N_G_2,
              "bootstrap draws, only 2^(#clusters - 1) = ",
              2^(N_G_bootcluster - 1),
              " distinct t-statistics and p-values can be computed. For a
              more thorough discussion, see Webb `Reworking wild bootstrap
              based inference for clustered errors` (2013)."
            ),
            use_cli_format = TRUE
          )
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
      rlang::abort(
        "Hypotheses with q>1 are currently only supported via WildBootTests.jl.
        Please set the function argument 'engine = WildBootTests.jl'."
      )
    }
  }
  
  if (p_val_type %in% c(">", "<")) {
    if (conf_int == TRUE) {
      conf_int <- FALSE
      rlang::warn(
        paste(
          "Currently, boottest() calculates confidence intervals for one-sided
          hypotheses only for `engine = 'WildBootTests.jl'`."
        ),
        use_cli_format = TRUE
      )
    }
  }
  
  
  if (conf_int == TRUE || is.null(conf_int)) {
    if (B < 100) {
      rlang::abort(
        "The function argument B is smaller than 100. The number of bootstrap
          iterations needs to be 100 or higher in order to guarantee that the
          root finding procudure used to find the confidence set
          works properly.",
        call. = FALSE
      )
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
      rlang::abort("The constraints vector must either be NULL or a numeric of
           the same length as the `param` input vector.")
    }
  }
  R
}


check_engine_btype <- function(
    engine, 
    bootstrap_type){
  
  if(engine == "WildBootTests.jl"){
    if(!(bootstrap_type %in% c("11", "fnw11", "31"))){
      rlang::abort(
        paste(
          "The bootstrap of type", 
          bootstrap_type, 
          "is not yet supported via 'WildBootTests. You can run it via 
          the 'R' engine instead.'")
      )
    }
  } else if(engine == "R-lean"){
    if(bootstrap_type != "fnw11"){
      if(bootstrap_type == "31"){
        rlang::abort(
          paste(
            "The bootstrap of type", 
            bootstrap_type, 
            "is not yet supported via 'R-lean'. You can run it via 
          the 'R' or 'WildBootTests.jl' engines instead.'")
        )
      } else {
        rlang::abort(
          paste(
            "The bootstrap of type", 
            bootstrap_type, 
            "is not yet supported via 'R-lean'. You can run it via 
          the 'R' engine instead.'")
        )
      }
      
    }
  }
  
}


check_bootstrap_types <- function(param, bootstrap_type){
  
  if(length(param) > 1){
    if(bootstrap_type != "fnw11"){
      rlang::abort("Only bootstrap_type = 'fnw11' is currently supported with
           hypotheses that contain more than one parameter. This feature 
           will be added in the near future.")
    }
  }
  
}
