#' several checks on the param input variable for 'boottest()'
#' @param object an object of type lm, fixest or felm
#' @param param character vector - boottest() function arg 'param'
#' @noRd

check_params_in_model <- function(object, param) {
  
  # for lm and fixest
  if (inherits(object, "lm")) {
    if (mean(param %in% c(names(coef(object)))) != 1) {
      stop(paste("The parameter", param, "is not included in the estimated
                  model. Maybe you are trying to test for an interaction 
                  parameter? To see all model parameter names,
                  run names(coef(model))."))
    }
  }
  
  # for lm and fixest
  if (inherits(object, "fixest")) {
    if (mean(param %in% c(names(coef(object)))) != 1) {
      stop(paste("The parameter", param, "is not included in the estimated
                  model. Maybe you are trying to test for an interaction 
                  parameter? To see all model parameter names,
                  run names(coef(model))."))    }
  }
  
  # for felm
  if (inherits(object, "felm")) {
    # check if param(s) is (are) in model
    if (mean(param %in% c(rownames(object$coefficients))) != 1) {
      stop(paste("The parameter", param, "is not included
               in the estimated model. Maybe you are trying to
               test for an interaction parameter? To see all model
               parameter names, run names(coef(model))."))
    }
  }
  
  if (inherits(object, "ivreg")) {
    # which parametrs can be tested?
    if (mean(param %in% names(c(object$exogenous, object$endogenous)) != 1)) {
      stop(paste("The parameter", param, "is not included in the estimated
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
      stop("Currently, only 2SLS is supported. Please set the `ivreg`
           function argument `method` to `OLS`.")
    }
  }
  
  
  if (inherits(object, "felm")) {
    if (!is.null(fe)) {
      if (fe %in% param) {
        stop(paste("The function argument fe =", fe, "is included in the
                   hypothesis (via the `param` argument). This is not allowed.
                   Please set fe to another factor variable or NULL."),
             call. = FALSE
        )
      }
      if (!(fe %in% names(object$fe))) {
        stop(paste(
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
      stop("Advanced formula notation in fixest / fixest via ^ to interact
          fixed effects is currently not supported in boottest().")
    }
    
    
    if (!is.null(fe)) {
      if (fe %in% param) {
        stop(paste("The function argument fe =", fe, "is included in the
                   hypothesis (via the `param` argument). This is not allowed.
                   Please set fe to another factor variable or NULL."),
             call. = FALSE
        )
      }
      if (!(fe %in% object$fixef_vars)) {
        stop(paste(
          "The fixed effect to be projected out in the bootstrap,",
          fe, "is not included as a dedicated fixed effect
                   in the estimated model."
        ))
      }
    }
  }
  
  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    msg <- format_message(
    "Note: The bootstrap usually performs best when the confidence", 
     paste("level (here,", 1 - sign_level, "%) times the number of replications"), 
     paste("plus 1 (", B, "+ 1 = ", B + 1, ") is an integer.")
    )
    message(msg)
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
        stop(paste(
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
      stop("Advanced formula notation in fixest / fixest via ^ to interact
          fixed effects is currently not supported in boottest().")
    }
    
    
    if (!is.null(fe)) {
      if (!(fe %in% object$fixef_vars)) {
        stop(paste(
          "The fixed effect to be projected out in the bootstrap,",
          fe, "is not included as a dedicated fixed effect in the
                   estimated model."
        ))
      }
    }
  }
  
  if (nrow(R) != length(r)) {
    stop(paste(
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
    stop("The R-lean algorithm currently only supports oneway clustering.")
  }
  
  if (!is.null(fe)) {
    stop("boottest() currently does not support 
         fixed effects with engine = 'R-lean'.")
  }
  
  if(!is.null(weights)){
    stop("boottest() currently does not support regression
         weights with engine = 'R-lean'.")
  }
  
  if(impose_null != TRUE){
    stop("boottest() currently does not support the 'WCU' bootstrap 
    - which does not impose the null on the 
          bootstrap dgp - for engine = 'R-lean'.")
  }
  
}


check_boot_algo3 <- function(
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

      stop("Bootstraps of type '11', '13', '31', '33' currently
           only support hypotheses of the form R * beta = 0 vs beta <> 0, 
           where R is a scalar equal to 1.")
  }
  
  if(length(clustid) > 1){
    stop("The '13', '31', and '33'
         bootstrap variants currently only
         support oneway clustering when 'boot_engine' == 'R'."
         )
  }
  
  if (!is.null(fe)) {
    stop("The '13', '31', and '33'
         bootstrap variants currently don't support fixed 
         effects in the bootstrap. Please set 'fe = NULL'.")
  }
  
  if(!is.null(weights)){
    stop("The '13', '31', and '33'
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
          warning(
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
            call. = FALSE,
            noBreaks. = TRUE
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
      stop(
        "Hypotheses with q>1 are currently only supported via WildBootTests.jl.
        Please set the function argument 'engine = WildBootTests.jl'."
      )
    }
  }
  
  if (p_val_type %in% c(">", "<")) {
    if (conf_int == TRUE) {
      conf_int <- FALSE
      warning(
        paste(
          "Currently, boottest() calculates confidence intervals for one-sided
          hypotheses only for engine = 'WildBootTests.jl'."
        ),
        call. = FALSE
      )
    }
  }
  
  
  if (conf_int == TRUE || is.null(conf_int)) {
    if (B <= 100) {
      stop(
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
      stop("The constraints vector must either be NULL or a numeric of
           the same length as the `param` input vector.")
    }
  }
  R
}