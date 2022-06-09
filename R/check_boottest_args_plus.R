check_boottest_args_plus <- function(object, R, param, sign_level, B, fe = NULL) {


  if (inherits(object, "ivreg")) {
    if (object$method != "OLS") {
      stop("Currently, only 2SLS is supported. Please set the `ivreg` function argument `method` to `OLS`.")
    }
  }

  
  if (inherits(object, "felm")) {
    if(!is.null(fe)){
      if(fe %in% param){
        stop(paste("The function argument fe =", fe, "is included in the hypothesis (via the `param` argument). This is not allowed. Please set fe to another factor variable or NULL."),
             call. = FALSE)
      }
      if(!(fe %in% names(object$fe))){
        stop(paste("The fixed effect to be projected out in the bootstrap,", fe, "is not included as a dedicated fixed effect in the estimated model."))
      } 
    }
  }
  
  if (inherits(object, "fixest")) {
    
    deparse_fml <- Reduce(paste, as.character(as.formula(object$fml_all$linear)))
    
    if (
      # '^' illegal in fixef argument, but legal in main formula - 
      # e.g. fml = y ~ x1 + I(x2^2) shold be possible
      ("fixef_vars" %in% names(object) && 
       grepl("^",
             Reduce(paste, as.character(as.formula(object$fml_all$fixef))),
             fixed = TRUE))
      # note: whitespace ~ - for IV
      # grepl("~", deparse_fml, fixed = TRUE)
    ) {
      stop("Advanced formula notation in fixest / fixest via ^ to interact
          fixed effects is currently not supported in boottest().")
    }
    
    
    if(!is.null(fe)){
      
      if(fe %in% param){
        stop(paste("The function argument fe =", fe, "is included in the hypothesis (via the `param` argument). This is not allowed. Please set fe to another factor variable or NULL."),
             call. = FALSE)
      }
      if(!(fe %in% object$fixef_vars)){
        stop(paste("The fixed effect to be projected out in the bootstrap,", fe, "is not included as a dedicated fixed effect in the estimated model."))
      }
      
    }
  }
  
  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(paste("Note: The bootstrap usually performs best when the
                  confidence level (here,", 1 - sign_level, "%)
                  times the number of replications plus 1
                  (", B, "+ 1 = ", B + 1, ") is an integer."))
  }
 
   
}


check_mboottest_args_plus <- function(object, R, r, B, sign_level, fe) {

  if (inherits(object, "felm")) {
    if(!is.null(fe)){
      if(!(fe %in% names(object$fe))){
        stop(paste("The fixed effect to be projected out in the bootstrap,", fe, "is not included as a dedicated fixed effect in the estimated model."))
      } 
    }
  }
  
  if (inherits(object, "fixest")) {
    
    deparse_fml <- Reduce(paste, as.character(as.formula(object$fml_all$linear)))
    
    if (
      # '^' illegal in fixef argument, but legal in main formula - 
      # e.g. fml = y ~ x1 + I(x2^2) shold be possible
      ("fixef_vars" %in% names(object) && 
       grepl("^",
             Reduce(paste, as.character(as.formula(object$fml_all$fixef))),
             fixed = TRUE))
 
    ) {
      stop("Advanced formula notation in fixest / fixest via ^ to interact
          fixed effects is currently not supported in boottest().")
    }
    
    
    if(!is.null(fe)){
      
      if(!(fe %in% object$fixef_vars)){
        stop(paste("The fixed effect to be projected out in the bootstrap,", fe, "is not included as a dedicated fixed effect in the estimated model."))
      }
      
    }
  }
  
  if (nrow(R) != length(r)) {
     stop(paste("The dimensions of func args R and r do not match. The number of rows of R is ", nrow(R), ", but the length of r is", length(r), "."))
  }
  
  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(paste("Note: The bootstrap usually performs best when the
                  confidence level (here,", 1 - sign_level, "%)
                  times the number of replications plus 1
                  (", B, "+ 1 = ", B + 1, ") is an integer."))
  }
  
}
