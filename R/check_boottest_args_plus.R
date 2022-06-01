check_boottest_args_plus <- function(object, R, param, sign_level, B, clustid = NULL, fe = NULL) {

  # # check if lm / feols / felm / ivreg use illegal function args
  # if (inherits(object, "lm")) {
  #   # throw error if specific function arguments are used in lm() call
  #   call_object <- names(object$call)[names(object$call) != ""]
  #   banned_fun_args <- c("contrasts", "subset", "offset", "x", "y")
  #   if (sum(call_object %in% banned_fun_args) > 0) {
  #     stop(paste(
  #       "boottest.lm currently does not accept objects of type lm with
  #     function arguments",
  #       paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
  #       "and", banned_fun_args[length(banned_fun_args)], "."
  #     ),
  #     call. = FALSE
  #     )
  #   }
  # }
  # 
  # if (inherits(object, "fixest")) {
  #   # throw error if specific function arguments are used in feols() call
  #   call_object <- names(object$call)[names(object$call) != ""]
  #   banned_fun_args <- c(
  #     "offset", "subset", "split", "fsplit", "panel.id",
  #     "demeaned"
  #   )
  #   if (sum(call_object %in% banned_fun_args) > 0) {
  #     stop("boottest.fixest currently does not accept objects of type
  #         fixest with function arguments
  #         offset, subset, split, fsplit, panel.id & demeaned.",
  #       call. = FALSE
  #     )
  #   }
  # 
  #   # check for forbidden fixest syntax sugar (can't handle this at the moment)
  #   deparse_fml <- Reduce(paste, as.character(as.formula(object$fml_all$linear)))
  # 
  #   if (
  #     grepl("[", deparse_fml, fixed = TRUE) ||
  #     grepl("i(", deparse_fml, fixed = TRUE) ||
  #     grepl("c(", deparse_fml, fixed = TRUE) ||
  #     # '^' illegal in fixef argument, but legal in main formula - 
  #     # e.g. fml = y ~ x1 + I(x2^2) shold be possible
  #     ("fixef_vars" %in% names(object) && 
  #      grepl("^",
  #            Reduce(paste, as.character(as.formula(object$fml_all$fixef))),
  #            fixed = TRUE))
  #   # note: whitespace ~ - for IV
  #   # grepl("~", deparse_fml, fixed = TRUE)
  #   ) {
  #     stop("Advanced formula notation in fixest / fixest (i(), ^, [x]
  #        and vectorized formulas via c(),) is currently not supported
  #        in boottest().")
  #   }
  # 
  #   if (!is.null(fe) && fe %in% c(clustid, param)) {
  #     stop(paste("The function argument fe =", fe, "is included in either
  #              the clustering variables or the the hypothesis (via the `param` argument). This is not allowed. Please
  #              set fe to another factor variable or NULL."),
  #       call. = FALSE
  #     )
  #   }
  # }
  # 
  # 
  # if (inherits(object, "felm")) {
  #   # throw error if specific function arguments are used in felm() call
  #   call_object <- names(object$call)[names(object$call) != ""]
  #   banned_fun_args <- c("contrasts", "subset")
  #   if (sum(call_object %in% banned_fun_args) > 0) {
  #     stop(paste(
  #       "boottest.felm currently does not accept objects of type fixest with
  #     function arguments",
  #       paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
  #       "and", banned_fun_args[length(banned_fun_args)], "."
  #     ),
  #     call. = FALSE
  #     )
  #   }
  # 
  #   if (!is.null(fe) && fe %in% c(clustid, param)) {
  #     stop(paste("The function argument fe =", fe, "is included in either
  #              the clustering variables or the the hypothesis (via the `param` argument). This is not allowed. Please
  #              set fe to another factor variable or NULL."),
  #       call. = FALSE
  #     )
  #   }
  # }
  # 
  if (inherits(object, "ivreg")) {
    if (object$method != "OLS") {
      stop("Currently, only 2SLS is supported. Please set the `ivreg` function argument `method` to `OLS`.")
    }
  }
  # 
  #   # throw error if specific function arguments are used in lm() call
  #   call_object <- names(object$call)[names(object$call) != ""]
  #   banned_fun_args <- c("contrasts", "subset", "offset", "instruments")
  #   if (sum(call_object %in% banned_fun_args) > 0) {
  #     stop(paste(
  #       "boottest.ivreg currently does not accept objects of type lm with
  #     function arguments",
  #       paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
  #       "and", banned_fun_args[length(banned_fun_args)], "."
  #     ),
  #     call. = FALSE
  #     )
  #   }
  # }


  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(paste("Note: The bootstrap usually performs best when the
                  confidence level (here,", 1 - sign_level, "%)
                  times the number of replications plus 1
                  (", B, "+ 1 = ", B + 1, ") is an integer."))
  }
  
  if (inherits(object, "felm")) {
    if(!is.null(fe)){
      if(!(fe %in% names(object$fe))){
        stop(paste("The fixed effect to be projected out in the bootstrap,", fe, "is not included as a dedicated fixed effect in the estimated model."))
      } 
    }
  }
  
  if (inherits(object, "fixest")) {
    if(!is.null(fe)){
      if(!(fe %in% object$fixef_vars)){
        stop(paste("The fixed effect to be projected out in the bootstrap,", fe, "is not included as a dedicated fixed effect in the estimated model."))
      } 
    }
  }
  
}


check_mboottest_args_plus <- function(object, R, r, B) {

  # # check if lm / feols / felm / ivreg use illegal function args
  # if (inherits(object, "lm")) {
  #   # throw error if specific function arguments are used in lm() call
  #   call_object <- names(object$call)[names(object$call) != ""]
  #   banned_fun_args <- c("contrasts", "subset", "offset", "x", "y")
  #   if (sum(call_object %in% banned_fun_args) > 0) {
  #     stop(paste(
  #       "boottest.lm currently does not accept objects of type lm with
  #     function arguments",
  #       paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
  #       "and", banned_fun_args[length(banned_fun_args)], "."
  #     ),
  #     call. = FALSE
  #     )
  #   }
  # }
  # 
  # if (inherits(object, "fixest")) {
  #   # throw error if specific function arguments are used in feols() call
  #   call_object <- names(object$call)[names(object$call) != ""]
  #   banned_fun_args <- c(
  #     "offset", "subset", "split", "fsplit", "panel.id",
  #     "demeaned"
  #   )
  #   if (sum(call_object %in% banned_fun_args) > 0) {
  #     stop("boottest.fixest currently does not accept objects of type
  #         fixest with function arguments
  #         offset, subset, split, fsplit, panel.id & demeaned.",
  #       call. = FALSE
  #     )
  #   }
  # 
  #   # check for forbidden fixest syntax sugar (can't handle this at the moment)
  #   deparse_fml <- Reduce(paste, as.character(as.formula(object$fml_all$linear)))
  # 
  #   if (
  #     grepl("[", deparse_fml, fixed = TRUE) ||
  #     grepl("i(", deparse_fml, fixed = TRUE) ||
  #     grepl("c(", deparse_fml, fixed = TRUE) ||
  #     # '^' illegal in fixef argument, but legal in main formula - 
  #     # e.g. fml = y ~ x1 + I(x2^2) shold be possible
  #     ("fixef_vars" %in% names(object) && 
  #       grepl("^",
  #             Reduce(paste, as.character(as.formula(object$fml_all$fixef))),
  #             fixed = TRUE))
  #     # note: whitespace ~ - for IV
  #     # grepl("~", deparse_fml, fixed = TRUE)
  #   ) {
  #     stop("Advanced formula notation in fixest / fixest (i(), ^, [x]
  #        and vectorized formulas via c(),) is currently not supported
  #        in mboottest().")
  #   }
  # }
  # 
  # 
  # if (inherits(object, "felm")) {
  #   # throw error if specific function arguments are used in felm() call
  #   call_object <- names(object$call)[names(object$call) != ""]
  #   banned_fun_args <- c("contrasts", "subset")
  #   if (sum(call_object %in% banned_fun_args) > 0) {
  #     stop(paste(
  #       "boottest.felm currently does not accept objects of type felm with
  #     function arguments",
  #       paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
  #       "and", banned_fun_args[length(banned_fun_args)], "."
  #     ),
  #     call. = FALSE
  #     )
  #   }
  # }
  # 
  # if (inherits(object, "ivreg")) {
  #   if (object$method != "OLS") {
  #     stop("Currently, only 2SLS is supported. Please set the function argument method to `OLS`.")
  #   }
  # 
  #   # throw error if specific function arguments are used in lm() call
  #   call_object <- names(object$call)[names(object$call) != ""]
  #   banned_fun_args <- c("contrasts", "subset", "offset", "instruments")
  #   if (sum(call_object %in% banned_fun_args) > 0) {
  #     stop(paste(
  #       "boottest.ivreg currently does not accept objects of type ivreg with
  #     function arguments",
  #       paste0(banned_fun_args[1:(length(banned_fun_args) - 1)], collapse = ", "),
  #       "and", banned_fun_args[length(banned_fun_args)], "."
  #     ),
  #     call. = FALSE
  #     )
  #   }
  # }

  if (nrow(R) != length(r)) {
    stop(paste("The dimensions of func args R and r do not match. The number of rows of R is ", nrow(R), ", but the length of r is", length(r), "."))
  }
}
