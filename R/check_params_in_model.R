check_params_in_model <- function(object, param) {

  # for lm and fixest
  if (inherits(object, "lm")) {
    if (mean(param %in% c(names(coef(object)))) != 1) {
      stop(paste("The parameter", param, "is not included in the estimated model.
               Maybe you are trying to test for an interaction parameter?
               To see all model parameter names, run names(coef(model))."))
    }
  }

  # for lm and fixest
  if (inherits(object, "fixest")) {
    if (mean(param %in% c(names(coef(object)))) != 1) {
      stop(paste("The parameter", param, "is not included in the estimated model.
               Maybe you are trying to test for an interaction parameter?
               To see all model parameter names, run names(coef(model))."))
    }
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
      stop(paste("The parameter", param, "is not included in the estimated model.
               Maybe you are trying to test for an interaction parameter?
               To see all model parameter names, run names(coef(model))."))
    }
  }
}
