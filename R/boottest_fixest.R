#' Fast wild cluster bootstrap inference for object of class fixest
#' 
#' `boottest.fixest` is a S3 method that allows for fast wild cluster 
#' bootstrap inference for objects of class fixest by  implementing
#' the fast wild bootstrap algorithm developed in Roodman et al., 2019 and 
#' implemented in the STATA package `boottest`. 
#' 
#' @param object An object of class fixest
#' @param clustid A character vector containing the names of the cluster variables
#' @param param A character vector. The name of the regression
#'        coefficients for which the hypothesis is to be tested
#' @param B Integer. The number of bootstrap iterations. When the number of clusters is low, 
#'        increasing B adds little additional runtime. 
#' @param bootcluster A character vector. Specifies the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the intersections of
#'        clustering implied by the listed variables. To mimic the behavior of stata's boottest command, 
#'        the default is to cluster by the intersection of all the variables specified via the `clustid` argument, 
#'        even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2). 
#'        Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.
#' @param fe A character vector of length one which contains the name of the fixed effect to be projected
#'        out in the bootstrap. Note: if regression weights are used, fe 
#'        needs to be NULL.
#' @param sign_level A numeric between 0 and 1 which sets the significance level 
#'        of the inference procedure. E.g. sign_level = 0.05 
#'        returns 0.95% confidence intervals. By default, sign_level = 0.05.
#' @param conf_int A logical vector. If TRUE, boottest computes confidence 
#'        intervals by p-value inversion. If FALSE, only the p-value is returned.
#' @param seed An integer. Allows the user to set a random seed. If you want to set a "global" seed, set it via `dqrng::dqset.seed()`.
#' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
#' @param beta0 A numeric. Shifts the null hypothesis 
#'        H0: param = beta0 vs H1: param != beta0
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm"
#'        and "webb". Alternatively, type can be a function(n) for drawing 
#'        wild bootstrap factors. "rademacher" by default.  
#'        For the Rademacher distribution, if the number of replications B exceeds 
#'        the number of possible draw ombinations, 2^(#number of clusters), then `boottest()` 
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value. 
#'        By default "two-tailed". Other options include "equal-tailed", ">" and "<".
#' @param tol Numeric vector of length 1. The desired accuracy 
#'        (convergence tolerance) used in the root finding procedure to find the confidence interval.
#'        1e-6 by default.
#' @param maxiter Integer. Maximum number of iterations used in the root finding procedure to find the confidence interval.
#'        10 by default.
#' @param na_omit Logical. If TRUE, `boottest()` omits rows with missing 
#'        variables in the cluster variable that have not previously been deleted
#'        when fitting the regression object (e.g. if the cluster variable was not used 
#'        when fitting the regression model).
#' @param nthreads The number of threads. Can be: a) an integer lower than, 
#'                 or equal to, the maximum number of threads; b) 0: meaning 
#'                 all available threads will be used; c) a number strictly
#'                 between 0 and 1 which represents the fraction of all threads 
#'                 to use. The default is to use 1 core.
#' @param ... Further arguments passed to or from other methods.
#' @importFrom dreamerr check_arg validate_dots

#' @return An object of class \code{boottest}
#' 
#' \item{p_val}{The bootstrap p-value.}
#' \item{conf_int}{The bootstrap confidence interval.}
#' \item{param}{The tested parameter.}
#' \item{N}{Sample size. Might differ from the regression sample size if
#'      the cluster variables contain NA values.}
#' \item{B}{Number of Bootstrap Iterations.}
#' \item{clustid}{Names of the cluster Variables.}
#' \item{N_G}{Dimension of the cluster variables as used in boottest.}
#' \item{sign_level}{Significance sign_level used in boottest.}
#' \item{type}{Distribution of the bootstrap weights.}
#' \item{p_test_vals}{All p-values calculated while calculating the 
#'       confidence interval.}
#' \item{t_stat}{The original test statistics - either imposing the null or not - with small sample correction `G / (G-1)`.}
#' \item{test_vals}{All t-statistics calculated while calculating the 
#'       confidence interval.}
#'  \item{t_boot}{All bootstrap t-statistics.}     
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}
#' @export
#' @method boottest fixest
#' @section Confidence Intervals:
#' \code{boottest} computes confidence intervals by inverting p-values. 
#'       In practice, the following procedure is used:
#' \itemize{
#' \item Based on an initial guess for starting values, calculate p-values 
#'       for 26 equal spaced points between the starting values.
#' \item Out of the 26 calculated p-values, find the two pairs of values x 
#'       for which the corresponding p-values px cross the significance 
#'       sign_level sign_level.
#' \item Feed the two pairs of x into an numerical root finding procedure 
#'       and solve for the root. boottest currently relies on
#'       \code{stats::uniroot} and sets an absolute tolerance of 1e-06 and
#'       stops the procedure after 10 iterations.
#' }
#' @section Standard Errors:
#' \code{boottest} does not calculate standard errors.
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in 
#'             STATA using boottest", The STATA Journal.
#'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. "Bootstrap-based improvements for inference with clustered errors." The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references MacKinnon, James G., and Matthew D. Webb. "The wild bootstrap for few (treated) clusters." The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals." L'Actualite economique 91.1-2 (2015): 11-33.             
#' @references Webb, Matthew D. Reworking wild bootstrap based inference for clustered errors. No. 1315. Queen's Economics Department Working Paper, 2013.

#' @examples
#' if(requireNamespace("fixest")){
#' library(fwildclusterboot)
#' library(fixest)
#' data(voters)
#' feols_fit <-feols(proposition_vote ~ treatment + ideology1 + log_income,
#'            fixef =  "Q1_immigration", 
#'            data = voters)
#' boot1 <- boottest(feols_fit, 
#'                   B = 9999, 
#'                   param = "treatment",
#'                   clustid = "group_id1")
#' boot2 <- boottest(feols_fit, 
#'                   B = 9999, 
#'                   param = "treatment", 
#'                   clustid = c("group_id1", "group_id2"))
#' boot3 <- boottest(feols_fit,
#'                   B = 9999,
#'                   param = "treatment", 
#'                   clustid = c("group_id1", "group_id2"),
#'                   fe = "Q1_immigration")
#' boot4 <- boottest(feols_fit, 
#'                   B = 9999, 
#'                   param = "treatment", 
#'                   clustid = c("group_id1", "group_id2"),
#'                   fe = "Q1_immigration", 
#'                   sign_level = 0.2, 
#'                   seed = 8,
#'                   beta0 = 2)
#' # test treatment + ideology1 = 2                   
#' boot5 <- boottest(feols_fit, 
#'                   B = 9999, 
#'                   clustid = c("group_id1", "group_id2"),
#'                   param = c("treatment", "ideology1"),
#'                   R = c(1, 1), 
#'                   beta0 = 2)
#' summary(boot1)
#' plot(boot1)
#' }

boottest.fixest <- function(object,
                            clustid,
                            param,
                            B,
                            bootcluster = "max",
                            fe = NULL,
                            sign_level = NULL,
                            conf_int = NULL,
                            seed = NULL,
                            R = NULL,
                            beta0 = 0,
                            type = "rademacher",
                            impose_null = TRUE,
                            p_val_type = "two-tailed",
                            tol = 1e-6, 
                            maxiter = 10,
                            na_omit = TRUE,
                            nthreads = getBoottest_nthreads(), 
                            ...) {
  
  
  call <- match.call()
  
  dreamerr::validate_dots(stop = TRUE)
  
  # Step 1: check arguments of feols call
  check_arg(clustid, "character vector | scalar character")
  check_arg(param, "scalar character | character vector")
  check_arg(B, "scalar integer ")
  check_arg(sign_level, "scalar numeric | NULL")
  check_arg(conf_int, "logical scalar | NULL")
  check_arg(seed, "scalar integer | NULL")
  check_arg(R, "NULL| scalar numeric | numeric vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL")
  check_arg(bootcluster, "character vector")
  check_arg(tol, "numeric scalar")
  check_arg(maxiter, "scalar integer")
  
  
  if(!is.null(object$fixef_removed)){
    stop(paste("feols() removes fixed effects with the following values: ", object$fixef_removed, ". Currently, boottest()'s internal pre-processing does not account for this deletion. Therefore, please exclude such fixed effects prior to estimation with feols(). You can find them listed under '$fixef_removed' of your fixest object."))
  }
  
  # check appropriateness of nthreads
  nthreads <- check_set_nthreads(nthreads)
  #cat("nthreads boottest \n")
  #print(nthreads)
  
  if(!is.null(seed)){
    dqrng::dqset.seed(seed)
  }
  
  if(maxiter < 1){
    stop("The function argument maxiter needs to be larger than 1.", 
         call. = FALSE)
  }
  
  if(tol < 0){
    stop("The function argument tol needs to be positive.", 
         call. = FALSE)
  }
  
  if(!(p_val_type %in% c("two-tailed", "equal-tailed",">", "<"))){
    stop("The function argument p_val_type must be
         'two-tailed', 'equal-tailed','>' or '<'.", 
         call. = FALSE)
  }
  
  if(p_val_type %in% c(">", "<") && conf_int == TRUE){
    conf_int <- FALSE
    warning(paste("Currently, boottest() does not calculate confidence intervals for one-sided hypotheses, but this will change in a future release."), call. = FALSE)
  }
  
  if ((conf_int == TRUE || is.null(conf_int)) & B <= 100) {
    stop("The function argument B is smaller than 100. The number of bootstrap 
          iterations needs to be 100 or higher in order to guarantee that the root
          finding procudure used to find the confidence set works properly.",
         call. = FALSE
    )
  }
  if (!is.null(sign_level) & (sign_level <= 0 || sign_level >= 1)) {
    stop("The function argument sign_level is outside of the unit interval
         (0, 1). Please specify sign_level so that it is within the 
         unit interval.",
         call. = FALSE
    )
  }
  
  if (is.null(sign_level)) {
    sign_level <- 0.05
  }
  
  if (mean(param %in% c(names(object$coefficients))) != 1) {
    stop(paste("The parameter", param, "is not included in the estimated model.
               Maybe you are trying to test for an interaction parameter? 
               To see all model parameter names, run names(coef(model))."))
  }
  # repeat the same: check if fe is in the data.frame
  
  if(is.null(R)){
    R <- rep(1, length(param))
  } else {
    if(length(R) != length(param)){
      stop("The constraints vector must either be NULL or a numeric of the same length as the `param` input vector.")
    }
  }
  
  if (!is.null(fe) && fe %in% c(clustid, param)) {
    stop(paste("The function argument fe =", fe, "is included in either 
               the clustering variables or the the hypothesis (via the `param` argument). This is not allowed. Please 
               set fe to another factor variable or NULL."),
         call. = FALSE
    )
  }
  
  if (((1 - sign_level) * (B + 1)) %% 1 != 0) {
    message(paste("Note: The bootstrap usually performs best when the
                  confidence sign_level (here,", 1 - sign_level, "%) 
                  times the number of replications plus 1 
                  (", B, "+ 1 = ", B + 1, ") is an integer."))
  }
  
  # throw error if specific function arguments are used in feols() call
  call_object <- names(object$call)[names(object$call) != ""]
  banned_fun_args <- c("offset", "subset", "split", "fsplit", "panel.id",
                       "demeaned")
  if (sum(call_object %in% banned_fun_args) > 0) {
    stop("boottest.fixest currently does not accept objects of type 
          fixest with function arguments 
          offset, subset, split, fsplit, panel.id & demeaned.",
         call. = FALSE
    )
  }
  
  
  deparse_fml <- Reduce(paste, as.character(as.formula(object$fml_all$linear)))
  if (grepl("[", deparse_fml, fixed = TRUE) ||
      grepl("i(", deparse_fml, fixed = TRUE) ||
      grepl("c(", deparse_fml, fixed = TRUE) ||
      grepl("^", deparse_fml, fixed = TRUE)
      # note: whitespace ~ - for IV
      # grepl("~", deparse_fml, fixed = TRUE)
  ) {
    stop("Advanced formula notation in fixest / fixest (i(), ^, [x] 
         and vectorized formulas via c(),) is currently not supported
         in boottest().")
  }
  
  # preprocess the data: Y, X, weights, fixed_effect
  preprocess <- preprocess2(object = object, 
                            cluster = clustid,
                            fe = fe, 
                            param = param,
                            bootcluster = bootcluster, 
                            na_omit = na_omit, 
                            R = R)
  
  clustid_dims <- preprocess$clustid_dims
  # R*beta; 
  point_estimate <- as.vector(object$coefficients[param] %*% preprocess$R0[param])
  
  # number of clusters used in bootstrap - always derived from bootcluster
  N_G <- length(unique(preprocess$bootcluster[, 1]))
  N_G_2 <- 2^N_G
  if (type %in% c("rademacher") & N_G_2 < B) {
    warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution for", length(unique(preprocess$bootcluster[, 1])), "clusters. Therefore, B = ", N_G_2, " with full enumeration. Consider using webb weights instead."),
            call. = FALSE, 
            noBreaks. = TRUE
    )
    warning(paste("Further, note that under full enumeration and with B =", N_G_2, "bootstrap draws, only 2^(#clusters - 1) = ", 2^(N_G - 1), " distinct t-statistics and p-values can be computed. For a more thorough discussion, see Webb `Reworking wild bootstrap based inference for clustered errors` (2013)."),
            call. = FALSE, 
            noBreaks. = TRUE
    )
    B <- N_G_2
    full_enumeration <- TRUE
  } else{
    full_enumeration <- FALSE
  }
  
  res <- boot_algo2(preprocessed_object = preprocess,
                    boot_iter = B,
                    point_estimate = point_estimate,
                    impose_null = impose_null,
                    beta0 = beta0,
                    sign_level = sign_level,
                    param = param,
                    # seed = seed,
                    p_val_type = p_val_type, 
                    nthreads = nthreads, 
                    type = type, 
                    full_enumeration = full_enumeration
  )
  
  # compute confidence sets
  if (is.null(conf_int) || conf_int == TRUE) {
    
    # guess for standard errors
    if(impose_null == TRUE){
      # should always be positive, point_estimate and t_stat need to have same
      # sign, abs for security
      se_guess <- abs(point_estimate / res$t_stat)
    } else if(impose_null == FALSE){
      se_guess <- abs((point_estimate - beta0) / res$t_stat)
    }
  
    
    res_p_val <- invert_p_val(
      object = res,
      boot_iter = B,
      point_estimate = point_estimate,
      se_guess = se_guess,
      clustid = preprocess$clustid,
      sign_level = sign_level,
      vcov_sign = preprocess$vcov_sign,
      impose_null = impose_null,
      p_val_type = p_val_type, 
      maxiter = maxiter, 
      tol = tol
    )
  } else {
    res_p_val <- list(
      conf_int = NA,
      p_test_vals = NA,
      test_vals = NA
    )
  }
  
  res_final <- list(
    point_estimate = point_estimate,
    p_val = res[["p_val"]],
    conf_int = res_p_val$conf_int,
    p_test_vals = res_p_val$p_grid_vals,
    test_vals = res_p_val$grid_vals,
    t_stat = res$t_stat,
    t_boot = res$t_boot,
    regression = res$object,
    param = param,
    N = preprocess$N,
    boot_iter = B,
    clustid = clustid,
    # depvar = depvar,
    N_G = preprocess$N_G,
    sign_level = sign_level,
    call = call,
    type = type,
    impose_null = impose_null, 
    R = R, 
    beta0 = beta0
  )
  
  class(res_final) <- "boottest"
  invisible(res_final)
}

