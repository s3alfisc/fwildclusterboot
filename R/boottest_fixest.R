#' Fast wild cluster bootstrap inference for object of class fixest
#'
#' `boottest.fixest` is a S3 method that allows for fast wild cluster
#' bootstrap inference for objects of class fixest by  implementing
#' the fast wild bootstrap algorithm developed in Roodman et al., 2019 and
#' implemented in the STATA package `boottest`.
#'
#' @param object An object of class fixest and estimated via `fixest::feols()`. Non-linear models are not supported.
#' @param clustid A character vector containing the names of the cluster variables. If NULL, 
#'        a heteroskedasticity-robust (HC1) wild bootstrap is run. 
#' @param param A character vector. The name of the regression
#'        coefficient(s) for which the hypothesis is to be tested
#' @param B Integer. The number of bootstrap iterations. When the number of clusters is low,
#'        increasing B adds little additional runtime.
#' @param bootcluster A character vector. Specifies the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the intersections of
#'        clustering implied by the listed variables. To mimic the behavior of stata's boottest command,
#'        the default is to cluster by the intersection of all the variables specified via the `clustid` argument,
#'        even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.
#'        Further, the subcluster bootstrap (MacKinnon & Webb, 2018) is supported - see the \code{vignette("fwildclusterboot", package = "fwildclusterboot")} for details.
#' @param fe A character vector of length one which contains the name of the fixed effect to be projected
#'        out in the bootstrap. Note: if regression weights are used, fe
#'        needs to be NULL.
#' @param sign_level A numeric between 0 and 1 which sets the significance level
#'        of the inference procedure. E.g. sign_level = 0.05
#'        returns 0.95% confidence intervals. By default, sign_level = 0.05.
#' @param conf_int A logical vector. If TRUE, boottest computes confidence
#'        intervals by test inversion. If FALSE, only the p-value is returned.
#' @param boot_algo Character scalar. Either "R" or "WildBootTests.jl". Controls the algorithm employed by boottest().
#'                  "R" is the default and implements the cluster bootstrap as in Roodman (2019). "WildBootTests.jl" executes the wild cluster bootstrap via the WildBootTests.jl
#'                  package. For it to run, Julia and WildBootTests.jl need to be installed. 
#                   The "R-lean" algorithm is a memory friendly, but less performant rcpp-armadillo based implementation of the wild cluster bootstrap.
#'                  Note that if no cluster is provided, boottest() always defaults to the "lean" algorithm. You can set the employed algorithm globally by using the
#'                  `setBoottest_boot_algo()` function.
#' @param seed An integer. Allows to set a random seed. For details, see below.  
#' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
#' @param r A numeric. Shifts the null hypothesis
#'        H0: param = r vs H1: param != r
#' @param beta0 Superseded function argument, replaced by function argument 'r'
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
#' @param ssc An object of class `boot_ssc.type` obtained with the function \code{\link[fwildclusterboot]{boot_ssc}}. Represents how the small sample adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none", cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`. The function is purposefully designed to mimic fixest's \code{\link[fixest]{ssc}} function.
#' @param getauxweights Logical. Whether to save auxilliary weight matrix (v)
#' @param floattype Float64 by default. Other option: Float32. Should floating point numbers in Julia be represented as 32 or 64 bit? Only relevant when 'boot_algo = "WildBootTests.jl"'
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set the maximum size of auxilliary weight matrix (v), in gigabytes. Only relevant when 'boot_algo = "WildBootTests.jl"'
#' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request bootstrap-c instead of bootstrap-t. Only relevant when 'boot_algo = "WildBootTests.jl"'
#' @param t_boot Logical. Should bootstrapped t-statistics be returned?
#' @param ... Further arguments passed to or from other methods.
#' @import JuliaConnectoR
#' @importFrom dreamerr check_arg validate_dots

#' @return An object of class \code{boottest}
#'
#' \item{p_val}{The bootstrap p-value.}
#' \item{conf_int}{The bootstrap confidence interval.}
#' \item{param}{The tested parameter.}
#' \item{N}{Sample size. Might differ from the regression sample size if
#'      the cluster variables contain NA values.}
#' \item{boot_iter}{Number of Bootstrap Iterations.}
#' \item{clustid}{Names of the cluster Variables.}
#' \item{N_G}{Dimension of the cluster variables as used in boottest.}
#' \item{sign_level}{Significance level used in boottest.}
#' \item{type}{Distribution of the bootstrap weights.}
#' \item{impose_null}{Whether the null was imposed on the bootstrap dgp or not.}
#' \item{R}{The vector "R" in the null hypothesis of interest Rbeta = r.}
#' \item{r}{The scalar "r" in the null hypothesis of interest Rbeta = r.}
#' \item{point_estimate}{R'beta. A scalar: the constraints vector times the regression coefficients.}
#' \item{grid_vals}{All t-statistics calculated while calculating the
#'       confidence interval.}
#' \item{p_grid_vals}{All p-values calculated while calculating the confidence
#'      interval.}
#' \item{t_stat}{The 'original' regression test statistics.}
#'  \item{t_boot}{All bootstrap t-statistics.}
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}
#' \item{boot_algo}{The employed bootstrap algorithm.}
#' \item{nthreads}{The number of threads employed.}
#' \item{internal_seed}{The integer value -inherited from set.seed() - used within boottest() to set the random seed in either R or Julia. If NULL, no internal seed was created.}
#' 
#' @export
#' @method boottest fixest
#' 
#' @section Setting Seeds: 
#' To guarantee reproducibility, you can either use `boottest()'s` `seed` function argument, or 
#' set a global random seed via 
#' + `set.seed()` when using
#'    1) the lean algorithm (via `boot_algo = "R-lean"`), 2) the heteroskedastic wild bootstrap 
#'    3) the wild cluster bootstrap via `boot_algo = "R"` with Mammen weights or 4) `boot_algo = "WildBootTests.jl"`
#' + `dqrng::dqset.seed()` when using `boot_algo = "R"` for Rademacher, Webb or Normal weights
#' 
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
#' \dontrun{
#' if (requireNamespace("fixest")) {
#'   library(fwildclusterboot)
#'   library(fixest)
#'   data(voters)
#'   feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income,
#'     fixef = "Q1_immigration",
#'     data = voters
#'   )
#'   boot1 <- boottest(feols_fit,
#'     B = 9999,
#'     param = "treatment",
#'     clustid = "group_id1"
#'   )
#'   boot2 <- boottest(feols_fit,
#'     B = 9999,
#'     param = "treatment",
#'     clustid = c("group_id1", "group_id2")
#'   )
#'   boot3 <- boottest(feols_fit,
#'     B = 9999,
#'     param = "treatment",
#'     clustid = c("group_id1", "group_id2"),
#'     fe = "Q1_immigration"
#'   )
#'   boot4 <- boottest(feols_fit,
#'     B = 9999,
#'     param = "treatment",
#'     clustid = c("group_id1", "group_id2"),
#'     fe = "Q1_immigration",
#'     sign_level = 0.2,
#'     seed = 8,
#'     r = 2
#'   )
#'   # test treatment + ideology1 = 2
#'   boot5 <- boottest(feols_fit,
#'     B = 9999,
#'     clustid = c("group_id1", "group_id2"),
#'     param = c("treatment", "ideology1"),
#'     R = c(1, 1),
#'     r = 2
#'   )
#'   summary(boot1)
#'   plot(boot1)
#' }
#' }
#'
boottest.fixest <- function(object,
                            param,
                            B,
                            clustid = NULL,
                            bootcluster = "max",
                            fe = NULL,
                            sign_level = 0.05,
                            conf_int = TRUE,
                            seed = NULL,
                            R = NULL,
                            r = 0,
                            beta0 = r,
                            type = "rademacher",
                            impose_null = TRUE,
                            p_val_type = "two-tailed",
                            tol = 1e-6,
                            maxiter = 10,
                            na_omit = TRUE,
                            nthreads = getBoottest_nthreads(),
                            ssc = boot_ssc(
                              adj = TRUE,
                              fixef.K = "none",
                              cluster.adj = TRUE,
                              cluster.df = "conventional"
                            ),
                            boot_algo = getBoottest_boot_algo(),
                            floattype = "Float64",
                            maxmatsize = FALSE,
                            bootstrapc = FALSE,
                            t_boot = FALSE,
                            getauxweights = FALSE,
                            ...) {
  call <- match.call()

  dreamerr::validate_dots(stop = TRUE)

  # Step 1: check arguments of feols call
  check_arg(object, "MBT class(fixest)")
  check_arg(clustid, "NULL | character scalar | character vector")
  check_arg(param, "MBT scalar character | character vector")
  check_arg(B, "MBT scalar integer GT{99}")
  check_arg(sign_level, "scalar numeric GT{0} LT{1}")
  check_arg(type, "charin(rademacher, mammen, norm, gamma, webb)")
  check_arg(p_val_type, "charin(two-tailed, equal-tailed,>, <)")

  check_arg(conf_int, "logical scalar")
  check_arg(seed, "scalar integer | NULL")
  check_arg(R, "NULL| scalar numeric | numeric vector")
  check_arg(r, "numeric scalar | NULL")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL")
  check_arg(bootcluster, "character vector")
  check_arg(tol, "numeric scalar GT{0}")
  check_arg(maxiter, "scalar integer GT{5}")
  check_arg(boot_ssc, "class(ssc) | class(boot_ssc)")
  check_arg(boot_algo, "charin(R, R-lean, WildBootTests.jl)")

  check_arg(floattype, "charin(Float32, Float64)")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")

  if (missing(r) & !missing(beta0)) {
    warning("Note that the 'beta0' function argument is superseded by a new argument, 'r'. Please specify your hypothesis via the new function argument instead of using 'beta0'.")
  }

  
  internal_seed <- set_seed(
    seed = seed, 
    boot_algo = boot_algo, 
    type = type
  )


  # fixest specific checks
  if (object$method != "feols") {
    stop("boottest() only supports OLS estimation via fixest::feols() - it does not support non-linear models computed via e.g. fixest::fepois() or fixest::feglm.")
  }

  if (!is.null(object$fixef_removed)) {
    stop(paste("feols() removes fixed effects with the following values: ", object$fixef_removed, ". Currently, boottest()'s internal pre-processing does not account for this deletion. Therefore, please exclude such fixed effects prior to estimation with feols(). You can find them listed under '$fixef_removed' of your fixest object."))
  }

  # --------------------------------------------

  # check appropriateness of nthreads
  nthreads <- check_set_nthreads(nthreads)

  if (is.null(clustid)) {
    heteroskedastic <- TRUE
    if (boot_algo == "R") {
      # heteroskedastic models should always be run through R-lean
      boot_algo <- "R-lean"
    }
  } else {
    heteroskedastic <- FALSE
  }


  R <- process_R(
    R = R,
    param = param
  )


  if (boot_algo != "WildBootTests.jl") {
    r_algo_checks(
      R = R,
      p_val_type = p_val_type,
      conf_int = conf_int,
      B = B
    )
  }

  check_params_in_model(object = object, param = param)

  check_boottest_args_plus(
    object = object,
    R = R,
    param = param,
    sign_level = sign_level,
    B = B,
    clustid = clustid,
    fe = fe
  )

  # preprocess the data: Y, X, weights, fixed_effect
  preprocess <- preprocess(
    object = object,
    cluster = clustid,
    fe = fe,
    param = param,
    bootcluster = bootcluster,
    na_omit = na_omit,
    R = R,
    boot_algo = boot_algo
  )

  enumerate <-
    check_set_full_enumeration(
      preprocess = preprocess,
      heteroskedastic = heteroskedastic,
      B = B,
      type = type,
      boot_algo = boot_algo
    )
  full_enumeration <- enumerate$full_enumeration
  B <- enumerate$B

  N <- preprocess$N
  k <- length(coef(object))
  G <- vapply(preprocess$clustid, function(x) length(unique(x)), numeric(1))
  vcov_sign <- preprocess$vcov_sign
  
  small_sample_correction <- get_ssc(boot_ssc_object = ssc, N = N, k = k, G = G, vcov_sign = vcov_sign, heteroskedastic = heteroskedastic)
  
  #clustermin, clusteradj
  
  
  clustid_dims <- preprocess$clustid_dims
  # R*beta;
  point_estimate <- as.vector(object$coefficients[param] %*% preprocess$R0[param])


  if (boot_algo == "R") {
    res <- boot_algo2(
      preprocessed_object = preprocess,
      boot_iter = B,
      point_estimate = point_estimate,
      impose_null = impose_null,
      r = r,
      sign_level = sign_level,
      param = param,
      # seed = seed,
      p_val_type = p_val_type,
      nthreads = nthreads,
      type = type,
      full_enumeration = full_enumeration,
      small_sample_correction = small_sample_correction,
      conf_int = conf_int,
      maxiter = maxiter,
      tol = tol
    )
  } else if (boot_algo == "R-lean") {
    
    check_r_lean(
      weights = stats::weights(object),
      clustid = clustid, 
      fe = fe
    )
    
    res <- boot_algo1(
      preprocessed_object = preprocess,
      boot_iter = B,
      point_estimate = point_estimate,
      impose_null = impose_null,
      r = r,
      sign_level = sign_level,
      param = param,
      # seed = seed,
      p_val_type = p_val_type,
      nthreads = nthreads,
      type = type,
      full_enumeration = full_enumeration,
      small_sample_correction = small_sample_correction,
      heteroskedastic = heteroskedastic,
      seed = internal_seed
    )
    conf_int <- p_grid_vals <- grid_vals <- FALSE
  } else if (boot_algo == "WildBootTests.jl") {
    fedfadj <- 0L

    julia_ssc <- get_ssc_julia(ssc)
    small <- julia_ssc$small
    clusteradj <- julia_ssc$clusteradj
    clustermin <- julia_ssc$clustermin
    
    if (ssc[["fixef.K"]] != "none") {
      message(paste("Currently, boottest() only supports fixef.K = 'none'."))
    }

    res <- boot_algo_julia(
      preprocess = preprocess,
      impose_null = impose_null,
      r = r,
      B = B,
      bootcluster = bootcluster,
      clustid = clustid,
      sign_level = sign_level,
      conf_int = conf_int,
      tol = tol,
      p_val_type = p_val_type,
      type = type,
      floattype = floattype,
      bootstrapc = bootstrapc,
      # LIML = LIML,
      # ARubin = ARubin,
      getauxweights = getauxweights,
      internal_seed = internal_seed,
      maxmatsize = maxmatsize,
      # fweights = 1L,
      small = small,
      clusteradj = clusteradj, 
      clustermin = clustermin,
      fe = fe,
      fedfadj = fedfadj
    )
  }

  # collect results
  res_final <- list(
    point_estimate = point_estimate,
    p_val = res$p_val,
    conf_int = res$conf_int,
    p_grid_vals = res$p_grid_vals,
    grid_vals = res$grid_vals,
    t_stat = res$t_stat,
    t_boot = res$t_boot,
    # regression = res$object,
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
    r = r,
    boot_algo = boot_algo,
    nthreads = nthreads, 
    internal_seed = internal_seed
  )


  class(res_final) <- "boottest"
  invisible(res_final)
}