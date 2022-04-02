#' Fast wild cluster bootstrap inference for object of class lm
#'
#' `boottest.ivreg` is a S3 method that allows for fast wild cluster
#' bootstrap inference for objects of class ivreg by  implementing
#' the fast wild bootstrap algorithm developed in Roodman et al., 2019
#' for instrumental variable models (WRE, Davidson & McKinnon, 2010)
#'
#' @param object An object of class lm
#' @param clustid A character vector containing the names of the cluster variables
#' @param param A character vector of length one. The name of the regression
#'        coefficient for which the hypothesis is to be tested
#' @param B Integer. The number of bootstrap iterations. When the number of clusters is low,
#'        increasing B adds little additional runtime
#' @param bootcluster A character vector. Specifies the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the intersections of
#'        clustering implied by the listed variables. To mimic the behavior of stata's boottest command,
#'        the default is to cluster by the intersection of all the variables specified via the `clustid` argument,
#'        even though that is not necessarily recommended (see the paper by Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by the cluster variable with the fewest clusters.
#' @param sign_level A numeric between 0 and 1 which sets the significance level
#'        of the inference procedure. E.g. sign_level = 0.05
#'        returns 0.95% confidence intervals. By default, sign_level = 0.05.
#' @param conf_int A logical vector. If TRUE, boottest computes confidence
#'        intervals by p-value inversion. If FALSE, only the p-value is returned.
#' @param seed An integer. Controls the random number generation, which is handled via the `StableRNG()` function from the `StableRNGs` Julia package.
#' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
#' @param r A numeric. Shifts the null hypothesis
#'        H0: param = r vs H1: param != r
#' @param beta0 Superseded function argument, replaced by function argument 'r'
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm", "gamma"
#'        and "webb". Alternatively, type can be a function(n) for drawing
#'        wild bootstrap factors. "rademacher" by default.
#'        For the Rademacher and Mammen distribution, if the number of replications B exceeds
#'        the number of possible draw ombinations, 2^(#number of clusters), then `boottest()`
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value.
#'        By default "two-tailed". Other options include "equal-tailed", ">" and "<".
#' @param tol Numeric vector of length 1. The desired accuracy
#'        (convergence tolerance) used in the root finding procedure to find the confidence interval.
#'        Relative tolerance of 1e-6 by default.
#' @param na_omit Logical. If TRUE, `boottest()` omits rows with missing
#'        variables in the cluster variable that have not previously been deleted
#'        when fitting the regression object (e.g. if the cluster variable was not used
#'        when fitting the regression model).
#' @param floattype Float64 by default. Other option: Float32. Should floating point numbers in Julia be represented as 32 or 64 bit?
#' @param fweights Logical. FALSE by default, TRUE for frequency weights.
#' @param getauxweights Logical. FALSE by default. Whether to save auxilliary weight matrix (v)
#' @param t_boot Logical. Should bootstrapped t-statistics be returned?
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set the maximum size of auxilliary weight matrix (v), in gigabytes
#' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request bootstrap-c instead of bootstrap-t
#' @param LIML Logical scalar. False by default. TRUE for LIML or Fuller LIML
#' @param Fuller NULL by default. Numeric scalar. Fuller LIML factor
#' @param kappa Null by default. fixed <U+03BA> for k-class estimation
#' @param ARubin False by default. Logical scalar. TRUE for Anderson-Rubin Test.
#' @param ssc An object of class `boot_ssc.type` obtained with the function \code{\link[fwildclusterboot]{boot_ssc}}. Represents how the small sample adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none", cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`. The function is purposefully designed to mimic fixest's \code{\link[fixest]{ssc}} function.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom dreamerr check_arg validate_dots
#'
#' @method boottest ivreg
#'
#' @return An object of class \code{boottest}
#'
#' \item{p_val}{The bootstrap p-value.}
#' \item{conf_int}{The bootstrap confidence interval.}
#' \item{param}{The tested parameter.}
#' \item{N}{Sample size. Might differ from the regression sample size if the
#'          cluster variables contain NA values.}
#' \item{B}{Number of Bootstrap Iterations.}
#' \item{clustid}{Names of the cluster Variables.}
#' \item{N_G}{Dimension of the cluster variables as used in boottest.}
#' \item{sign_level}{Significance level used in boottest.}
#' \item{type}{Distribution of the bootstrap weights.}
#' \item{t_stat}{The original test statistics - either imposing the null or not - with small sample correction `G / (G-1)`.}
#' \item{test_vals}{All t-statistics calculated while calculating the
#'       confidence interval.}
#'  \item{t_boot}{All bootstrap t-statistics.}
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}
#' \item{getauxweights}{The bootstrap auxiliary weights matrix v. Only returned if getauxweights = TRUE.}
#' \item{t_boot}{The bootstrapped t-statistics. Only returned if t_boot = TRUE.}
#'
#' @export
#'
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#'             STATA using boottest", The STATA Journal.
#'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. "Bootstrap-based improvements for inference with clustered errors." The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references MacKinnon, James G., and Matthew D. Webb. "The wild bootstrap for few (treated) clusters." The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals." L'Actualite economique 91.1-2 (2015): 11-33.
#' @references Webb, Matthew D. Reworking wild bootstrap based inference for clustered errors. No. 1315. Queen's Economics Department Working Paper, 2013.
#' @examples
#' \dontrun{
#' library(ivreg)
#' library(fwildclusterboot)
#'
#' # drop all NA values from SchoolingReturns
#' SchoolingReturns <- SchoolingReturns[rowMeans(sapply(SchoolingReturns, is.na)) == 0, ]
#' ivreg_fit <- ivreg(log(wage) ~ education + age +
#'   ethnicity + smsa + south + parents14 |
#'   nearcollege + age + ethnicity + smsa
#'     + south + parents14,
#' data = SchoolingReturns
#' )
#'
#' boot_ivreg <- boottest(
#'   object = ivreg_fit,
#'   B = 999,
#'   param = "education",
#'   clustid = "kww",
#'   type = "mammen",
#'   impose_null = TRUE
#' )
#' summary(boot_ivreg)
#' }
#'
boottest.ivreg <- function(object,
                           clustid,
                           param,
                           B,
                           bootcluster = "max",
                           conf_int = TRUE,
                           seed = NULL,
                           R = NULL,
                           r = 0,
                           beta0 = r,
                           sign_level = 0.05,
                           type = "rademacher",
                           impose_null = TRUE,
                           p_val_type = "two-tailed",
                           tol = 1e-6,
                           na_omit = TRUE,
                           floattype = "Float64",
                           fweights = FALSE,
                           getauxweights = FALSE,
                           t_boot = FALSE,
                           maxmatsize = NULL,
                           bootstrapc = FALSE,
                           LIML = FALSE,
                           Fuller = NULL,
                           kappa = NULL,
                           ARubin = FALSE,
                           ssc = boot_ssc(
                             adj = TRUE,
                             fixef.K = "none",
                             cluster.adj = TRUE,
                             cluster.df = "conventional"
                           ),
                           ...) {

  # check inputs
  call <- match.call()
  dreamerr::validate_dots(stop = TRUE)

  check_arg(object, "MBT class(ivreg)")
  check_arg(clustid, "MBT character scalar | character vector")
  check_arg(param, "MBT scalar character | character vector")
  check_arg(B, "MBT scalar integer GT{0}")
  check_arg(sign_level, "scalar numeric GT{0} LT{1}")
  check_arg(type, "charin(rademacher, mammen, norm, gamma, webb)")
  check_arg(conf_int, "logical scalar ")
  check_arg(seed, "scalar integer | NULL")
  check_arg(R, "NULL| scalar numeric | numeric vector")
  check_arg(r, "numeric scalar | NULL")
  check_arg(bootcluster, "character vector")
  check_arg(tol, "numeric scalar GT{0}")
  check_arg(floattype, "charin(Float32, Float64)")
  check_arg(fweights, "scalar logical")
  check_arg(t_boot, "scalar logical")
  check_arg(getauxweights, "scalar logical")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")
  # IV specific arguments
  check_arg(LIML, "scalar logical")
  check_arg(Fuller, "NULL | scalar numeric")
  check_arg(kappa, "NULL | scalar numeric")
  check_arg(ARubin, "scalar logical")
  check_arg(p_val_type, "charin(two-tailed, equal-tailed,>, <)")
  check_arg(boot_ssc, "class(ssc) | class(boot_ssc)")

  # set random seed

  if (is.null(seed)) {
    internal_seed <- get_seed()
  } else {
    set.seed(seed)
    internal_seed <- get_seed()
  }

  JuliaConnectoR::juliaEval("using Random")
  # JuliaConnectoR::juliaEval('using StableRNGs')
  # JuliaConnectoR::juliaEval(paste0("rng = StableRNG(",internal_seed,")"))
  rng_char <- paste0("Random.seed!(", internal_seed, ")")
  JuliaConnectoR::juliaEval(rng_char)
  internal_seed <- JuliaConnectoR::juliaEval(paste0("Random.MersenneTwister(", as.integer(internal_seed), ")"))

  # translate ssc into small_sample_adjustment
  if (ssc[["adj"]] == TRUE && ssc[["cluster.adj"]] == TRUE) {
    small_sample_adjustment <- TRUE
  } else {
    small_sample_adjustment <- FALSE
  }

  if (ssc[["fixef.K"]] != "none" || ssc[["cluster.df"]] != "conventional") {
    message(paste("Currently, boottest() only supports fixef.K = 'none' and cluster.df = 'conventional'."))
  }


  check_params_in_model(
    object = object,
    param = param
  )


  R <- process_R(
    R = R,
    param = param
  )


  check_boottest_args_plus(
    object = object,
    R = R,
    param = param,
    sign_level = sign_level,
    B = B
  )


  # preprocess data: X, Y, weights, fixed effects
  preprocess <- preprocess(
    object = object,
    cluster = clustid,
    fe = NULL,
    param = param,
    bootcluster = bootcluster,
    na_omit = na_omit,
    R = R,
    boot_algo = "WildBootTests.jl"
  )

  enumerate <-
    check_set_full_enumeration(
      preprocess = preprocess,
      B = B,
      type = type,
      boot_algo = "WildBootTests.jl"
    )

  full_enumeration <- enumerate$full_enumeration
  B <- enumerate$B

  point_estimate <- as.vector(object$coefficients[param] %*% preprocess$R0[param])

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
    small_sample_adjustment = small_sample_adjustment,
    p_val_type = p_val_type,
    type = type,
    floattype = floattype,
    bootstrapc = bootstrapc,
    # LIML = LIML,
    # ARubin = ARubin,
    getauxweights = getauxweights,
    internal_seed = internal_seed,
    maxmatsize = maxmatsize,
    fweights = 1L,
    small = small,
    clusteradj = clusteradj, 
    clustermin = clustermin,
    fe = NULL,
    fedfadj = NULL
  )

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
    boot_algo = "WildBootTests.jl"
  )

  class(res_final) <- "boottest"

  invisible(res_final)
}
