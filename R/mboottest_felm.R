#' Fast wild cluster bootstrap inference for joint hypotheses for object of
#' class felm
#'
#' `mboottest.felm` is a S3 method that allows for fast wild cluster
#' bootstrap inference of multivariate hypotheses for objects of class felm by
#' implementing the fast wild bootstrap algorithm developed in
#'  Roodman et al., 2019.
#'
#' @param object An object of class felm
#' @param clustid A character vector or rhs formula containing the names of
#' the cluster variables
#' @param B Integer. The number of bootstrap iterations. When the number of
#'  clusters is low,
#'        increasing B adds little additional runtime.
#' @param bootcluster A character vector or rhs formula of length 1. Specifies
#'  the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by
#'        the intersections of
#'        clustering implied by the listed variables. To mimic the behavior of
#'         stata's boottest command,
#'        the default is to cluster by the intersection of all the variables
#'        specified via the `clustid` argument,
#'        even though that is not necessarily recommended (see the paper by
#'        Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by the
#'        cluster variable with the fewest clusters.
#'        Further, the subcluster bootstrap (MacKinnon & Webb, 2018) is
#'        supported - see the `vignette("fwildclusterboot",
#'        package = "fwildclusterboot")` for details.
#' @param fe A character vector or rhs formula of length one which contains
#' the name of the fixed effect to be projected
#'        out in the bootstrap. Note: if regression weights are used, fe
#'        needs to be NULL.
#' @param R Hypothesis Vector or Matrix giving linear combinations of
#' coefficients. Must be either a vector of length k or a matrix of dimension
#'  q x k, where q is the number
#'        of joint hypotheses and k the number of estimated coefficients.
#' @param r A vector of length q, where q is the number of tested hypotheses.
#' Shifts the null hypothesis
#'        H0: param = r vs H1: param != r. If not provided, a vector of zeros
#'        of length q.
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm", "gamma"
#'        and "webb". Alternatively, type can be a function(n) for drawing
#'        wild bootstrap factors. "rademacher" by default.
#'        For the Rademacher and Mammen distribution, if the number of
#'        replications B exceeds
#'        the number of possible draw ombinations, 2^(#number of clusters),
#'         then `boottest()`
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value.
#'        By default "two-tailed". Other options include "equal-tailed", ">"
#'        and "<".
#' @param tol Numeric vector of length 1. The desired accuracy
#'        (convergence tolerance) used in the root finding procedure to find
#'         the confidence interval.
#'        Relative tolerance of 1e-6 by default.
#' @param floattype Float64 by default. Other option: Float32. Should floating
#'  point numbers in Julia be represented as 32 or 64 bit?
#' @param getauxweights Logical. FALSE by default. Whether to save auxilliary
#'  weight matrix (v)
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set
#' the maximum size of auxilliary weight matrix (v), in gigabytes
#' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request
#'  bootstrap-c instead of bootstrap-t
#' @param ssc An object of class `boot_ssc.type` obtained with the function
#'  [fwildclusterboot::boot_ssc()]. Represents how the small sample
#'  adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none",
#'   cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`.
#'             The function is purposefully designed to mimic fixest's
#'              [fixest::ssc()] function.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom dreamerr check_arg validate_dots
#' @importFrom stats as.formula coef model.matrix model.response model.weights
#' residuals rlnorm rnorm update
#'
#' @method mboottest felm
#'
#' @return An object of class `mboottest`
#'
#' \item{p_val}{The bootstrap p-value.}
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
#' \item{point_estimate}{R'beta. A scalar: the constraints vector times the
#' regression coefficients.}
#' \item{teststat_stat}{The 'original' regression test statistics.}
#' \item{teststat_boot}{All bootstrap t-statistics.}
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}

#' @export
#'
#' @section Setting Seeds:
#' To guarantee reproducibility, you need to
#' set a global random seed via `set.seed()` when using
#' @section Multiple Fixed Effects:
#' If your felm() model contains fixed effects, boottest() will internally convert
#'  all fixed effects but the one specified via the `fe` argument to dummy variables.
#' @section Run `boottest` quietly:
#' You can suppress all warning and error messages by setting the following global
#' options:
#' `options(rlib_warning_verbosity = "quiet")`
#' `options(rlib_message_verbosity = "quiet")`
#' Not that this will turn off all warnings (messages) produced via `rlang::warn()` and
#' `rlang::inform()`, which might not be desirable if you use other software build on
#' `rlang`, as e.g. the `tidyverse`.
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#' STATA using boottest", The STATA Journal.
#' (<https://ideas.repec.org/p/qed/wpaper/1406.html>)
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller.
#'  "Bootstrap-based improvements for inference with clustered errors."
#'  The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references Cameron, A.Colin & Douglas L. Miller.
#' "A practitioner's guide to cluster-robust inference"
#' Journal of Human Resources (2015) \doi{doi:10.3368/jhr.50.2.317}
#' @references Davidson & MacKinnon. "Wild Bootstrap Tests for IV regression"
#' Journal of Economics and Business Statistics (2010)
#' \doi{https://doi.org/10.1198/jbes.2009.07221}
#' @references MacKinnon, James G., and Matthew D. Webb.
#' "The wild bootstrap for few (treated) clusters."
#'  The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James G., and Matthew D. Webb.
#' "Cluster-robust inference: A guide to empirical practice"
#'  Journal of Econometrics (2022)
#'  \doi{https://doi.org/10.1016/j.jeconom.2022.04.001}
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals."
#'  L'Actualite economique 91.1-2 (2015): 11-33.
#' @references Webb, Matthew D.
#' "Reworking wild bootstrap based inference for clustered errors"
#' . No. 1315. Queen's Economics Department Working Paper, 2013.
#' @examples
#' \dontrun{
#' requireNamespace("lfe")
#' requireNamespace("clubSandwich")
#' R <- clubSandwich::constrain_zero(2:3, coef(lm_fit))
#' wboottest <-
#'   mboottest(
#'     object = lm_fit,
#'     clustid = "group_id1",
#'     B = 999,
#'     R = R
#'   )
#' summary(wboottest)
#' print(wboottest)
#' nobs(wboottest)
#' pval(wboottest)
#' generics::tidy(wboottest)
#' }
#'
mboottest.felm <- function(object,
                           clustid,
                           B,
                           R,
                           r = rep(0, nrow(R)),
                           bootcluster = "max",
                           fe = NULL,
                           type = "rademacher",
                           impose_null = TRUE,
                           p_val_type = "two-tailed",
                           tol = 1e-6,
                           floattype = "Float64",
                           getauxweights = FALSE,
                           maxmatsize = NULL,
                           bootstrapc = FALSE,
                           ssc = boot_ssc(
                             adj = TRUE,
                             fixef.K = "none",
                             cluster.adj = TRUE,
                             cluster.df = "conventional"
                           ),
                           ...) {
  call <- match.call()
  type <- tolower(type)
  dreamerr::validate_dots(stop = TRUE)

  # Step 1: check arguments of feols call
  check_arg(object, "MBT class(felm)")
  check_arg(clustid, "character scalar | character vector | formula")
  check_arg(B, "MBT scalar integer")
  check_arg(R, "MBT numeric matrix")
  check_arg(type, "charin(rademacher, mammen, norm, gamma, webb)")
  check_arg(p_val_type, "charin(two-tailed, equal-tailed,>, <)")
  check_arg(r, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL | formula")
  check_arg(bootcluster, "character vector | formula")
  check_arg(tol, "numeric scalar GT{0}")
  check_arg(boot_ssc, "class(ssc) | class(boot_ssc)")
  check_arg(floattype, "charin(Float32, Float64)")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")

  # remind packages users to set a global seed
  inform_seed(
    frequency_id = "seed-reminder-m-felm",
    engine = "WildBootTests.jl"
  )

  if (inherits(clustid, "formula")) {
    clustid <- attr(terms(clustid), "term.labels")
  }

  if (inherits(bootcluster, "formula")) {
    bootcluster <- attr(terms(bootcluster), "term.labels")
  }

  if (inherits(fe, "formula")) {
    fe <- attr(terms(fe), "term.labels")
  }

  check_mboottest_args_plus(
    object = object,
    R = R,
    r = r,
    fe = fe
  )

  fedfadj <- 0L

  preprocess <- preprocess2.felm(
    object = object,
    clustid = clustid,
    R = R,
    param = NULL,
    bootcluster = bootcluster,
    fe = fe,
    engine = "WildBootTests.jl",
    bootstrap_type = NULL
  )

  enumerate <-
    check_set_full_enumeration(
      preprocess = preprocess,
      B = B,
      type = type,
      engine = "WildBootTests.jl"
    )

  full_enumeration <- enumerate$full_enumeration
  B <- enumerate$B

  julia_ssc <- get_ssc_julia(ssc)
  small <- julia_ssc$small
  clusteradj <- julia_ssc$clusteradj
  clustermin <- julia_ssc$clustermin

  if (ssc[["fixef.K"]] != "none") {
    rlang::inform(
      "Currently, boottest() only supports fixef.K = 'none'.",
      use_cli_format = TRUE
      )
  }


  res <- boot_algo_julia(
    preprocess = preprocess,
    impose_null = impose_null,
    r = r,
    B = B,
    bootcluster = bootcluster,
    clustid = clustid,
    sign_level = NULL,
    conf_int = FALSE,
    tol = tol,
    p_val_type = p_val_type,
    type = type,
    floattype = floattype,
    bootstrapc = bootstrapc,
    # LIML = LIML,
    # ARubin = ARubin,
    getauxweights = getauxweights,
    maxmatsize = maxmatsize,
    # fweights = 1L,
    small = small,
    clusteradj = clusteradj,
    clustermin = clustermin,
    fe = fe,
    fedfadj = fedfadj
  )

  # collect results
  res_final <- list(
    p_val = res$p_val,
    teststat = res$t_stat,
    teststat_boot = res$t_boot,
    # regression = res$object,
    N = preprocess$N,
    boot_iter = B,
    clustid = clustid,
    # depvar = depvar,
    N_G = preprocess$N_G,
    call = call,
    type = type,
    impose_null = impose_null,
    R = R,
    r = r,
    engine = "WildBootTests.jl"
  )

  class(res_final) <- "mboottest"

  invisible(res_final)
}
