#' Fast wild cluster bootstrap inference of joint hypotheses for object of
#'  class lm
#'
#' `mboottest.lm` is a S3 method that allows for fast wild cluster
#' bootstrap inference of multivariate hypotheses for objects of class lm by
#' implementing the fast wild bootstrap algorithm developed in
#' Roodman et al., 2019.
#'
#' @param object An object of class lm
#' @param clustid A character vector or rhs formula containing the names of
#' the cluster variables
#' @param B Integer. The number of bootstrap iterations. When the number of
#' clusters is low,
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
#'         Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by the
#'        cluster variable with the fewest clusters.
#'        Further, the subcluster bootstrap (MacKinnon & Webb, 2018) is
#'        supported - see the \code{vignette("fwildclusterboot",
#'         package = "fwildclusterboot")} for details.
#' @param seed An integer. Allows to set a random seed. For details,
#'  see below.
#' @param R Hypothesis Vector or Matrix giving linear combinations of
#' coefficients. Must be either a vector of length k or a matrix of dimension
#' q x k, where q is the number
#'        of joint hypotheses and k the number of estimated coefficients.
#' @param r A vector of length q, where q is the number of tested hypotheses.
#'  Shifts the null hypothesis
#'        H0: param = r vs H1: param != r. If not provided, a vector of zeros
#'         of length q.
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm", "gamma"
#'        and "webb". Alternatively, type can be a function(n) for drawing
#'        wild bootstrap factors. "rademacher" by default.
#'        For the Rademacher and Mammen distribution, if the number of
#'        replications B exceeds
#'        the number of possible draw ombinations, 2^(#number of clusters),
#'        then `boottest()`
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value.
#'        By default "two-tailed". Other options include "equal-tailed", ">"
#'        and "<".
#' @param tol Numeric vector of length 1. The desired accuracy
#'        (convergence tolerance) used in the root finding procedure to find
#'        the confidence interval.
#'        Relative tolerance of 1e-6 by default.
#' @param floattype Float64 by default. Other option: Float32. Should floating
#'  point numbers in Julia be represented as 32 or 64 bit?
#' @param getauxweights Logical. FALSE by default. Whether to save auxilliary
#'  weight matrix (v)
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set
#'  the maximum size of auxilliary weight matrix (v), in gigabytes
#' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request
#' bootstrap-c instead of bootstrap-t
#' @param ssc An object of class `boot_ssc.type` obtained with the function
#'  \code{\link[fwildclusterboot]{boot_ssc}}. Represents how the small sample
#'   adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none",
#'    cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`.
#'             The function is purposefully designed to mimic fixest's
#'             \code{\link[fixest]{ssc}} function.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom dreamerr check_arg validate_dots
#' @importFrom stats as.formula coef model.matrix model.response model.weights
#' residuals rlnorm rnorm update
#'
#' @return An object of class \code{mboottest}
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
#' \item{boot_algo}{The employed bootstrap algorithm.}
#' \item{nthreads}{The number of threads employed.}
#' \item{internal_seed}{The integer value -inherited from set.seed() - used
#' within boottest() to set the random seed in either R or Julia. If NULL, no
#'  internal seed was created.}
#'
#' @export
#' @method mboottest lm
#'
#' @section Setting Seeds:
#' To guarantee reproducibility, you can either use `boottest()'s` `seed`
#'  function argument, or
#' set a global random seed via
#' + `set.seed()` when using
#'    1) the lean algorithm (via `boot_algo = "R-lean"`) including the
#'     heteroskedastic wild bootstrap
#'    2) the wild cluster bootstrap via `boot_algo = "R"` with Mammen weights or
#'    3) `boot_algo = "WildBootTests.jl"`
#' + `dqrng::dqset.seed()` when using `boot_algo = "R"` for Rademacher, Webb
#'  or Normal weights
#'
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#' STATA using boottest", The STATA Journal.
#' (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller.
#'  "Bootstrap-based improvements for inference with clustered errors."
#'  The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references Cameron, A.Colin & Douglas L. Miller.
#' "A practitioner's guide to cluster-robust inference"
#' Journal of Human Resources (2015) \doi{doi: 10.3368/jhr.50.2.317}
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
#' library(clubSandwich)
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
mboottest.lm <- function(object,
                         clustid,
                         B,
                         R,
                         r = rep(0, nrow(R)),
                         bootcluster = "max",
                         seed = NULL,
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
  dreamerr::validate_dots(stop = TRUE)

  check_arg(object, "MBT class(lm)")

  check_arg(clustid, "character scalar | character vector | formula")
  check_arg(B, "MBT scalar integer")
  check_arg(R, "MBT numeric matrix")

  check_arg(seed, "scalar integer | NULL")
  check_arg(r, "numeric vector  | NULL")
  check_arg(bootcluster, "character vector | formula")
  check_arg(tol, "numeric scalar")
  check_arg(floattype, "character scalar")
  check_arg(getauxweights, "scalar logical")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")
  check_arg(floattype, "charin(Float32, Float64")
  check_arg(p_val_type, "charin(two-tailed, equal-tailed,>, <)")
  check_arg(tol, "numeric scalar GT{0}")
  check_arg(teststat_boot, "logical scalar")

  if (inherits(clustid, "formula")) {
    clustid <- attr(terms(clustid), "term.labels")
  }

  if (inherits(bootcluster, "formula")) {
    bootcluster <- attr(terms(bootcluster), "term.labels")
  }

  internal_seed <- set_seed(
    seed = seed,
    boot_algo = "WildBootTests.jl",
    type = type
  )


  if (ssc[["fixef.K"]] != "none" ||
    ssc[["cluster.df"]] != "conventional") {
    message(
      paste(
        "Currently, boottest() only supports fixef.K = 'none'
        and cluster.df = 'conventional'."
      )
    )
  }

  check_mboottest_args_plus(
    object = object,
    R = R,
    r = r,
    fe = NULL
  )


  # preprocess data: X, Y, weights, fixed effects
  preprocess <- preprocess2.lm(
    object = object,
    clustid = clustid,
    R = R,
    param = NULL,
    bootcluster = bootcluster,
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
    internal_seed = internal_seed,
    maxmatsize = maxmatsize,
    # fweights = 1L,
    small = small,
    clusteradj = clusteradj,
    clustermin = clustermin,
    fe = NULL,
    fedfadj = 0L
  )

  # collect results
  res_final <- list(
    p_val = res$p_val,
    teststat = res$t_stat,
    teststat_boot = res$t_boot,
    N = preprocess$N,
    boot_iter = B,
    clustid = clustid,
    N_G = preprocess$N_G,
    call = call,
    type = type,
    impose_null = impose_null,
    R = R,
    r = r,
    boot_algo = "WildBootTests.jl",
    internal_seed = internal_seed
  )


  class(res_final) <- "mboottest"

  invisible(res_final)
}
