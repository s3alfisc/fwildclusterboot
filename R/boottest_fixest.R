#' Fast wild cluster bootstrap inference for object of class fixest
#'
#' `boottest.fixest` is a S3 method that allows for fast wild cluster
#' bootstrap inference for objects of class fixest by  implementing
#' fast wild bootstrap algorithms as developed in Roodman et al., 2019
#' and MacKinnon, Nielsen & Webb (2022).
#'
#' @param object An object of class fixest and estimated via `fixest::feols()`.
#' Non-linear models are not supported.
#' @param clustid A character vector or rhs formula containing the names of the
#' cluster variables. If NULL,
#'        a heteroskedasticity-robust (HC1) wild bootstrap is run.
#' @param param A character vector or rhs formula. The name of the regression
#'        coefficient(s) for which the hypothesis is to be tested
#' @param B Integer. The number of bootstrap iterations. When the number of
#'  clusters is low,
#'        increasing B adds little additional runtime.
#' @param bootcluster A character vector or rhs formula of length 1. Specifies
#' the bootstrap clustering variable or variables. If more
#'        than one variable is specified, then bootstrapping is clustered by the
#'         intersections of
#'        clustering implied by the listed variables. To mimic the behavior of
#'        stata's boottest command,
#'        the default is to cluster by the intersection of all the variables
#'        specified via the `clustid` argument,
#'        even though that is not necessarily recommended (see the paper by
#'         Roodman et al cited below, section 4.2).
#'        Other options include "min", where bootstrapping is clustered by
#'        the cluster variable with the fewest clusters.
#'        Further, the subcluster bootstrap (MacKinnon & Webb, 2018) is
#'         supported - see the `vignette("fwildclusterboot", package =
#'          "fwildclusterboot")` for details.
#' @param fe A character vector or rhs formula of length one which contains
#' the name of the fixed effect to be projected
#' out in the bootstrap. Note: if regression weights are used, fe
#' needs to be NULL.
#' @param sign_level A numeric between 0 and 1 which sets the significance level
#'        of the inference procedure. E.g. sign_level = 0.05
#'        returns 0.95% confidence intervals. By default, sign_level = 0.05.
#' @param conf_int A logical vector. If TRUE, boottest computes confidence
#'        intervals by test inversion. If FALSE, only the p-value is returned.
#' @param engine Character scalar. Either "R", "R-lean" or "WildBootTests.jl".
#'  Controls if `boottest()` should run via its native R implementation
#'  or `WildBootTests.jl`.
#'  "R" is the default and implements the cluster bootstrap
#'  as in Roodman (2019). "WildBootTests.jl" executes the
#'  wild cluster bootstrap via the WildBootTests.jl
#'  package. For it to run, Julia and WildBootTests.jl need
#'  to be installed.
#'  The "R-lean" algorithm is a memory friendly, but less
#'  performant rcpp-armadillo based implementation of the wild
#'  cluster bootstrap.
#'  Note that if no cluster is provided, boottest() always
#'  defaults to the "lean" algorithm. You can set the employed
#'  algorithm globally by using the
#'  `setBoottest_engine()` function.
#' @param bootstrap_type Determines which wild cluster bootstrap type should be
#' run. Options are "fnw11","11", "13", "31" and "33" for the wild cluster
#' bootstrap and "11" and "31" for the heteroskedastic bootstrap.
#' For more information, see the details section. "fnw11" is the default for
#' the cluster bootstrap, which runs a "11" type
#' wild cluster bootstrap via the algorithm outlined in "fast and wild"
#' (Roodman et al (2019)). "11" is the default for the heteroskedastic
#' bootstrap.
#' @param R Hypothesis Vector giving linear combinations of coefficients.
#' Must be either NULL or a vector of the same length as `param`. If NULL,
#' a vector of ones of length param.
#' @param r A numeric. Shifts the null hypothesis
#'        H0: param = r vs H1: param != r
#' @param beta0 Deprecated function argument. Replaced by function argument 'r'.
#' @param type character or function. The character string specifies the type
#'        of boostrap to use: One of "rademacher", "mammen", "norm"
#'        and "webb". Alternatively, type can be a function(n) for drawing
#'        wild bootstrap factors. "rademacher" by default.
#'        For the Rademacher distribution, if the number of replications B
#'        exceeds
#'        the number of possible draw ombinations, 2^(#number of clusters),
#'         then `boottest()`
#'        will use each possible combination once (enumeration).
#' @param impose_null Logical. Controls if the null hypothesis is imposed on
#'        the bootstrap dgp or not. Null imposed `(WCR)` by default.
#'        If FALSE, the null is not imposed `(WCU)`
#' @param p_val_type Character vector of length 1. Type of p-value.
#'        By default "two-tailed". Other options include "equal-tailed",
#'        ">" and "<".
#' @param tol Numeric vector of length 1. The desired accuracy
#'        (convergence tolerance) used in the root finding procedure to find
#'         the confidence interval.
#'        1e-6 by default.
#' @param maxiter Integer. Maximum number of iterations used in the root
#' finding procedure to find the confidence interval.
#'        10 by default.
#' @param nthreads The number of threads. Can be: a) an integer lower than,
#'                 or equal to, the maximum number of threads; b) 0: meaning
#'                 all available threads will be used; c) a number strictly
#'                 between 0 and 1 which represents the fraction of all threads
#'                 to use. The default is to use 1 core.
#' @param ssc An object of class `boot_ssc.type` obtained with the function
#'  [fwildclusterboot::boot_ssc()]. Represents how the small sample
#'   adjustments are computed. The defaults are `adj = TRUE, fixef.K = "none",
#'   cluster.adj = "TRUE", cluster.df = "conventional"`.
#'             You can find more details in the help file for `boot_ssc()`.
#'             The function is purposefully designed to mimic fixest's
#'             [fixest::ssc()] function.
#' @param getauxweights Logical. Whether to save auxilliary weight matrix (v)
#' @param floattype Float64 by default. Other option: Float32. Should floating
#'  point numbers in Julia be represented as 32 or 64 bit? Only relevant when
#'   'engine = "WildBootTests.jl"'
#' @param maxmatsize NULL by default = no limit. Else numeric scalar to set
#' the maximum size of auxilliary weight matrix (v), in gigabytes. Only
#' relevant when 'engine = "WildBootTests.jl"'
#' @param bootstrapc Logical scalar, FALSE by default. TRUE  to request
#' bootstrap-c instead of bootstrap-t. Only relevant when
#' 'engine = "WildBootTests.jl"'
#' @param sampling 'dqrng' or 'standard'. If 'dqrng', the 'dqrng' package is
#' used for random number generation (when available). If 'standard',
#' functions from the 'stats' package are used when available.
#' This argument is mostly a convenience to control random number generation in
#' a wrapper package around `fwildclusterboot`, `wildrwolf`.
#' I recommend to use the fast' option.
#' @param ... Further arguments passed to or from other methods.

#' @importFrom dreamerr check_arg validate_dots

#' @return An object of class `boottest`
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
#' \item{point_estimate}{R'beta. A scalar: the constraints vector times the
#'  regression coefficients.}
#' \item{grid_vals}{All t-statistics calculated while calculating the
#'       confidence interval.}
#' \item{p_grid_vals}{All p-values calculated while calculating the confidence
#'      interval.}
#' \item{t_stat}{The 'original' regression test statistics.}
#'  \item{t_boot}{All bootstrap t-statistics.}
#' \item{regression}{The regression object used in boottest.}
#' \item{call}{Function call of boottest.}
#' \item{engine}{The employed bootstrap algorithm.}
#' \item{nthreads}{The number of threads employed.}
#'
#' @export
#' @method boottest fixest
#'
#' @section Setting Seeds:
#' To guarantee reproducibility, you need to
#' set a global random seed via
#' + `set.seed()` when using
#'    1) the lean algorithm (via `engine = "R-lean"`) including the
#'     heteroskedastic wild bootstrap
#'    2) the wild cluster bootstrap via `engine = "R"` with Mammen weights or
#'    3) `engine = "WildBootTests.jl"`
#' + `dqrng::dqset.seed()` when using `engine = "R"` for Rademacher, Webb
#' or Normal weights
#'
#' @section Confidence Intervals:
#' `boottest` computes confidence intervals by inverting p-values.
#'       In practice, the following procedure is used:
#' \itemize{
#' \item Based on an initial guess for starting values, calculate p-values
#'       for 26 equal spaced points between the starting values.
#' \item Out of the 26 calculated p-values, find the two pairs of values x
#'       for which the corresponding p-values px cross the significance
#'       sign_level sign_level.
#' \item Feed the two pairs of x into an numerical root finding procedure
#'       and solve for the root. boottest currently relies on
#'       `stats::uniroot` and sets an absolute tolerance of 1e-06 and
#'       stops the procedure after 10 iterations.
#' }
#' @section Standard Errors:
#' `boottest` does not calculate standard errors.
#' @section Multiple Fixed Effects:
#' If your feols() model contains fixed effects, boottest() will internally convert all fixed
#' effects but the one specified via the `fe` argument to dummy variables.
#' @section Stata, Julia and Python Implementations:
#' The fast wild cluster bootstrap algorithms are further implemented in the
#' following software packages:
#' \itemize{
#' \item Stata:[boottest](https://github.com/droodman/boottest)
#' \item Julia:[WildBootTests.jl](https://github.com/droodman/WildBootTests.jl)
#' \item Python:[wildboottest](https://github.com/s3alfisc/wildboottest)
#' }
#' @srrstats {G1.1} *The help files of all boottest methods document that the
#' "fast and wild" algorithm is already implemented in the STATA boottest
#' package. Additional information in the boottest() documentation points to
#' Julia, Python and Stata implementations.
#' @srrstats {RE3.2} *Ensure that convergence thresholds have sensible
#'  default values, demonstrated through explicit documentation.* The
#'  convergence values set for the bisection used to invert p-values to
#'  calculate confidence intervals are documented (the `tol` function argument)
#' & can be changed by the user.
#' @srrstats {RE3.3} *Allow explicit setting of convergence thresholds,
#'  unless reasons against doing so are explicitly documented.*  See above.
#' @srrstats {RE4.0} *Regression Software should return some form of
#' "model" object, generally through using or modifying existing class
#' structures for model objects (such as `lm`, `glm`, or model objects
#'  from other packages), or creating a new class of model objects.*
#'  Objects of type boottest are returned.
#' @srrstats {G2.0} *Function argument checks are implemented throughout
#' with via the `dreamerr` package.* Function values checked via dreamerr.
#' @srrstats {G2.1} *Implement assertions on types of inputs (see the initial
#' point on nomenclature above).*Function values checked via dreamerr.
#' @srrstats {G2.1a} *Provide explicit secondary documentation of expectations
#' on data types of all vector inputs. Potential vector inputs are "R", "param",
#' "clustid", "bootcluster". Length expectations are clearly documented.
#' @srrstats {G2.2} *Appropriately prohibit or restrict submission of
#'  multivariate input to parameters expected to be univariate.* See
#' dreamerr checks below.
#' @srrstats {G2.3} *For univariate character input:*Function values checked
#' via dreamerr.
#' @srrstats {G2.3a} *Use `match.arg()` or equivalent where applicable to only
#' permit expected values.*Function values checked via dreamerr.
#' @srrstats {G2.3b} *Either: use `tolower()` or equivalent to ensure input of
#' character parameters is not case dependent; or explicitly document that
#' parameters are strictly case-sensitive.* Only char arguments are bootstrap
#' types. If not lower case, tolower(type) is applied.



#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#'             STATA using boottest", The STATA Journal.
#'      (<https://ideas.repec.org/p/qed/wpaper/1406.html>)
#' @references MacKinnon, James G., Morten Ørregaard Nielsen, and
#' Matthew D. Webb. Fast and reliable jackknife and bootstrap
#'  methods for cluster-robust inference. No. 1485. 2022.
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller.
#' "Bootstrap-based improvements for inference with clustered errors."
#' The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references Cameron, A.Colin & Douglas L. Miller.
#' "A practitioner's guide to cluster-robust inference"
#' Journal of Human Resources (2015) \doi{doi:10.3368/jhr.50.2.317}
#' @references Davidson & MacKinnon. "Wild Bootstrap Tests for IV regression"
#' Journal of Economics and Business Statistics (2010)
#'  \doi{https://doi.org/10.1198/jbes.2009.07221}
#' @references MacKinnon, James G., and Matthew D. Webb.
#' "The wild bootstrap for few (treated) clusters.
#' " The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James G., and Matthew D. Webb.
#'  "Cluster-robust inference: A guide to empirical practice"
#'  Journal of Econometrics (2022)
#'  \doi{https://doi.org/10.1016/j.jeconom.2022.04.001}
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals."
#'  L'Actualite economique 91.1-2 (2015): 11-33.
#' @references Webb, Matthew D. Reworking wild bootstrap based inference for
#'  clustered errors. No. 1315. Queen's Economics Department Working Paper,
#'   2013.
#' @srrstats {G1.0} *`boottest()` links to multiple published papers.*
#' @examples
#' \dontrun{
#' requireNamespace("fixest")
#' requireNamespace("fwildclusterboot")
#' data(voters)
#' feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income,
#'  fixef = "Q1_immigration",
#'  data = voters
#' )
#' boot1 <- boottest(feols_fit,
#'  B = 9999,
#'  param = "treatment",
#'  clustid = "group_id1"
#' )
#' boot2 <- boottest(feols_fit,
#'  B = 9999,
#'  param = "treatment",
#'  clustid = c("group_id1", "group_id2")
#' )
#' boot3 <- boottest(feols_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = c("group_id1", "group_id2"),
#'   fe = "Q1_immigration"
#' )
#' boot4 <- boottest(feols_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = c("group_id1", "group_id2"),
#'   fe = "Q1_immigration",
#'   sign_level = 0.2,
#'   r = 2
#' )
#' # test treatment + ideology1 = 2
#' boot5 <- boottest(feols_fit,
#'   B = 9999,
#'   clustid = c("group_id1", "group_id2"),
#'   param = c("treatment", "ideology1"),
#'   R = c(1, 1),
#'   r = 2
#' )
#' summary(boot1)
#' print(boot1)
#' plot(boot1)
#' nobs(boot1)
#' pval(boot1)
#' confint(boot1)
#' generics::tidy(boot1)
#'
#' # run different bootstrap types following MacKinnon, Nielsen & Webb (2022):
#'
#' # default: the fnw algorithm
#' boot_fnw11 <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1",
#'   bootstrap_type = "fnw11"
#' )
#'
#' # WCR 31
#'boot_WCR31 <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1",
#'   bootstrap_type = "31"
#' )
#'
#' # WCU33
#'boot_WCR31 <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1",
#'   bootstrap_type = "33",
#'   impose_null = FALSE
#' )
#'
#' }
#'
#' @srrstats {RE1.0} *Regression Software should enable models to be specified
#' via a formula interface, unless reasons for not doing so are explicitly
#' documented.* The following function arguments can be formulas: param, clustid, fe.



boottest.fixest <- function(object,
                            param,
                            B,
                            clustid = NULL,
                            bootcluster = "max",
                            fe = NULL,
                            sign_level = 0.05,
                            conf_int = TRUE,
                            R = NULL,
                            r = 0,
                            beta0 = NULL,
                            type = "rademacher",
                            impose_null = TRUE,
                            bootstrap_type = "fnw11",
                            p_val_type = "two-tailed",
                            tol = 1e-6,
                            maxiter = 10,
                            sampling = "dqrng",
                            nthreads = getBoottest_nthreads(),
                            ssc = boot_ssc(
                              adj = TRUE,
                              fixef.K = "none",
                              cluster.adj = TRUE,
                              cluster.df = "conventional"
                            ),
                            engine = getBoottest_engine(),
                            floattype = "Float64",
                            maxmatsize = FALSE,
                            bootstrapc = FALSE,
                            getauxweights = FALSE,
                            ...) {
  call <- match.call()

  dreamerr::validate_dots(stop = TRUE)
  type <- tolower(type)

  # Step 1: check arguments of feols call
  check_arg(object, "MBT class(fixest)")
  check_arg(clustid, "NULL | character scalar | character vector | formula")
  check_arg(param, "MBT scalar character | character vector | formula")
  check_arg(B, "MBT scalar integer GT{99}")
  check_arg(impose_null, "logical scalar")
  check_arg(bootstrap_type, "charin(11, 13, 31, 33, fnw11)")

  check_arg(sign_level, "scalar numeric GT{0} LT{1}")
  check_arg(type, "charin(rademacher, mammen, norm, gamma, webb)")
  check_arg(p_val_type, "charin(two-tailed, equal-tailed,>, <)")

  check_arg(conf_int, "logical scalar")
  check_arg(R, "NULL| scalar numeric | numeric vector")
  check_arg(r, "numeric scalar | NULL")
  check_arg(fe, "character scalar | NULL | formula")
  check_arg(bootcluster, "character vector | formula")
  check_arg(tol, "numeric scalar GT{0}")
  check_arg(maxiter, "scalar integer GT{5}")
  check_arg(boot_ssc, "class(ssc) | class(boot_ssc)")
  check_arg(engine, "charin(R, R-lean, WildBootTests.jl)")

  check_arg(floattype, "charin(Float32, Float64)")
  check_arg(maxmatsize, "scalar integer | NULL")
  check_arg(bootstrapc, "scalar logical")

  check_arg(sampling, "charin(dqrng, standard)")

  if(bootstrap_type != "fnw11"){
    if(engine == "R"){
      if(conf_int){
        cis_only_for_fnw11()
      }
    }
  }

  if (!is.null(beta0)) {
    arg_beta0_is_deprecated_error()
  }

  if (inherits(clustid, "formula")) {
    clustid <- attr(terms(clustid), "term.labels")
  }

  if (inherits(bootcluster, "formula")) {
    bootcluster <- attr(terms(bootcluster), "term.labels")
  }

  if (inherits(param, "formula")) {
    param <- attr(terms(param), "term.labels")
  }

  if (inherits(fe, "formula")) {
    fe <- attr(terms(fe), "term.labels")
  }


  if (!is.null(object$fixef_removed)) {
    please_remove_fixest_na_error()
  }

  # --------------------------------------------

  # check appropriateness of nthreads
  nthreads <- check_set_nthreads(nthreads)

  if (is.null(clustid)) {
    heteroskedastic <- TRUE
    if (engine == "R") {
      # heteroskedastic models should always be run through R-lean
      engine <- "R-lean"
    }
  } else {
    heteroskedastic <- FALSE
  }

  check_bootstrap_types(
    param = param,
    bootstrap_type = bootstrap_type
  )

  R_long <- process_R(
    R = R,
    param = param
  )


  if (engine != "WildBootTests.jl") {
    r_algo_checks(
      R = R_long,
      p_val_type = p_val_type,
      conf_int = conf_int,
      B = B
    )
  }

  check_params_in_model(object = object, param = param)

  check_boottest_args_plus(
    object = object,
    R = R_long,
    param = param,
    sign_level = sign_level,
    B = B,
    fe = fe
  )

  # preprocess the data: Y, X, weights, fixed_effect
  preprocess <- preprocess2_fixest(
    object = object,
    clustid = clustid,
    R = R_long,
    param = param,
    bootcluster = bootcluster,
    fe = fe,
    engine = engine,
    bootstrap_type = bootstrap_type
  )

  enumerate <-
    check_set_full_enumeration(
      preprocess = preprocess,
      heteroskedastic = heteroskedastic,
      B = B,
      type = type,
      engine = engine
    )
  full_enumeration <- enumerate$full_enumeration
  B <- enumerate$B

  N <- preprocess$N
  k <- preprocess$k
  G <-
    vapply(preprocess$clustid, function(x) {
      length(unique(x))
    }, numeric(1))
  vcov_sign <- preprocess$vcov_sign

  small_sample_correction <-
    get_ssc(
      boot_ssc_object = ssc,
      N = N,
      k = k,
      G = G,
      vcov_sign = vcov_sign,
      heteroskedastic = heteroskedastic
    )

  # clustermin, clusteradj


  clustid_dims <- preprocess$clustid_dims
  # R*beta;
  point_estimate <-
    as.vector(object$coefficients[param] %*% preprocess$R0[param])

  boot_vcov <- boot_coef <- NULL

  res <-
    run_bootstrap(
      object = object,
      engine = engine,
      preprocess = preprocess,
      bootstrap_type = bootstrap_type,
      B = B,
      point_estimate = point_estimate,
      impose_null = impose_null,
      r = r,
      sign_level = sign_level,
      param = param,
      p_val_type = p_val_type,
      nthreads = nthreads,
      type = type,
      full_enumeration = full_enumeration,
      small_sample_correction = small_sample_correction,
      conf_int = conf_int,
      maxiter = maxiter,
      tol = tol,
      clustid = clustid,
      fe = fe,
      R_long = R_long,
      heteroskedastic = heteroskedastic,
      ssc = ssc,
      floattype = floattype ,
      bootstrapc = bootstrapc ,
      getauxweights = getauxweights ,
      maxmatsize = maxmatsize,
      sampling = sampling,
      bootcluster = bootcluster

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
    R = R_long,
    r = r,
    engine = engine,
    nthreads = nthreads,
    boot_vcov = boot_vcov,
    boot_coef = boot_coef
  )


  class(res_final) <- "boottest"
  invisible(res_final)
}

