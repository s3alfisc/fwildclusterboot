#' Fast wild cluster bootstrap inference
#'
#'
#' `boottest` is a S3 method that allows for fast wild cluster
#' bootstrap inference for objects of class lm, fixest and felm by  implementing
#' the fast wild bootstrap algorithm developed in Roodman et al., 2019.
#'
#' @param object An object of type lm, fixest, felm or ivreg
#' @param ... other arguments
#'
#' @seealso [boottest.lm][fwildclusterboot::boottest.lm],
#' [boottest.fixest][fwildclusterboot::boottest.fixest],
#' [boottest.felm][fwildclusterboot::boottest.felm],
#'  [boottest.ivreg][fwildclusterboot::boottest.ivreg]
#'
#' @examples
#' requireNamespace("fwildclusterboot")
#' data(voters)
#' lm_fit <- lm(
#'   proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'   data = voters
#' )
#' boot <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1"
#' )
#' summary(boot)
#' print(boot)
#' plot(boot)
#' nobs(boot)
#' pval(boot)
#' confint(boot)
#' generics::tidy(boot)
#'
#' @export
#'
#' @section Setting Seeds:
#' To guarantee reproducibility, you can either use `boottest()'s` `seed`
#' function argument, or
#' set a global random seed via
#' + `set.seed()` when using
#'    1) the lean algorithm (via `engine = "R-lean"`), 2) the heteroskedastic
#'     wild bootstrap
#'    3) the wild cluster bootstrap via `engine = "R"` with Mammen weights
#'     or 4) `engine = "WildBootTests.jl"`
#' + `dqrng::dqset.seed()` when using `engine = "R"` for Rademacher, Webb
#' or Normal weights
#'
#' @return An object of class `boottest`.
#' 
#' @section Stata, Julia and Python Implementations:
#' The fast wild cluster bootstrap algorithms are further implemented in the 
#' following software packages: 
#' \itemize{
#' \item Stata:[boottest](https://github.com/droodman/boottest) 
#' \item Julia:[WildBootTests.jl](https://github.com/droodman/WildBootTests.jl) 
#' \item Python:[wildboottest](https://github.com/s3alfisc/wildboottest) 
#' }
v#' 
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#' STATA using boottest", The STATA Journal.
#' (<https://ideas.repec.org/p/qed/wpaper/1406.html>)
#' @references MacKinnon, James G., Morten Ørregaard Nielsen, and 
#' Matthew D. Webb. Fast and reliable jackknife and bootstrap
#'  methods for cluster-robust inference. No. 1485. 2022. 
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

boottest <- function(object,
                     ...) {
  UseMethod("boottest")
}


#' Arbitrary Linear Hypothesis Testing for Regression Models via Wald-Tests
#'
#'
#' `mboottest` is a S3 method that allows for arbitrary linear
#' hypothesis testing
#' for objects of class lm, fixest, felm
#'
#' @param object An object of type lm, fixest or felm
#' @param ... other arguments
#'
#' @seealso [mboottest.lm][fwildclusterboot::mboottest.lm]
#'  [mboottest.felm][fwildclusterboot::mboottest.felm]
#'  [mboottest.fixest][fwildclusterboot::mboottest.fixest]
#'
#' @export
#'
#' @section Setting Seeds:
#' To guarantee reproducibility, you can either use `boottest()'s` `seed`
#'  function argument, or
#' set a global random seed via
#' + `set.seed()` when using
#'    1) the lean algorithm (via `engine = "R-lean"`),
#'    2) the heteroskedastic wild bootstrap
#'    3) the wild cluster bootstrap via `engine = "R"` with Mammen weights or
#'     4) `engine = "WildBootTests.jl"`
#' + `dqrng::dqset.seed()` when using `engine = "R"` for Rademacher,
#'  Webb or Normal weights
#'
#' @return An object of class `mboottest`.
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
mboottest <- function(object,
                      ...) {
  UseMethod("mboottest")
}

#' `pval` is a S3 method to collect pvalues for objects
#' of type `boottest` and `mboottest`
#'
#' @param object An object of type lm, fixest, felm or ivreg
#' @param ... other arguments
#'
#' @export
#' 
#' @examples
#' requireNamespace("fwildclusterboot")
#' data(voters)
#' lm_fit <- lm(
#' proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'   data = voters
#' )
#' boot <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1"
#' )
#' pval(boot)
#' @return 
#' A scalar with the bootstrapped p-value. 




pval <- function(object,
                 ...) {
  UseMethod("pval")
}

#' `teststat` is a S3 method to collect teststats for objects
#' of type `boottest` and `mboottest`
#'
#' @param object An object of type lm, fixest, felm or ivreg
#' @param ... other arguments
#'
#' @export
#' @examples
#' requireNamespace("fwildclusterboot")
#' data(voters)
#' lm_fit <- lm(
#'   proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'   data = voters
#' )
#' boot <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1"
#' )
#' teststat(boot)
#' @return A scalar with containing the non-bootstrapped
#' test statistic of interest

teststat <- function(object,
                     ...) {
  UseMethod("teststat")
}

#' S3 method to obtain wild cluster bootstrapped confidence intervals
#' @param object object of type boottest
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method confint boottest
#' @return A vector containing the boundaries of the wild cluster
#'  bootstrapped confidence interval
#' @examples
#' requireNamespace("fwildclusterboot")
#' data(voters)
#' lm_fit <- lm(
#'   proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'   data = voters
#' )
#' boot <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1"
#' )
#' teststat(boot)
#'
confint.boottest <- function(object, ...) {
  stopifnot(inherits(object, "boottest"))

  object$conf_int
}

#' S3 method to obtain the wild cluster bootstrapped p-value of an object
#' of type boottest
#' @param object object of type boottest
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method pval boottest
#' @return A vector containing the boundaries of the wild cluster
#'  bootstrapped p-value
#' @examples
#' #' requireNamespace("fwildclusterboot")
#' data(voters)
#' lm_fit <- lm(
#'   proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'   data = voters
#' )
#' boot <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1"
#' )
#' confint(boot)
#'

pval.boottest <- function(object, ...) {
  stopifnot(inherits(object, "boottest"))

  object$p_val
}

#' S3 method to obtain the non-bootstrapped t-statistic calculated
#' via `boottest()`
#' @param object An object of type boottest
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method teststat boottest
#' @return A vector containing the non-bootstrapped t-statistic
#'  calculated in `boottest()`
#' @examples
#' requireNamespace("fwildclusterboot")
#' data(voters)
#' lm_fit <- lm(
#'   proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'   data = voters
#' )
#' boot <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1"
#' )
#' teststat(boot)
#'
teststat.boottest <- function(object, ...) {
  stopifnot(inherits(object, "boottest"))

  object$t_stat
}


#' S3 method to obtain the effective number of observation used in `boottest()`
#' @param object object of type boottest
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method nobs boottest
#' @return A scalar containing the effective number of observations
#'  used in `boottest()`
#' @examples
#' requireNamespace("fwildclusterboot")
#' data(voters)
#' lm_fit <- lm(
#' proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'   data = voters
#' )
#' boot <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1"
#' )
#' nobs(boot)

nobs.boottest <- function(object, ...) {
  stopifnot(inherits(object, "boottest"))

  object$N
}

tidy.boottest <- function(x, ...) {
  #' S3 method to summarize objects of class boottest into tidy data.frame
  #' @param x object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics tidy
  #' @export
  #' @method tidy boottest
  #' @rdname tidy.boottest
  #' @return A tidy data.frame with estimation results for objects of type
  #'         boottest
  #' @examples
  #' requireNamespace("fwildclusterboot")
  #' data(voters)
  #' lm_fit <- lm(
  #' proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
  #'   data = voters
  #' )
  #' boot <- boottest(lm_fit,
  #'   B = 9999,
  #'   param = "treatment",
  #'   clustid = "group_id1"
  #' )
  #' generics::tidy(boot)

  stopifnot(inherits(x, "boottest"))
  # dreamerr::validate_dots(stop = TRUE)

  if (x$engine == "WildBootTests.jl") {
    R <- x$R[which(x$R != 0)]
    hypothesis <-
      paste(paste0(paste0(R, "*"), x$param, collapse = "+"), "=", x$r)
  } else {
    hypothesis <-
      paste(
        paste0(
          paste0(
            x$R, "*"
          ),
          x$param,
          collapse = "+"
        ),
        "=", x$r
      )
  }

  term <- hypothesis
  estimate <- x$point_estimate
  statistic <- x$t_stat
  p.value <- x$p_val
  # std.error <- NA
  if (!is.null(x$conf_int)) {
    conf.low <- x$conf_int[1]
    conf.high <- x$conf_int[2]
  } else {
    conf.low <- conf.high <- NA
  }

  res <-
    data.frame(term, estimate, statistic, p.value, conf.low, conf.high)

  return(res)
}

summary.boottest <- function(object, digits = 3, ...) {
  #' S3 method to summarize objects of class boottest
  #' @param object object of type boottest
  #' @param digits rounding of output. 3 by default
  #' @param ... Further arguments passed to or from other methods.
  #' @method summary boottest
  #' @export
  #' @return Returns result summaries for objects of type boottest
  #' @examples
  #' requireNamespace("fwildclusterboot")
  #' data(voters)
  #' lm_fit <- lm(
  #' proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
  #'   data = voters
  #' )
  #' boot <- boottest(lm_fit,
  #'   B = 9999,
  #'   param = "treatment",
  #'   clustid = "group_id1"
  #' )
  #' summary(boot)

  stopifnot(inherits(object, "boottest"))
  dreamerr::validate_dots(stop = TRUE)

  N <- object$N
  B <- object$B
  sign_level <- object$sign_level
  signif_level <- paste0((1 - sign_level) * 100, "%")
  call <- object$call
  N_G <- object$N_G
  B <- object$B
  type <-
    ifelse(object$type %in% c("rademacher", "mammen", "norm", "webb"),
      object$type,
      "custom"
    )
  # clustid <-
  estim_function <- class(object$regression)

  clustering_type <- paste0(length(object$clustid), "-way")
  numb_clusters <- object$N_G

  tidy_names <-
    c(
      "term",
      "estimate",
      "statistic",
      "p.value",
      "conf.low",
      "conf.high"
    )

  tidy_object <- lapply(
    tidy_names,
    function(x) {
      if (is.numeric(tidy.boottest(object)[[x]])) {
        round(tidy.boottest(object)[[x]], digits = digits)
      } else {
        tidy.boottest(object)[[x]]
      }
    }
  )

  tidy_object <- as.data.frame(tidy_object)
  names(tidy_object) <- tidy_names

  if (object$engine == "WildBootTests.jl") {
    R <- object$R[which(object$R != 0)]
    hypothesis <-
      paste(paste0(paste0(R, "*"), object$param, collapse = "+"), "=", object$r)
  } else {
    hypothesis <-
      paste(
        paste0(
          paste0(
            object$R, "*"
          ),
          object$param,
          collapse = "+"
        ),
        "=",
        object$r
      )
  }

  print(call)
  cat(
    "\t\n",
    sprintf("Hypothesis: %s\n", hypothesis),
    sprintf("Observations: %s\n", N),
    sprintf("Bootstr. Iter: %s\n", B),
    sprintf("Bootstr. Type: %s\n", type),
    sprintf("Clustering: %s\n", clustering_type),
    sprintf("Confidence Sets: %s\n", signif_level),
    sprintf("Number of Clusters: %s\n", Reduce(paste, numb_clusters)),

    # sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)),
    sprintf("%s\n", "")
  )

  return(tidy_object)
}

plot.boottest <- function(x, ...) {
  #' Plot the bootstrap distribution of t-statistics
  #' @param x An object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @method plot boottest
  #' @export
  #' @return A plot of bootstrap t-statistics under different null hypotheses
  #' @examples
  #' requireNamespace("fwildclusterboot")
  #' data(voters)
  #' lm_fit <- lm(
  #' proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
  #'   data = voters
  #' )
  #' boot <- boottest(lm_fit,
  #'   B = 9999,
  #'   param = "treatment",
  #'   clustid = "group_id1"
  #' )
  #' plot(boot)

  stopifnot(inherits(x, "boottest"))
  dreamerr::validate_dots(stop = TRUE)

  if (is.null(x$conf_int)) {
    stop("No plot method if boottest()'s function argument 'conf_int = FALSE'.")
  }
  test_vals <- x$grid_vals
  p_test_vals <- x$p_grid_vals
  conf_int <- x$conf_int
  sign_level <- x$sign_level
  xlab <- x$param

  graphics::plot(
    x = test_vals,
    y = p_test_vals,
    type = "b",
    pch = 20,
    lty = 2,
    xlab = xlab,
    ylab = "p-value"
  )
  graphics::lines(test_vals, p_test_vals, type = "l", lty = 1)
  graphics::abline(v = conf_int[1], col = "blue")
  graphics::abline(v = conf_int[2], col = "blue")
  graphics::abline(h = sign_level, col = "red")
}

glance.boottest <- function(x, ...) {
  #' S3 method to glance at objects of class boottest
  #' @param x object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics glance
  #' @method glance boottest
  #' @export
  #' @return A single row summary "glance" of an object of type boottest
  #'         - lists characteristics of the input regression model
  #' @examples
  #' \dontrun{
  #' requireNamespace("fwildclusterboot")
  #' data(voters)
  #' lm_fit <- lm(
  #' proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
  #'   data = voters
  #' )
  #' boot <- boottest(lm_fit,
  #'   B = 9999,
  #'   param = "treatment",
  #'   clustid = "group_id1"
  #' )
  #' generics::glance(boot)
  #' }

  stopifnot(inherits(x, "boottest"))
  broom::glance(eval(x$call$object))
}

glance.mboottest <- function(x, ...) {
  #' S3 method to glance at objects of class boottest
  #' @param x object of type mboottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics glance
  #' @method glance mboottest
  #' @export
  #' @return A single row summary "glance" of an object of type boottest
  #'         - lists characteristics of the input regression model
  #' @examples
  #' \dontrun{
  #' requireNamespace("fwildclusterboot")
  #' data(voters)
  #' lm_fit <- lm(
  #' proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
  #'   data = voters
  #' )
  #' mboot <- mboottest(
  #'     object = lm_fit,
  #'     clustid = "group_id1",
  #'     B = 999,
  #'     R = R
  #' )
  #' generics::glance(mboot)
  #' }
  
  stopifnot(inherits(x, "mboottest"))
  broom::glance(eval(x$call$object))
}


#' S3 method to obtain the wild cluster bootstrapped p-value of an object
#' of type mboottest
#' @param object object of type mboottest
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method pval mboottest
#' @return A vector containing the boundaries of the wild cluster
#' bootstrapped p-value
#' @examples
#' \dontrun{
#' requireNamespace("clubSandwich")
#' R <- clubSandwich::constrain_zero(2:3, coef(lm_fit))
#' wboottest <-
#'   mboottest(
#'     object = lm_fit,
#'     clustid = "group_id1",
#'     B = 999,
#'     R = R
#'   )
#' pval(wboottest)
#' }
#'
pval.mboottest <- function(object, ...) {
  stopifnot(inherits(object, "mboottest"))

  object$p_val
}

#' S3 method to obtain the non-bootstrapped test statistic calculated
#'  via `mboottest()`
#' @param object An object of type 'mboottest'
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method teststat mboottest
#' @return A vector containing the non-bootstrapped t-statistic calculated
#' in `mboottest()`
#' @examples
#' \dontrun{
#' requireNamespace("clubSandwich")
#' R <- clubSandwich::constrain_zero(2:3, coef(lm_fit))
#' wboottest <-
#'   mboottest(
#'     object = lm_fit,
#'     clustid = "group_id1",
#'     B = 999,
#'     R = R
#'   )
#' teststat(wboottest)
#' }
#'
teststat.mboottest <- function(object, ...) {
  stopifnot(inherits(object, "mboottest"))

  object$teststat
}


#' S3 method to obtain the effective number of observation used
#' in `mboottest()`
#' @param object object of type mboottest
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @method nobs mboottest
#' @return A scalar containing the effective number of observations
#'  used in `mboottest()`
#' @examples
#' \dontrun{
#' requireNamespace("clubSandwich")
#' R <- clubSandwich::constrain_zero(2:3, coef(lm_fit))
#' wboottest <-
#'   mboottest(
#'     object = lm_fit,
#'     clustid = "group_id1",
#'     B = 999,
#'     R = R
#'   )
#' nobs(wboottest)
#' }
#'
nobs.mboottest <- function(object, ...) {
  stopifnot(inherits(object, "mboottest"))

  object$N
}


#' S3 method to print key information for objects of type `bboottest`
#' @param x object of type boottest
#' @param ... Further arguments passed to or from other methods.
#' @param digits Number of rounding digits
#' @export
#' @method print boottest
#' @return A scalar containing the effective number of observations
#' used in `mboottest`
#' @examples
#' #' requireNamespace("fwildclusterboot")
#' data(voters)
#' lm_fit <- lm(
#'   proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
#'   data = voters
#' )
#' boot <- boottest(lm_fit,
#'   B = 9999,
#'   param = "treatment",
#'   clustid = "group_id1"
#' )
#' print(boot)
#'
print.boottest <- function(x, ..., digits = 4) {
  stopifnot(inherits(x, "boottest"))

  print(x$call)
  cat("", "\n")

  vals <- lapply(
    c("p_val", "conf_int", "t_stat"),
    function(y) {
      round(x[[y]], digits = digits)
    }
  )

  cat("p value:", vals[[1]], "\n")
  cat("confidence interval:", vals[[2]], "\n")
  cat("test statistic", vals[[3]], "\n")
}


#' S3 method to print key information for objects of type `mboottest`
#' @param x object of type mboottest
#' @param ... Further arguments passed to or from other methods.
#' @param digits Number of rounding digits
#' @export
#' @method print mboottest
#' @return A scalar containing the effective number of observations used
#' in `mboottest`
#' @examples
#' \dontrun{
#' requireNamespace("clubSandwich")
#' R <- clubSandwich::constrain_zero(2:3, coef(lm_fit))
#' wboottest <-
#'   mboottest(
#'     object = lm_fit,
#'     clustid = "group_id1",
#'     B = 999,
#'     R = R
#'   )
#' print(wboottest)
#' }
#'
print.mboottest <- function(x, ..., digits = 4) {
  stopifnot(inherits(x, "mboottest"))

  print(x$call)
  cat("", "\n")

  vals <- lapply(
    c("p_val", "teststat"),
    function(y) {
      round(x[[y]], digits = digits)
    }
  )

  cat("p value:", vals[[1]], "\n")
  cat("test statistic", vals[[2]], "\n")
}


tidy.mboottest <- function(x, ...) {

  #' S3 method to summarize objects of class mboottest into tidy data.frame
  #' @param x object of type mboottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics tidy
  #' @rdname tidy.mboottest
  #' @export
  #'
  #' @method tidy mboottest
  #' @return A tidy data.frame with estimation results for objects of type
  #'         mboottest
  #' @examples
  #' \dontrun{
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


  stopifnot(inherits(x, "mboottest"))
  # dreamerr::validate_dots(stop = TRUE)

  statistic <- x$teststat
  p.value <- x$p_val

  res <- data.frame(teststat = statistic, p_val = p.value)

  return(res)
}

summary.mboottest <- function(object, digits = 3, ...) {
  #' S3 method to summarize objects of class mboottest
  #' @param object object of type mboottest
  #' @param digits rounding of output. 3 by default
  #' @param ... Further arguments passed to or from other methods.
  #' @method summary mboottest
  #'
  #' @export
  #'
  #' @return Returns result summaries for objects of type mboottest
  #' @examples
  #' \dontrun{
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



  stopifnot(inherits(object, "mboottest"))
  dreamerr::validate_dots(stop = TRUE)

  N <- object$N
  B <- object$B
  call <- object$call
  N_G <- object$N_G
  B <- object$B
  type <- ifelse(
    object$type %in% c("rademacher", "mammen", "norm", "webb"),
    object$type, "custom"
  )

  clustering_type <- paste0(length(object$clustid), "-way")
  numb_clusters <- object$N_G

  tidy_names <- c("teststat", "p_val")

  # rounding
  tidy_object <- lapply(
    tidy_names,
    function(x) {
      if (is.numeric(tidy(object)[[x]])) {
        round(tidy(object)[[x]], digits = digits)
      } else {
        tidy(object)[[x]]
      }
    }
  )

  tidy_object <- as.data.frame(tidy_object)
  names(tidy_object) <- tidy_names

  hypothesis <- "Multivariate mboottest"

  print(call)
  cat(
    "\t\n",
    sprintf("Hypothesis: %s\n", hypothesis),
    sprintf("Observations: %s\n", N),
    sprintf("Bootstr. Iter: %s\n", B),
    sprintf("Bootstr. Type: %s\n", type),
    sprintf("Clustering: %s\n", clustering_type),
    sprintf("Number of Clusters: %s\n", Reduce(paste, numb_clusters)),

    # sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)),
    sprintf("%s\n", "")
  )

  tidy(object)
}


#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance
