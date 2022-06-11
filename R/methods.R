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
#' @seealso \link[fwildclusterboot]{boottest.lm}, \link[fwildclusterboot]{boottest.fixest}, \link[fwildclusterboot]{boottest.felm}, \link[fwildclusterboot]{boottest.ivreg}
#'
#' @export
#' 
#' @section Setting Seeds: 
#' To guarantee reproducibility, you can either use `boottest()'s` `seed` function argument, or 
#' set a global random seed via 
#' + `set.seed()` when using
#'    1) the lean algorithm (via `boot_algo = "R-lean"`), 2) the heteroskedastic wild bootstrap 
#'    3) the wild cluster bootstrap via `boot_algo = "R"` with Mammen weights or 4) `boot_algo = "WildBootTests.jl"`
#' + `dqrng::dqset.seed()` when using `boot_algo = "R"` for Rademacher, Webb or Normal weights
#'
#' @return An object of class \code{boottest}.
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#'             STATA using boottest", The STATA Journal.
#'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. "Bootstrap-based improvements for inference with clustered errors." The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references Cameron, A.Colin & Douglas L. Miller. "A practitioner's guide to cluster-robust inference" Journal of Human Resources (2015) \doi{doi: 10.3368/jhr.50.2.317}
#' @references MacKinnon, James G., and Matthew D. Webb. "The wild bootstrap for few (treated) clusters." The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James G., and Matthew D. Webb. "Cluster-robust inference: A guide to empirical practice" Journal of Econometrics (2022) \doi{https://doi.org/10.1016/j.jeconom.2022.04.001}
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals." L'Actualite economique 91.1-2 (2015): 11-33.
#' @references Webb, Matthew D. Reworking wild bootstrap based inference for clustered errors. No. 1315. Queen's Economics Department Working Paper, 2013.

boottest <- function(object,
                     ...) {
  UseMethod("boottest")
}


#' Arbitrary Linear Hypothesis Testing for Regression Models via Wald-Tests
#'
#'
#' `mboottest` is a S3 method that allows for arbitrary linear hypothesis testing
#' for objects of class lm, fixest, felm
#'
#' @param object An object of type lm, fixest or felm
#' @param ... other arguments
#'
#' @seealso \link[fwildclusterboot]{mboottest.lm} \link[fwildclusterboot]{mboottest.felm} \link[fwildclusterboot]{mboottest.fixest}
#'
#' @export
#'
#' @section Setting Seeds: 
#' To guarantee reproducibility, you can either use `boottest()'s` `seed` function argument, or 
#' set a global random seed via 
#' + `set.seed()` when using
#'    1) the lean algorithm (via `boot_algo = "R-lean"`), 2) the heteroskedastic wild bootstrap 
#'    3) the wild cluster bootstrap via `boot_algo = "R"` with Mammen weights or 4) `boot_algo = "WildBootTests.jl"`
#' + `dqrng::dqset.seed()` when using `boot_algo = "R"` for Rademacher, Webb or Normal weights
#' 
#' @return An object of class \code{mboottest}.
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in
#'             STATA using boottest", The STATA Journal.
#'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
#' @references Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. "Bootstrap-based improvements for inference with clustered errors." The Review of Economics and Statistics 90.3 (2008): 414-427.
#' @references MacKinnon, James G., and Matthew D. Webb. "The wild bootstrap for few (treated) clusters." The Econometrics Journal 21.2 (2018): 114-135.
#' @references MacKinnon, James. "Wild cluster bootstrap confidence intervals." L'Actualite economique 91.1-2 (2015): 11-33.
#' @references Webb, Matthew D. Reworking wild bootstrap based inference for clustered errors. No. 1315. Queen's Economics Department Working Paper, 2013.

mboottest <- function(object,
                      ...) {
  UseMethod("mboottest")
}








#' #' S3 method to obtain wild cluster bootstrapped confidence intervals
#' #' @param object object of type boottest
#' #' @param ... Further arguments passed to or from other methods.
#' #' @export
#' #' @method confint boottest
#' #' @return A vector containing the boundaries of the wild cluster bootstrapped confidence interval
#' #'
#' confint.boottest <- function(object, ...){
#'
#'   stopifnot(inherits(object, "boottest"))
#'
#'   object$conf_int
#'
#' }
#'
#' #' S3 method to obtain the effective number of observation used in `boottest()`
#' #' @param object object of type boottest
#' #' @param ... Further arguments passed to or from other methods.
#' #' @export
#' #' @method nobs boottest
#' #' @return A scalar containing the effective number of observations used in `boottest()`
#'
#' nobs.boottest <- function(object, ...){
#'
#'   stopifnot(inherits(object, "boottest"))
#'
#'   object$N
#'
#' }


#' #' S3 method to obtain wild cluster bootstrapped confidence intervals
#' #' @param object object of type boottest
#' #' @param ... Further arguments passed to or from other methods.
#' #' @export
#' #' @method confint boottest
#' #' @return A vector containing the boundaries of the wild cluster bootstrapped confidence interval
#' #'
#' confint.boottest <- function(object, ...){
#'
#'   stopifnot(inherits(object, "boottest"))
#'
#'   object$conf_int
#'
#' }
#'
#' #' S3 method to obtain the effective number of observation used in `boottest()`
#' #' @param object object of type boottest
#' #' @param ... Further arguments passed to or from other methods.
#' #' @export
#' #' @method nobs boottest
#' #' @return A scalar containing the effective number of observations used in `boottest()`
#'
#' nobs.boottest <- function(object, ...){
#'
#'   stopifnot(inherits(object, "boottest"))
#'
#'   object$N
#'
#' }

