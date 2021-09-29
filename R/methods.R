#' Fast wild cluster bootstrap inference
#' 
#'  
#' `boottest` is a S3 method that allows for fast wild cluster 
#' bootstrap inference for objects of class lm, fixest and felm by  implementing
#' the fast wild bootstrap algorithm developed in Roodman et al., 2019.
#' 
#' @param object An object of type lm, fixest of felm
#' @param ... other arguments
#' 
#' @seealso \code{\link{boottest.lm}}, \code{\link{boottest.fixest}} and \code{\link{boottest.felm}}
#' 
#' @export
#' 
#' @return An object of class \code{boottest}.
#' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in 
#'             STATA using boottest", The STATA Journal.
#'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
             
boottest <- function(object,
                     ...) {
  UseMethod("boottest")
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
