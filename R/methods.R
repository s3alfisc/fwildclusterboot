boottest <- function(object,
                     ...) {
  #' 
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
  

  UseMethod("boottest")
}

#' tidy <- function(object, ...) {
#'   #' S3 method to summarize objects of class boottest into tidy data.frame
#'   #' @export
#'   #' @param object object of type boottest
#'   #' @param ... other arguments
#'   UseMethod("tidy")
#' }
#' 
#' glance <- function(object, ...) {
#'   #' S3 method to summarize objects of class boottest
#'   #' only needed for use with modelsummary package
#'   #' broom needs to be imported
#'   #' @export
#'   #' @param object object of type boottest
#'   #' @param ... other arguments
#'   UseMethod("glance")
#' }
