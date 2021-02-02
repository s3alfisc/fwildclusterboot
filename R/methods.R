boottest <- function(object, ...) {
  #' S3 Method boottest
  #' @export
  #' @param object An object of type lm, fixest of felm
  #' @param ... other arguments
  UseMethod("boottest")
}

tidy <- function(object, ...) {
  #' S3 method to summarize objects of class boottest into tidy data.frame
  #' @export
  #' @param object object of type boottest
  #' @param ... other arguments
  UseMethod("tidy")
}