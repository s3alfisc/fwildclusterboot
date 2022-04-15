setBoottest_internal_seed <- function(internal_seed){
  
  #' Set global variable whether `boottest()` / `mboottest()` should use an internal seed. 
  #' @param internal_seed Logical scalar. If TRUE, use internal seed for all 
  #'        calls of boottest() unless specified otherwise
  #' @export

  if (missing(internal_seed) || is.null(internal_seed)) {
    # New default: one cores used
    internal_seed <- TRUE
  }
  
  internal_seed <- set_internal_seed(internal_seed)
  
  options("boottest_internal_seed" = internal_seed)
  
  
  
}

getBoottest_internal_seed <- function() {
  #' get the bootstrap algorithm to be run via `boottest()` and `waldboottest()`
  #' @return The number of threads currently used by boottest as set in options
  #' @noRd
  
  x <- getOption("boottest_internal_seed")
  if (!(x %in% c(TRUE, FALSE))) {
    stop("The value of getOption(\"boottest_boot_algo\") is currently not legal. Please use function setBoottest_internal_seed to set it to an appropriate value. ")
  }
  x
}

set_internal_seed <- function(set_internal_seed) {
  dreamerr::check_value(set_internal_seed, "logical scalar")
  set_internal_seed
}
