.onLoad <- function(libname, pkgname) {
  #' setting options for nthreads when package is loaded
  #' @param libname library name
  #' @param pkgname package name
  #' @return Changes number of threads used.
  #' @noRd

  # adds info on number of threads to options()
  # setBoottest_nthreads()

  setBoottest_nthreads()
  setBoottest_engine()

  invisible()
}

.onAttach <- 
function(libname, pkgname) {
  packageStartupMessage("\nPlease cite as: \n")
  packageStartupMessage(" Fischer & Roodman. (2021). fwildclusterboot: Fast Wild Cluster.")
  packageStartupMessage(" Bootstrap Inference for Linear Regression Models.")
  packageStartupMessage(" Available from https://cran.r-project.org/package=fwildclusterboot\.")
}
