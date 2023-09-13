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
  
  packageStartupMessage("Note that with version 0.15, rademacher weights are")
  packageStartupMessage("drawn from `dqrng::dqrrademacher()` instead of `dqrng::dqsample()`.")
  packageStartupMessage("This brings nice performance gains, but you might no longer be able to exactly reproduce")
  packageStartupMessage("your bootsrap inferences under a given seed. If this is a ")
  packageStartupMessage("requirement for you, please revert to `fwildclusterboot` version")
  packageStartupMessage("0.13-0.14.3.")
  
  packageStartupMessage("\nPlease cite as: \n")
  packageStartupMessage(" Fischer & Roodman. (2021). fwildclusterboot: Fast Wild Cluster.")
  packageStartupMessage(" Bootstrap Inference for Linear Regression Models.")
  packageStartupMessage(" Available from https://cran.r-project.org/package=fwildclusterboot/.")
  
}
