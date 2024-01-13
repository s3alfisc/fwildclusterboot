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
    packageStartupMessage(
      "\nPlease cite as: \n",
      "Fischer & Roodman. (2021). fwildclusterboot: Fast Wild Cluster.",
      "Bootstrap Inference for Linear Regression Models. \n",
      "Available from https://cran.r-project.org/package=fwildclusterboot/. \n"
    )
    packageStartupMessage(
      "Too guarantee reproducibility, please don't forget to set a ",
      "global random seed **both** via `set.seed()` and `dqrng::dqset.seed()`. ",
      "This is required as `boottest()` uses different random number generators ",
      "for different algorithms."
    )
  }
