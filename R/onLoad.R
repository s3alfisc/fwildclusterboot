.onLoad <- function(libname, pkgname) {
  #' setting options for nthreads when package is loaded
  #' @param libname library name
  #' @param pkgname package name
  #' @return Changes number of threads used.
  #' @noRd
  
  # adds info on number of threads to options()
  # setBoottest_nthreads()
  
  setBoottest_nthreads()
  setBoottest_boot_algo()
  
  invisible()
}
