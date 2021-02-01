.onLoad <- function(libname, pkgname){
  #' setting options for nthreads when package is loaded
  #' @param libname library name
  #' @param pkgname package name
  
  # adds info on number of threads to options()
  setBoottest_nthreads()
  
  invisible()
}