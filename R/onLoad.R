.onLoad <- function(libname, pkgname){
  #' setting options for nthreads when package is loaded
  
  # adds info on number of threads to options()
  setBoottest_nthreads()
  
  invisible()
}