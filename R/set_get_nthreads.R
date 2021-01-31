# Functions in this script are written by Laurent Berge, 
# and stem originally from the fixest R package. They 
# are slightly adapted to fit the needs of boottest.


setBoottest_nthreads = function(nthreads){
  # By default, we use only 50% of threads (never use all)
  
  max_CRAN = as.numeric(Sys.getenv("OMP_THREAD_LIMIT"))
  max_CRAN[is.na(max_CRAN)] = 1000
  
  max_threads = min(cpp_get_nb_threads(), 1000, max_CRAN) # we cap at 1k nthreads
  
  if(missing(nthreads) || is.null(nthreads)){
    # New default => 50% of all available threads (usually equiv to the nber of procs)
    nthreads = check_set_nthreads(0.5)
  }
  
  nthreads = check_set_nthreads(nthreads)
  
  options("boottest_nthreads" = nthreads)
  
  invisible()
}


getBoottest_nthreads = function(){
  
  x = getOption("boottest_nthreads")
  if(length(x) != 1 || !is.numeric(x) || is.na(x) || x %% 1 != 0 || x < 0){
    stop("The value of getOption(\"boottest_nthreads\") is currently not legal. Please use function setFixest_nthreads to set it to an appropriate value. ")
  }
  
  x
}

check_set_nthreads = function(nthreads){
  # Simple function that checks that the nber of threads is valid
  dreamerr::set_up(1)
  
  dreamerr::check_value(nthreads, "integer scalar GE{0} | numeric scalar GT{0} LT{1}", .message = paste0("The argument 'nthreads' must be an integer lower or equal to the number of threads available (", max(cpp_get_nb_threads(), 1), "). It can be equal to 0 which means all threads. Alternatively, if equal to a number strictly between 0 and 1, it represents the fraction of all threads to be used."))
  
  max_threads = cpp_get_nb_threads()
  
  # # To add later
  # if(cpp_is_in_fork()) return(1)
  
  if(nthreads == 0){
    nthreads = max(max_threads, 1)
    
  } else if(nthreads < 1){
    nthreads = max(ceiling(max_threads * nthreads), 1)
    
  } else if(nthreads > 1){
    if(max_threads == 0){
      dreamerr::warn_up("OpenMP not detected: cannot use ", nthreads, " threads, single-threaded mode instead.")
      nthreads = 1
    } else if(nthreads > max_threads){
      dreamerr::warn_up("Asked for ", nthreads, " threads while the maximum is ", max_threads, ". Set to ", max_threads, " threads instead.")
      nthreads = max_threads
    }
    
  }
  
  nthreads
}
