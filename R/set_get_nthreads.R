# The original source of all three included funcitons is Laurent Berge's fixest
# code in https://github.com/lrberge/fixest
# The original code was distributed under GPL-3 license

 
# changes to these functions by Alexander Fischer
# 1. functions are renamed to _Boottest_
# 2. the default number of threads in check_set_nthreads is set to 1

setBoottest_nthreads <-  function(nthreads){
  #' Set the number of threads for use with open m 
  #' By default, we use only 50% of threads (never use all)
  #' @param nthreads Integer. Number of threads to be used
  #' @export
  
  
  max_CRAN <-  as.numeric(Sys.getenv("OMP_THREAD_LIMIT"))
  max_CRAN[is.na(max_CRAN)] <-  1000
  
  max_threads <-  min(cpp_get_nb_threads(), 1000, max_CRAN) # we cap at 1k nthreads
  
  if(missing(nthreads) || is.null(nthreads)){
    # New default: one cores used 
    nthreads <-  1
  }
  
  nthreads <-  check_set_nthreads(nthreads)
  
  options("boottest_nthreads" = nthreads)
  
  invisible()
}


getBoottest_nthreads <-  function(){
  #' get the number of threads for use with open mp
  x <-  getOption("boottest_nthreads")
  if(length(x) != 1 || !is.numeric(x) || is.na(x) || x %% 1 != 0 || x < 0){
    stop("The value of getOption(\"boottest_nthreads\") is currently not legal. Please use function setBoottest_nthreads to set it to an appropriate value. ")
  }
  #cat("getBoottest nr threads \n")
  #print(x)
  x
}

check_set_nthreads <-  function(nthreads){
  #' Simple function that checks that the nber of threads is valid
  #' @param nthreads Integer. Number of threads to be used
  #' @importFrom dreamerr set_up check_value warn_up
  
  dreamerr::set_up(1)
  
  dreamerr::check_value(nthreads, "integer scalar GE{0} | numeric scalar GT{0} LT{1}", .message = paste0("The argument 'nthreads' must be an integer lower or equal to the number of threads available (", max(cpp_get_nb_threads(), 1), "). It can be equal to 0 which means all threads. Alternatively, if equal to a number strictly between 0 and 1, it represents the fraction of all threads to be used."))
  
  max_threads <-  cpp_get_nb_threads()
  #cat("max_threads \n")
  #print(max_threads)
  
  # # To add later
  # if(cpp_is_in_fork()) return(1)
  
  if(nthreads == 0){
    nthreads <-  max(max_threads, 1)
    
  } else if(nthreads < 1){
    nthreads <-  max(ceiling(max_threads * nthreads), 1)
    
  } else if(nthreads > 1){
    if(max_threads == 0){
      dreamerr::warn_up("OpenMP not detected: cannot use ", nthreads, " threads, single-threaded mode instead.")
      nthreads <-  1
    } else if(nthreads > max_threads){
      dreamerr::warn_up("Asked for ", nthreads, " threads while the maximum is ", max_threads, ". Set to ", max_threads, " threads instead.")
      nthreads <-  max_threads
    }
    
  }
  #cat("nthreads set_get \n")
  #print(nthreads)
  nthreads
}


