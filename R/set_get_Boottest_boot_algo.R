setBoottest_boot_algo <-  function(boot_algo){
  
  #' Sets the bootstrap algo to be run via `boottest()` and `waldboottest()`
  #' @param boot_algo Character scalar. Either 'R' or 'WildBootTests.jl'. Default is 'R'
  #' @return No return value 
  #' @export

  if(missing(boot_algo) || is.null(boot_algo)){
    # New default: one cores used 
    boot_algo <-  "R"
  }
  
  boot_algo <- set_boot_algo(boot_algo)
  
  options("boottest_boot_algo" = boot_algo)
  
  invisible()
}

getBoottest_boot_algo <-  function(){
  #' get the bootstrap algorithm to be run via `boottest()` and `waldboottest()`
  #' @return The number of threads currently used by boottest as set in options 
  #' @noRd
  
  x <-  getOption("boottest_boot_algo")
  if(!(x %in% c("R", "WildBootTests.jl"))){
    stop("The value of getOption(\"boottest_boot_algo\") is currently not legal. Please use function setBoottest_boot_algo to set it to an appropriate value. ")
  }
  x
}

set_boot_algo <- function(boot_algo){
  
  dreamerr::check_value(boot_algo, "charin(R, WildBootTests.jl)")

  boot_algo
  
}
