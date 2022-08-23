is_juliaconnector_prepared <- function(){
  
  find_julia_bindir <- tryCatch(
    expr = {
      # has JULIA_BINDIR env variable been set?
      Sys.getenv()[["JULIA_BINDIR"]]
    }, 
    error = function(e){
      message(
        strwrap(
          "The Julia environment variable 'JULIA_BINDIR' could not be found.  
      Therefore, all tests that involve 'WildBootTests.jl' will be 
      skipped."
        ), 
        prefix = " ", 
        initial = " "
      )
      return(FALSE)
    }
  )
  
  is_juliaconnector_prepared <- 
    ifelse(
      find_julia_bindir != FALSE, 
      TRUE, 
      FALSE
      )
  
  # hard code to always run Julia tests
  is_juliaconnector_prepared <- TRUE
  
  is_juliaconnector_prepared
  
}
