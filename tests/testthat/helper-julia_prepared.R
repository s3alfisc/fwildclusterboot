juliaconnector_prepared <- tryCatch(
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

juliaconnector_prepared <- ifelse(juliaconnector_prepared != FALSE, TRUE, FALSE)
