check_r_lean <- function(clustid, fe){
  
  if(length(clustid) > 1){
    stop("The R-lean algorithm currently only supports oneway clustering.")
  }
  
  if (boot_algo == "R-lean") {
    if (!is.null(fe)) {
      stop("boottest() currently does not support fixed effects with boot_algo = 'R-lean'.")
    }
  }
  
}