check_r_lean <- function(weights, clustid, fe){
  
  if(length(clustid) > 1){
    stop("The R-lean algorithm currently only supports oneway clustering.")
  }
  
  if (!is.null(fe)) {
    stop("boottest() currently does not support fixed effects with boot_algo = 'R-lean'.")
  }
  
  if(!is.null(weights)){
    stop("boottest() currently does not support regression weights with boot_algo = 'R-lean'.")
  }
  
}