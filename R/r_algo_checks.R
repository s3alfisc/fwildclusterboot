r_algo_checks <- function(R, p_val_type, conf_int, B){
  
  if(!is.null(R)){
    if(length(nrow(R)) != 0){
      stop("Hypotheses with q>1 are currently only supported via WildBootTests.jl. Please set the function argument 'boot_algo = WildBootTests.jl'.")
    }
  }
  
  if(p_val_type %in% c(">", "<")){
    if(conf_int == TRUE){
      conf_int <- FALSE
      warning(paste("Currently, boottest() calculates confidence intervals for one-sided hypotheses only for boot_algo = 'WildBootTests.jl'."), call. = FALSE)
    }
  } 
  
  
  if (conf_int == TRUE || is.null(conf_int)){
    if(B <= 100){
      stop("The function argument B is smaller than 100. The number of bootstrap 
          iterations needs to be 100 or higher in order to guarantee that the root
          finding procudure used to find the confidence set works properly.",
           call. = FALSE
      )
    }
  }
  
  
}