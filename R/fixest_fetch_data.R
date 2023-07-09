fetch_data = function(x, prefix = "", suffix = ""){
  
  #' fetch data from environment
  #' @noRd

  # x: fixest estimation
  # We try different strategies:
  # 1) using the environment where the estimation was done
  # 2) the "parent.frame()" defined as the frame on top of ALL fixest functions
  # 3) the global environment, if it wasn't in 1)
  
  # Maybe I should keep only 1) => is there a reason to add the others?
  
  # 1) safest
  # 2) less safe but OK => note ???
  # 3) kind of dangerous => warning() ???
  
  if(is.null(x$call$data)) return(NULL)
  
  # 1) Environment of the call
  
  data = NULL
  try(data <- eval(x$call$data, x$call_env), silent = TRUE)
  
  if(!is.null(data)){
    return(data)
  }
  
  # 2) First non fixest frame
  
  fixest_funs = ls(getNamespace("fixest"))
  
  i = 2
  sysOrigin = sys.parent(i)
  while(sysOrigin != 0 && as.character(sys.call(sysOrigin)[[1]]) %in% fixest_funs){
    i = i + 1
    sysOrigin = sys.parent(i)
  }
  
  if(sysOrigin != 0){
    # We try again...
    try(data <- eval(x$call$data, parent.frame(sysOrigin)), silent = TRUE)
    
    if(!is.null(data)){
      return(data)
    }
  }
  
  # 3) Global environment
  
  if(!identical(parent.env(x$call_env), .GlobalEnv)){
    # ...and again
    try(data <- eval(x$call$data, .GlobalEnv), silent = TRUE)
    
    if(!is.null(data)){
      return(data)
    }
  }
  
  # => Error message
  
  if(nchar(prefix) == 0){
    msg = "W"
  } else {
    s = ifelse(grepl(" $", prefix), "", " ")
    if(grepl("\\. *$", prefix)){
      msg = paste0(prefix, s, "W")
    } else {
      msg = paste0(prefix, s, "w")
    }
  }
  
  if(nchar(prefix) == 0){
    msg = "W"
  } else if(grepl("\\. *$", prefix)){
    msg = paste0(gsub(" +$", "", prefix), " W")
  } else {
    msg = paste0(gsub(prefix, " +$", ""), " w")
  }
  
  if(nchar(suffix) > 0){
    suffix = gsub("^ +", "", suffix)
  }
  
  stop_up(msg, "e fetch the data in the enviroment where the estimation was made, but the data does not seem to be there any more (btw it was ", charShorten(deparse(x$call$data)[1], 15), "). ", suffix)
  
  
}