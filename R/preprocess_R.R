process_R <- function(R, param){
  # check R & param
  if(is.null(R)){
    R <- rep(1, length(param))
  } else {
    if(length(R) != length(param)){
      stop("The constraints vector must either be NULL or a numeric of the same length as the `param` input vector.")
    }
  }
  R
}