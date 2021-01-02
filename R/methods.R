boottest <- function(object, ...){
  #' S3 Method boottest
  #'@export 
  #'@param object An object of type lm, fixest of felm
  #'@param ... other arguments
  UseMethod("boottest")
}

boot_algo <- function(preprocessed_object, ...){
  #' S3 Method boot_algo
  #'@export 
  #'@param preprocessed_object An object of type preprocess_lm, preprocess_felm or preprocess_fixest
  #'@param ... other arguments
  UseMethod("boot_algo")
}

boot_algo2 <- function(preprocessed_object, ...){
  #' S3 Method boot_algo2
  #'@export 
  #'@param preprocessed_object An object of type preprocess_lm, preprocess_felm or preprocess_fixest
  #'@param ... other arguments
  UseMethod("boot_algo2")
}

preprocess <- function(object, ...){
  #' S3 Method preprocess
  #'@export 
  #'@param object An object of type lm, fixest of felm
  #'@param ... other arguments
  UseMethod("preprocess")
}

invert_p_val <- function(object, ...){
  #' S3 Method invert_p_val
  #'@export 
  #'@param object An object of type boot_algo_oneclust or boot_algo_multclust
  #'@param ... other arguments
  UseMethod("invert_p_val")
}

invert_p_val2 <- function(object, ...){
  #' S3 Method invert_p_val2
  #'@export 
  #'@param object An object of type boot_algo_oneclust or boot_algo_multclust
  #'@param ... other arguments
  UseMethod("invert_p_val2")
}

invert_p_val2a <- function(object, ...){
  #' S3 Method invert_p_val2
  #'@export 
  #'@param object An object of type boot_algo_oneclust or boot_algo_multclust
  #'@param ... other arguments
  UseMethod("invert_p_val2a")
}