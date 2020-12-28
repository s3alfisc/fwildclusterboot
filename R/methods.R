boottest <- function(x, ...){
  #'@export 
  UseMethod("boottest")
}

boot_algo <- function(x, ...){
  #'@export 
  UseMethod("boot_algo")
}

boot_algo2 <- function(x, ...){
  #'@export 
  UseMethod("boot_algo2")
}

preprocess <- function(x, ...){
  #'@export 
  UseMethod("preprocess")
}

invert_p_val <- function(x, ...){
  #'@export 
  UseMethod("invert_p_val")
}

invert_p_val2 <- function(x, ...){
  #'@export 
  UseMethod("invert_p_val2")
}