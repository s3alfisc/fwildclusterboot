get_seed <- function() {
  
  #' creates an integer based on the global random seed set via set.seed()
  #'for using set.seed() for controlling rcpp's seed, see this 
  #' blog post http://rorynolan.rbind.io/2018/09/30/rcsetseed/
  
  x <- sample.int(.Machine$integer.max, 1)
  x
}
