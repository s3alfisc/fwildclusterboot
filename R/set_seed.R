get_seed <- function() {

  #' creates an integer based on the global random seed set via set.seed()
  #' for using set.seed() for controlling rcpp's seed, see this
  #' blog post http://rorynolan.rbind.io/2018/09/30/rcsetseed/

  #max_int <- .Machine$integer.max
  max_int <- 2147483647L
  x <- sample.int(max_int, 1)
  x
}
