get_seed <- function() {
  #' creates an integer based on the global random seed set via set.seed()
  #' for using set.seed() for controlling rcpp's seed, see this
  #' blog post http://rorynolan.rbind.io/2018/09/30/rcsetseed/
  #' @noRd

  # max_int <- .Machine$integer.max
  max_int <- 2147483647L
  x <- sample.int(max_int, 1)
  x
}

set_seed <- function(seed, boot_algo, type) {
  if (!is.null(seed)) {
    if (boot_algo == "R") {
      if (type %in% c("rademacher", "webb", "norm")) {
        dqrng::dqset.seed(seed)
        internal_seed <- NULL
      } else if (type == "mammen") {
        set.seed(seed)
        internal_seed <- NULL
      }
    } else if (boot_algo == "R-lean") {
      set.seed(seed)
      internal_seed <- NULL
    } else if (boot_algo == "WildBootTests.jl") {
      JuliaConnectoR::juliaEval("using StableRNGs")
      set.seed(seed)
      seed <- get_seed()
      internal_seed <-
        JuliaConnectoR::juliaEval(paste0("rng = StableRNG(", seed, ")"))
    }
  } else if (is.null(seed)) {
    if (boot_algo == "WildBootTests.jl") {
      seed <- get_seed()
      JuliaConnectoR::juliaEval("using StableRNGs")
      internal_seed <-
        JuliaConnectoR::juliaEval(paste0("rng = StableRNG(", seed, ")"))
    } else {
      internal_seed <- NULL
    }
  }

  internal_seed
}
