#' set_seed <- function(seed, boot_algo, type){
#'   
#'   #' Function sets seed in boottest() in alignment with 
#'   #' global seed set via set.seed()
#'   #' 
#'   #' @param seed A random seed provided to boottest() via the seed argument. 
#'   #'             NULL if no seed is provided
#'   #' @param boot_algo The bootstrap algorithm used in boottest()
#'   #' @param type The type of the bootstrap weights
#'   
#'   # seed_provided <- !is.null(seed)
#'   
#'   if(is.null(seed)){
#'     internal_seed <- get_seed()
#'   } else {
#'     set.seed(seed)
#'     internal_seed <- get_seed()
#'   }
#'   
#'   cat(internal_seed, "\n")
#'   
#'   if(boot_algo == "R"){
#'     if(type %in% c("rademacher", "webb", "norm")){
#'       dqrng::dqset.seed(internal_seed)
#'     } else {
#'       set.seed(internal_seed)
#'     }
#'   } else if(boot_algo == "R-lean"){
#'     set.seed(internal_seed)
#'   } else if(boot_algo == "WildBootTests.jl"){
#'     JuliaConnectoR::juliaEval('using Random')
#'     rng_char <- paste0("Random.seed!(", internal_seed, ")")
#'     JuliaConnectoR::juliaEval(rng_char)
#'     internal_seed <- JuliaConnectoR::juliaEval(paste0("Random.MersenneTwister(", as.integer(internal_seed),")"))
#'   }
#'   
#'   invisible(internal_seed) 
#' 
#' }

# for using set.seed() for controlling rcpp's seed, see this 
# blog post http://rorynolan.rbind.io/2018/09/30/rcsetseed/
get_seed <- function() {
  x <- sample.int(.Machine$integer.max, 1)
  return(x)
}
