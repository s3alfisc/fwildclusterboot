set_julia_seed <- function(rng){

  #' Set a global seed in the current Julia session
  #' @param rng An integer that controls the random number generation
  #' @export

  JuliaConnectoR::juliaEval('using Random')
  rng_char <- paste0("Random.seed!(", rng, ")")
  JuliaConnectoR::juliaEval(rng_char)

}
