set_boottest_seed <- function(seed, set_julia_seed = FALSE){
  
  #' Set a seed for fwildclusterboot
  #' @param seed Integer. Sets a random seed
  #' @param WildBootTests.jl Logical. Should a Julia seed be set? Requires 
  #' installation of Julia and JuliaConnectoR. FALSE by default.
  
  set.seed(seed)
  dqrng::dqset.seed(seed)
  
  if(set_julia_seed == TRUE){
    if(require(JuliaConnectoR)){
      fwildclusterboot::set_julia_seed(seed)
    }
  }
  
}
