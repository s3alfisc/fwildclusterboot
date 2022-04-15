set_get_internal_seed <- function(internal_seed = TRUE, seed = NULL, boot_algo, type){
  
  if(internal_seed == TRUE){
    if (is.null(seed)) {
      internal_seed <- get_seed()
    } else {
      set.seed(seed)
      internal_seed <- get_seed()
    }
    
    if (boot_algo == "R") {
      if (type %in% c("rademacher", "webb", "norm")) {
        dqrng::dqset.seed(internal_seed)
      } else {
        set.seed(internal_seed)
      }
    } else if (boot_algo == "R-lean") {
      set.seed(internal_seed)
    } else if (boot_algo == "WildBootTests.jl") {
      JuliaConnectoR::juliaEval('using StableRNGs')
      internal_seed <- JuliaConnectoR::juliaEval(paste0("rng = StableRNG(",internal_seed,")"))
    }
  } else if(internal_seed == FALSE){
    if(!is.null(seed)){
      if(boot_algo == "R"){
        if (type %in% c("rademacher", "webb", "norm")) {
          dqrng::dqset.seed(seed)
          internal_seed <- NULL
        } else if (type == "mammen"){
          set.seed(seed)
          internal_seed <- NULL
        }   
      } else if(boot_algo == "R-lean"){
        set.seed(seed)
      } else if(boot_algo == "WildBootTests.jl"){
        JuliaConnectoR::juliaEval('using StableRNGs')
        internal_seed <- JuliaConnectoR::juliaEval(paste0("rng = StableRNG(",seed,")"))
      }
    } else {
      internal_seed <- NULL
    }
  }
  
  internal_seed
}
