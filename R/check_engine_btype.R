check_engine_btype <- function(
    engine, 
    bootstrap_type){
  
  if(engine == "WildBootTests.jl"){
    if(!(bootstrap_type %in% c("11", "fnw11", "31"))){
      stop(
        paste(
          "The bootstrap of type", 
          bootstrap_type, 
          "is not yet supported via 'WildBootTests. You can run it via 
          the 'R' engine instead.'")
      )
    }
  } else if(engine == "R-lean"){
    if(bootstrap_type != "fnw11"){
      if(bootstrap_type == "31"){
        stop(
          paste(
            "The bootstrap of type", 
            bootstrap_type, 
            "is not yet supported via 'R-lean'. You can run it via 
          the 'R' or 'WildBootTests.jl' engines instead.'")
        )
      } else {
        stop(
          paste(
            "The bootstrap of type", 
            bootstrap_type, 
            "is not yet supported via 'R-lean'. You can run it via 
          the 'R' engine instead.'")
        )
      }
      
    }
  }
  
}
