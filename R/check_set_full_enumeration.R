# diverse helper functions 
check_set_full_enumeration <- function(heteroskedastic = FALSE, preprocess, B, type){
  
  full_enumeration <- FALSE
  if(heteroskedastic == FALSE){
    N_G_bootcluster <- preprocess$N_G_bootcluster
    N_G_2 <- 2^N_G_bootcluster      
    if (type == "rademacher") {
      if(N_G_2 <= B){
        warning(paste("There are only", N_G_2, "unique draws from the rademacher distribution for", N_G_bootcluster, "bootstrap clusters. Therefore, B = ", N_G_2, " with full enumeration. Consider using webb weights instead. Further, note that under full enumeration and with B =", N_G_2, "bootstrap draws, only 2^(#clusters - 1) = ", 2^(N_G_bootcluster - 1), " distinct t-statistics and p-values can be computed. For a more thorough discussion, see Webb `Reworking wild bootstrap based inference for clustered errors` (2013)."),
                call. = FALSE, 
                noBreaks. = TRUE
        )
        B <- N_G_2
        full_enumeration <- TRUE
      }
    } 
  }  
  
  res <- list(B = B,
              full_enumeration = full_enumeration)
  
  res
  
}

