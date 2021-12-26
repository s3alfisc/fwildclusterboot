boot_ssc <- function(adj = FALSE, fixef.K = "none", cluster.adj = TRUE, cluster.df = "conventional"){
  
  #' boot_ssc() handles the small sample correction factor applied in boottest() in 
  #' analogy to fixest::ssc()
  #' @param adj. Logical scalar, defaults
  #' @param fixef.k
  #' @param cluster.adj
  #' @param cluster.df
  #' @export

  dreamerr::check_arg_plus(adj, "loose logical scalar conv")
  dreamerr::check_arg_plus(fixef.K, "match(none, full)")
  dreamerr::check_arg_plus(cluster.df, "match(conventional, min)")
  dreamerr::check_arg(cluster.adj, "logical scalar")
  

  res <- 
  list(adj = adj, 
       fixef.K = fixef.K, 
       cluster.adj = cluster.adj,
       cluster.df = cluster.df)
  
  class(res) <- "boot_ssc.type"
  
  res

}

get_ssc <- function(boot_ssc_object, N, k, G, vcov_sign){
  
  #' @param N
  #' @param k
  #' @param G
  #' @param vcov_sign
  
  adj <- boot_ssc_object$adj
  fixef.K <- boot_ssc_object$fixef.K
  cluster.adj <- boot_ssc_object$cluster.adj
  cluster.df <- boot_ssc_object$cluster.df
  
  # cat("--------------------------------", "\n")
  # 
  # cat("adj:", adj, "\n")
  # cat("fixef.K:", fixef.K, "\n")
  # cat("cluster.adj:", cluster.adj, "\n")
  # cat("cluster.df:", cluster.df, "\n")
  # 
  # cat("--------------------------------", "\n")
  # 
  # cat("N:", N, "\n")
  # cat("k:", k, "\n")
  # cat("G:", G, "\n")
  # cat("vcov_sign:", vcov_sign, "\n")
  # 
  # cat("--------------------------------", "\n")
  
  if(adj == TRUE){
    #if(fixef.K == 'none'){
      adj <- (N-1) / (N-k)
    #} else {
    #  adj <- (N-1) / N
    #}
  } else {
    adj <- 1
  }
  
  if(cluster.adj == TRUE){
    if(cluster.df == "conventional"){
      cluster.adj <- G / (G - 1) 
    } else if(cluster.df == "min"){
      G <- min(G)
      cluster.adj <- G / (G - 1) 
    }
  } else {
    cluster.adj <- 1 
  }
  
  # cat("updated parameters", "\n")
  # cat("adj update:", adj, "\n")
  # cat("cluster.adj update:", cluster.adj, "\n")
  
  small_sample_correction <- adj * cluster.adj * vcov_sign
  small_sample_correction
  
  
}
