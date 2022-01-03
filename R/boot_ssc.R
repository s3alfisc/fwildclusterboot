boot_ssc <- function(adj = TRUE, fixef.K = "none", cluster.adj = TRUE, cluster.df = "conventional"){
  
  #' set the small sample correction factor applied in `boottest()`
  #' @param adj Logical scalar, defaults to TRUE. If TRUE, applies a small sample correction of (N-1) / (N-k) where N is the number of observations and k is the number of estimated coefficients excluding any fixed effects projected out in either fixest::feols() or lfe::felm().
  #' @param fixef.K Character scalar, equal to 'none': the fixed effects parameters are discarded when calculating k in (N-1) / (N-k).
  #' @param cluster.adj Logical scalar, defaults to TRUE. If TRUE, a cluster correction G/(G-1) is performed, with G the number of clusters. 
  #' @param cluster.df Either "conventional"(the default) or "min". Controls how "G" is computed for multiway clustering if cluster.adj = TRUE. Note that the covariance matrix in the multiway clustering case is of the form V = V_1 + V_2 - V_12. If "conventional", then each summand G_i is multiplied with a small sample adjustment G_i / (G_i - 1). If "min", all summands are multiplied with the same value, min(G) / (min(G) - 1)
  #' @export
  #' @examples 
  #' boot_ssc(adj = TRUE, cluster.adj = TRUE)
  #' boot_ssc(adj = TRUE, cluster.adj = TRUE, cluster.df = "min")

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
  
  #' Compute small sample adjustment factors 
  #' @param boot_ssc_object An object of type 'boot_ssc.type'
  #' @param N The number of observations
  #' @param k The number of estimated parameters
  #' @param G The number of clusters
  #' @param vcov_sign A vector that helps create the covariance matrix
  #' @return A small sample adjustment factor
  
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
