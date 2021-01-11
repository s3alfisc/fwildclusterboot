
p_val_null2 <- function(beta0, A, B, CC, CD, DD, clustid, boot_iter, small_sample_correction){
  
  
  #' Calculate p-values based on A, B, CC, CD, DD and other inputs
  #' @param beta0 Scalar. Shifts the null hypothesis. 
  #' @param A A list. 
  #' @param B A list. 
  #' @param CC A list. 
  #' @param CD A list. 
  #' @param DD A list. 
  #' @param clustid A data.frame containing the cluster variables. 
  #' @param boot_iter An integer. Number of bootstrap iterations. 
  #' @param small_sample_correction A vector of the dimension of ncol(clustid)
  numer <- A + B * beta0
  names_clustid <- names(clustid)
  
  JJ <- list()
  for(x in names_clustid){
    #JJ[[x]] <- colSums(small_sample_correction[x] * (CC[[x]] + 2* CD[[x]]*beta0+ DD[[x]]* beta0^2))
    JJ[[x]] <- (small_sample_correction[x] * (CC[[x]] + 2* CD[[x]]*beta0+ DD[[x]]* beta0^2))
  }

  len_names_clustid <- length(names_clustid)
  if(len_names_clustid == 1){
    JJ_sum <- JJ[["clustid"]]
  } else if(len_names_clustid > 1){
    JJ_sum <- Reduce("+", JJ)
  }
  
  denom <- suppressWarnings(sqrt(JJ_sum))

  t <- abs(numer) / denom
  delete_invalid_t_total <- sum(is.na(t))
  t <- t[!is.na(t)]
  
  t_boot <- t[2:(boot_iter + 1 - delete_invalid_t_total)]
  
  #if(p_val == "two-tailed symmetric"){
    p_val <- mean(abs(t[1] - beta0) < (t_boot))
  # } else if(p_val == "equal-tailed"){
  #   p_l <- mean((t[1] - beta0) < t_boot)
  #   p_h <- mean((t[1] - beta0) > t_boot)
  #   p_val <- 2*min(p_l, p_h)
  # } else if(p_val == "<"){
  #   p_l <- mean((t[1] - beta0) < t_boot)
  #   p_val <- p_l
  # } else if(p_val == ">"){
  #   p_h <- mean((t[1] - beta0) > t_boot)
  #   p_val <- p_h
  # }

  res <- list(p_val = p_val, 
              t = t, 
              t_boot = t_boot, 
              invalid_t = delete_invalid_t_total)
  
}

