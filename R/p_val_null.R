
p_val_null2 <- function(beta0, A, B, CC, CD, DD, clustid, boot_iter, small_sample_correction){
  
  numer <- A + B * beta0
  names_clustid <- names(clustid)
  
  JJ <- list()
  for(x in names_clustid){
    JJ[[x]] <- colSums(small_sample_correction[x] * (CC[[x]] + 2* CD[[x]]*beta0+ DD[[x]]* beta0^2))
  }

  len_names_clustid <- length(names_clustid)
  if(len_names_clustid == 1){
    JJ_sum <- unlist(JJ)
  } else if(len_names_clustid > 1){
    JJ_sum <- Reduce("+", JJ)
  }
  
  denom <- suppressWarnings(sqrt(JJ_sum))

  t <- abs(numer) / denom
  delete_invalid_t_total <- sum(is.na(t))
  t <- t[!is.na(t)]
  
  t_boot <- t[2:(boot_iter + 1 - delete_invalid_t_total)]
  p_val <- mean(abs(t[1] - beta0) < (t_boot))
  
  res <- list(p_val = p_val, 
              t = t, 
              t_boot = t_boot, 
              invalid_t = delete_invalid_t_total)
  
}

