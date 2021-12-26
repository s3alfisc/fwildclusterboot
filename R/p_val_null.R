p_val_null2 <- function(beta0, A, B, CC, CD, DD, clustid, boot_iter, small_sample_correction, impose_null, point_estimate, p_val_type) {


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
  #' @param impose_null If TRUE, no null hypothesis if imposed on the bootstrap
  #' @param point_estimate The point estimate of the test parameter from the regression model.
  #' @param p_val_type type Type of p-value. By default "two-tailed". Other options: "equal-tailed", ">", "<"
  #' 
  #' @return A list containing the bootstrapped p-value, the bootstrapped t-statistics, 
  #'         and the number of invalid test statistics. 
  #' @noRd
  
  # Roodman et al on WCU: "Therefore, as we vary ßj0, the bootstrap samples
  # do not change, and hence, only one set of bootstrap samples needs to be constructed."
  # here, note that if impose_null = FALSE -> WCU: B = 0, CD = 0, DD = 0 and hence numer and
  # JJ do not vary in beta0

  numer <- A + B * beta0
  names_clustid <- names(clustid)

  JJ <- list()
  for (x in seq_along(names_clustid)) {
    # JJ[[x]] <- colSums(small_sample_correction[x] * (CC[[x]] + 2* CD[[x]]*beta0+ DD[[x]]* beta0^2))
    JJ[[x]] <- (small_sample_correction[x] * (CC[[x]] + 2 * CD[[x]] * beta0 + DD[[x]] * beta0^2))
  }

  len_names_clustid <- length(names_clustid)
  if (len_names_clustid == 1) {
    JJ_sum <- JJ[[1]]
  } else if (len_names_clustid > 1) {
    JJ_sum <- Reduce("+", JJ)
  }

  denom <- suppressWarnings(sqrt(JJ_sum))

  if (impose_null == TRUE) {
    t <- numer / denom
    t_stat <- t[1]
    t_boot <- t[2:boot_iter]
    
    # delete invalid test statistics
    delete_invalid_t_total <- sum(is.na(t_boot))
    t_boot <- t_boot[!is.na(t_boot)]
    
  } else if (impose_null == FALSE) {
    t <- numer / denom
    t[1] <- ((point_estimate - beta0) / denom[1]) #- t[1] 
    t_stat <- t[1]
    t_boot <- t[2:boot_iter]
    
    # delete invalid test statistics
    delete_invalid_t_total <- sum(is.na(t_boot))
    t_boot <- t_boot[!is.na(t_boot)]
    
  }

  # delete_invalid_t_total <- sum(is.na(t))
  # t <- t[!is.na(t)]
  # 
  # t_boot <- t[2:(boot_iter + 1 - delete_invalid_t_total)]

  # p_val_type <- "two-tailed symmetric"
  if (p_val_type == "two-tailed") {
    p_val <- mean(abs(t[1]) < abs(t_boot), na.rm = FALSE)
  } else if (p_val_type == "equal-tailed") {
    p_l <- mean(t[1] < t_boot, na.rm = FALSE)
    p_h <- mean(t[1] > t_boot, na.rm = FALSE)
    p_val <- 2 * min(p_l, p_h, na.rm = FALSE)
  } else if (p_val_type == ">") {
    p_l <- mean(t[1] < t_boot, na.rm = FALSE)
    p_val <- p_l
  } else if (p_val_type == "<") {
    p_h <- mean(t[1] > t_boot, na.rm = FALSE)
    p_val <- p_h
  }

  res <- list(
    p_val = p_val,
    t_stat = t_stat,
    t_boot = t_boot,
    invalid_t = delete_invalid_t_total
  )
}
