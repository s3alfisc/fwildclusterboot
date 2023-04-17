#' Pairs Cluster Bootstrap as in MacKinnon "Fast Cluster Bootstraps" 
#' (Econometrics & Statistics, 2021)
#'
#' @param preprocessed_object A list: output of the preprocess2 function.
#' @param B number of bootstrap iterations
#' @param r Shifts the null hypothesis.
#' @param sign_level The significance level.
#' @param param name of the test parameter.
#' @param p_val_type type Type of p-value. By default "two-tailed".
#' Other options: "equal-tailed", ">", "<"
#' @param small_sample_correction The small sample correction to be applied.
#' See ssc().
#' If FALSE, run wild cluster bootstrap.
#' @param object the regression object
#' @return A list of bootstrap results. 
#' @noRd


boot_algo_pairs <- function(
    preprocessed_object,
    B,
    r = 0,
    sign_level,
    param,
    p_val_type,
    small_sample_correction,
    object){
  
  
  X <- preprocessed_object$X
  y <- preprocessed_object$Y
  R <- preprocessed_object$R0
  cluster_df <- preprocessed_object$clustid
  clustid <- names(cluster_df)
  fe <- preprocessed_object$fe
  cluster <- as.factor(cluster_df[,1])
  bootcluster <- preprocessed_object$bootcluster
  G <- N_G_bootcluster <- length(unique(bootcluster[[1]]))
  k <- length(R)
  
  # create X_g's, X1_g's, y_g's etc
  X_list <- matrix_split(X, cluster, "row")
  y_list <- split(y, cluster, drop = FALSE)
  
  # precompute a range of other objects
  tXgXg <- lapply(
    seq_along(1:G),
    function(g) (crossprod(X_list[[g]]))
  )
  
  tXgyg <- lapply(
    seq_along(1:G),
    function(g) t(X_list[[g]]) %*% y_list[[g]]
  )
  
  
  #tXX <- Reduce("+", tXgXg) # crossprod(X)
  #tXy <- Reduce("+", tXgyg) # t(X) %*% y
  
  
  t_boot <- rep(NA, B+1)
  for(b in 1:(B+1)){
    
    if(b == 1){
      b_index <- 1:G
    } else {
      b_index <- sample(1:G, G, TRUE)
    }
    
    b_tXX <- Reduce("+", tXgXg[b_index]) 
    b_tXX_inv <- solve(b_tXX)
    b_tXy <- Reduce("+",tXgyg[b_index]) 
    b_coef <- b_tXX_inv %*% b_tXy 
    
    b_meat <- list()
    for(g in seq_along(b_index)){
      
      b_score <- tXgyg[[g]] - tXgXg[[g]] %*% b_coef
      b_meat[[g]] <- tcrossprod(b_score)
    }
    
    b_meat <- Reduce("+",b_meat)
    b_vcov <- small_sample_correction * b_tXX_inv %*% b_meat %*% b_tXX_inv 
    
    t_boot[b] <- R %*% b_coef / sqrt(R %*% b_vcov %*% R)
    
  }
  
  t_stat <- t_boot[1]
  t_boot <- t_boot[2:(B+1)]
  
  
  p_val <-
    get_bootstrap_pvalue(
      p_val_type = p_val_type,
      t_stat = t_stat,
      t_boot = t_boot
    )
  
  res <- list(
    p_val = p_val,
    t_stat = t_stat,
    t_boot = t_boot,
    B = B,
    R0 = R,
    param = param,
    clustid = clustid,
    invalid_t = NULL,
    ABCD = NULL,
    small_sample_correction = small_sample_correction
  )
  
  class(res) <- "boot_algo_pairs"
  
  invisible(res)
  
}
