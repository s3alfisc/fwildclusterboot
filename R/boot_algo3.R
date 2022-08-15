boot_algo3 <- function(preprocessed_object, 
                       boot_iter, 
                       bootstrap_type, 
                       r = 0, 
                       sign_level, 
                       param,
                       p_val_type,
                       nthreads,
                       type,
                       full_enumeration,
                       small_sample_correction,
                       heteroskedastic,
                       seed, 
                       object){
  
  #' WRE13, WRE33, WRU13 and WRU33 bootstraps as in MNW (2022) "Fast and 
  #' reliable" 
  #'
  #' @param preprocessed_object A list: output of the preprocess2 function.
  #' @param boot_iter number of bootstrap iterations
  #' @param r Shifts the null hypothesis.
  #' @param sign_level The significance level.
  #' @param param name of the test parameter.
  #' @param p_val_type type Type of p-value. By default "two-tailed".
  #' Other options: "equal-tailed", ">", "<"
  #' @param nthreads The number of threads. Can be: a) an integer lower than,
  #'                 or equal to, the maximum number of threads; b) 0: meaning
  #'                 all available threads will be used; c) a number strictly
  #'                 between 0 and 1 which represents the fraction of all
  #'                 threads to use. The default is to use 50\% of all
  #'                 threads. You can set permanently the number of threads
  #'                 used within this package using the function ...
  #' @param type character or function. The character string specifies the
  #'        type of boostrap to use: One of "rademacher", "mammen", "norm"
  #'        and "webb". Alternatively, type can be a function(n) for drawing
  #'        wild bootstrap factors. "rademacher" by default.
  #' @param full_enumeration Is full enumeration employed? Full enum.
  #' is used if N_G^2 < boot_iter for Mammen and Rademacher weights
  #' @param small_sample_correction The small sample correction to be applied.
  #' See ssc().
  #' @param heteroskedastic Logical - if TRUE, run a heteroskedastic.
  #' If FALSE, run wild cluster bootstrap.
  #' @param seed Integer scalar. Either set via boottest()'s seed argument
  #' or inherited from R's global seed (set via set.seed)
  #' @param object the regression object
  #' @return A list of ...
  #' @importFrom summclust summclust coeftable
  #' @noRd
  
  #check_arg(bootstrap_type, "charin(WCR13, WCR33, WCU13, WCU33)")
  # other checks: only test of one param, no weights, no fixed effects, 
  # ...
  
  if(substr(bootstrap_type, 5, 5) == 1){
    crv_type <- "crv1"  
  } else {
    crv_type <- "crv3"
  }
  
  #preprocessed_object <- preprocess
  
  X <- preprocessed_object$X
  y <- preprocessed_object$Y
  R <- preprocessed_object$R0
  cluster_df <- preprocessed_object$clustid
  clustid <- names(cluster_df)
  cluster <- as.factor(cluster_df[,1])
  bootcluster <- preprocessed_object$bootcluster
  G <- N_G_bootcluster <- length(unique(bootcluster[[1]]))
  k <- length(R)
  
  v <- get_weights(
    type = type, 
    full_enumeration = full_enumeration, 
    N_G_bootcluster = N_G_bootcluster, 
    boot_iter = boot_iter
  )
  
  v3 <<- v
  
  # X1: X without parameter beta for which hypothesis beta = 0 is tested
  X1 <- X[, which(R == 0)]
  
  # create X_g's, X1_g's, y_g's etc 
  X_list <- matrix_split(X, cluster, "row")
  X1_list <- matrix_split(X1, cluster, "row")
  y_list <- split(y, cluster, drop = FALSE)
  
  # precompute a range of other objects
  tXgXg <- lapply(
    seq_along(1:G),
    function(g) crossprod(X_list[[g]])
  )
  
  tX1gX1g <- lapply(
    seq_along(1:G),
    function(g) crossprod(X1_list[[g]])
  )
  
  tX1gyg <- lapply(
    seq_along(1:G),
    function(g) t(X1_list[[g]]) %*% y_list[[g]]
  )
  
  tXgyg <- lapply(
    seq_along(1:G),
    function(g) t(X_list[[g]]) %*% y_list[[g]]
  )
  
  
  tXgX1g <- lapply(
    seq_along(1:G),
    function(g) t(X_list[[g]]) %*% X1_list[[g]]
  )
  
  tXX <- Reduce("+", tXgXg) # crossprod(X)  
  tX1X1 <- Reduce("+", tX1gX1g) # crossprod(X1)
  tX1y <- Reduce("+", tX1gyg) #t(X1) %*% y
  tXy <- Reduce("+", tXgyg) # t(X) %*% y
  
  tXXinv <- solve(tXX)
  tX1X1inv <- solve(tX1X1)
  
  # OLS estimate, ROLS estimate
  beta_hat <- tXXinv %*% tXy # mean(c(beta_hat) - coef(object)) #essentially zero
  beta_tilde <- beta_hat - tXXinv %*% R %*% solve(t(R) %*% tXXinv %*% R) %*% (R %*% beta_hat - 0)
  
  # precompute more objects 
  
  inv_tXX_tXgXg <- lapply(
    1:G,
    function(x) MASS::ginv(tXX - tXgXg[[x]])
  )
  
  beta_1g_tilde <- lapply(
    1:G, 
    function(g) MASS::ginv(tX1X1 - tX1gX1g[[g]]) %*% (tX1y - tX1gyg[[g]])
  )
  
  beta_g_hat <- lapply(
    1:G, 
    function(g) MASS::ginv(tXX - tXgXg[[g]]) %*% (tXy - tXgyg[[g]])
  )
  
  Ag <- lapply(
    1:G, 
    function(g) tXgXg[[g]] %*% tXXinv
  )
  
  if(crv_type == "crv1"){
    
    score_hat_g <- lapply(
      1:G, 
      function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat
    )
    
  }
  
  scores_list <- get_scores(
    bootstrap_type = bootstrap_type,
    G = G, 
    tXgyg = tXgyg, 
    tXgXg = tXgXg, 
    tXgX1g = tXgX1g, 
    beta_hat = beta_hat, 
    beta_tilde = beta_tilde, 
    beta_g_hat = beta_g_hat, 
    beta_1g_tilde = beta_1g_tilde
  )
  
  # pre-allocate space for bootstrap 
  # start the bootstrap loop 
  t_boot <- vector(mode = "numeric", boot_iter + 1)
  dim(R) <- c(1, k) # turn R into matrix
  
  print(colMeans(v[,1:5]))
  
  for(b in 1:(boot_iter + 1)){
    
    # Step 1: get bootstrapped scores
    
    scores_g_boot <- matrix(NA,G,k)
    
      for(g in 1:G){
        scores_g_boot[g,] <- scores_list[[g]] * v[g, b]
      }
    
      # numerator (both for WCR, WCU)
      scores_boot <- colSums(scores_g_boot)
      delta_b_star <- tXXinv %*% scores_boot 
    
    # Step 2: get bootstrapped vcov's
    
    if(crv_type == "crv1"){
      
      score_hat_g_boot <- list()
      for(g in 1:G){
        # see MacKinnon (https://www.econstor.eu/bitstream/10419/247206/1/qed-wp-1465.pdf)
        # equ (20), note this can be accelerated
        score_hat_g_boot[[g]] <- tcrossprod(scores_g_boot[g,] - Ag[[g]] %*% scores_boot)
      }
      
      score_hat_boot <- Reduce("+", score_hat_g_boot)
      
      # all.equal(tXXinv %*% score_hat_boot %*% tXXinv,
      #           sandwich::vcovCL(
      #             object,
      #             cluster = reformulate(clustid),
      #             cadjust = FALSE,
      #             type = "HC0"
      #             )
      #           )

      # all.equal(tXXinv, solve(crossprod(X)))
      se <- se2 <- 
        sqrt(
          small_sample_correction * 
          (R %*% tXXinv) %*% score_hat_boot %*% (tXXinv %*% t(R))
        )
      
      t_boot[b] <- (c(delta_b_star)[which(R == 1)] / se)
      
    } else if (crv_type == "crv3"){
      
      delta_diff <- matrix(NA, G, k)
      
      for(g in 1:G){
        score_diff <- scores_boot - scores_g_boot[g,]
        delta_diff[g,] <- 
          
          (
            (inv_tXX_tXgXg[[g]] %*% score_diff) - delta_b_star
          )^2
      }
      
      se <- se3 <- 
         
        sqrt( 
          ((G-1) / G) *
          colSums(
            delta_diff
          )
        )
      
      t_boot[b] <- c(delta_b_star)[which(R == 1)] / se[which(R == 1)]
    }
 
  }  
  

  # get original t-stat
  if(crv_type == "crv1"){
    
    vcov <- 
      #small_sample_correction * 
      sandwich::vcovCL(
        object, 
        cluster = reformulate(clustid), 
        cadjust = FALSE, 
        type = "HC0"
      )
    t_stat <- coef(object)[which(R == 1)] / sqrt(diag(vcov)[which(R == 1)])
    
  } else if (crv_type == "crv3"){
    t_stat <- summclust::summclust(
      object, type = "CRV3", cluster = reformulate(clustid)
    )
    t_stat <- summclust:::coeftable(
      t_stat, param = param
    )[,"tstat"] * sqrt(G / (G-1)) 
    # because summclust uses small sample correction
  }
  
  t_boot <- t_boot[-1]
  
  cat("p_val_type", p_val_type, "\n")
  
  p_val <- 
    get_bootstrap_pvalue(
      p_val_type = p_val_type, 
      t_stat = t_stat, 
      t_boot = t_boot
    )
  
  res <- list(
    p_val = p_val,
    # t_stat = t_stat,
    t_stat = t_stat,
    t_boot = t_boot,
    B = boot_iter,
    R0 = R,
    param = param,
    clustid = clustid,
    # v = v,
    invalid_t = NULL,
    ABCD = NULL,
    small_sample_correction = small_sample_correction
  )
  
  class(res) <- "boot_algo3"
  
  invisible(res)
  
}

