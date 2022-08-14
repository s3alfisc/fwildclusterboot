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
  
  cat("ssc", small_sample_correction)
  
  if(bootstrap_type %in% c("WCR13", "WCR33", "WCU13", "WCU33")){
    crv_type <- "crv3"  
  } else {
    crv_type <- "crv1"
  }
  
  #preprocessed_object <- preprocess
  
  X <- preprocessed_object$X
  y <- preprocessed_object$Y
  R <- preprocessed_object$R0
  cluster_df <- preprocessed_object$clustid
  clustid <- names(cluster_df)
  cluster <- as.factor(cluster_df[,1])
  bootcluster <- preprocessed_object$bootcluster
  N_G_bootcluster <- length(unique(bootcluster[[1]]))
  G <- N_G_bootcluster
  k <- length(R)
  
  v <- get_weights(
    type = type, 
    full_enumeration = full_enumeration, 
    N_G_bootcluster = N_G_bootcluster, 
    boot_iter = boot_iter
  )
  
  B <- ncol(v)
  
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
  
  if(crv_type == "crv1"){
    
    score_hat_g <- lapply(
      1:G, 
      function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat
    )
    
  }
  
  scores <- get_scores(
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
  
  scores_num <- scores$score_num
  scores_denom <- scores$score_denom
  
  if(bootstrap_type %in% c("WCR11", "WCR33", "WCU13", "WCU31")){
    all.equal(scores_num, scores_denom)
  }
  
  # pre-allocate space for bootstrap 
  # start the bootstrap loop 
  #delta_b_star <- vector(mode = "numeric",B)
  #se <- vector(mode = "numeric",B)
  t_boot <- vector(mode = "numeric", B)
  dim(R) <- c(1, k) # turn R into matrix
  
  
  for(b in 1:B){
    
    # Step 1: get bootstrapped scores
    
    score_num_g_boot <- score_denom_g_boot <- matrix(NA,G,k)
    
    if(bootstrap_type %in% c("WCR11", "WCR33", "WCU11", "WCU33")){
      for(g in 1:G){
        # calculate bootstrapped score (24) - this could also be done outside of 
        # the bootstrap loopin vectorized fashion
        score_num_g_boot[g,] <- score_denom_g_boot[g,] <- scores_num[[g]] * v[g, b]
      }
      score_num_boot <- score_denom_boot <- colSums(score_num_g_boot)
      delta_num_b_star <- delta_denom_b_star <- tXXinv %*% score_num_boot 
      
    } else {
      for(g in 1:G){
        score_num_g_boot[g,] <- scores_num[[g]] * v[g, b]
        score_denom_g_boot[g,] <- scores_denom[[g]] * v[g, b]
      }
      score_num_boot <- colSums(score_num_g_boot)
      delta_num_b_star <- tXXinv %*% score_num_boot 
      score_denom_boot <- colSums(score_denom_g_boot)
      delta_denom_b_star <- tXXinv %*% score_denom_boot 
    }
    
    # Step 2: get bootstrapped denominator
    
    # tests
    # if(bootstrap_type == "WCR" & b == 1){
    #   all.equal(delta_num_b_star, beta_hat - beta_tilde)
    # }
    # if(bootstrap_type == "WCU" & b == 1){
    #   all.equal(c(delta_num_b_star), rep(0, k))
    # }
    
    # Step 3: calculate bootstrapped vcov's
    
    if(crv_type == "crv1"){
      
      score_hat_g_boot <- list()
      for(g in 1:G){
        score_hat_g_boot[[g]] <- tcrossprod(score_denom_g_boot[g,])
      }
      # pretty obviously there is an error here!
      score_hat_boot <- Reduce("+",score_hat_g_boot)
      
      # all.equal(tXXinv %*% score_hat_boot %*% tXXinv,
      #           sandwich::vcovCL(
      #             object,
      #             cluster = reformulate(clustid),
      #             cadjust = FALSE,
      #             type = "HC0"
      #             )
      #           )
      # 
      se <- se2 <- 
        #small_sample_correction *
        sqrt(
          (R %*% tXXinv) %*% score_hat_boot %*% (tXXinv %*% t(R))
        )
      
      t_boot[b] <- (c(delta_num_b_star)[which(R == 1)] / se)
      
    } else if (crv_type == "crv3"){
      
      delta_diff <- matrix(NA, G, k)
      
      for(g in 1:G){
        score_diff <- score_denom_boot - score_denom_g_boot[g,]
        delta_diff[g,] <- 
          (
            (inv_tXX_tXgXg[[g]] %*% score_diff) - delta_denom_b_star
          )^2
      }
      
      se <- se3 <- 
        #small_sample_correction * 
        sqrt( 
          colSums(
            delta_diff
          )
        )
      
      t_boot[b] <- c(delta_num_b_star)[which(R == 1)] / se[which(R == 1)]
      
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
  
  
  p_val <- 
    get_bootstrap_pvalue(
      p_val_type = p_val_type, 
      t_stat = t_stat, 
      t_boot = t_boot[-1]
    )
  
  res <- list(
    p_val = p_val,
    t_stat = t_stat,
    t_boot = t_boot[-1],
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

