boot_algo3 <- function(preprocessed_object,
                       B,
                       bootstrap_type,
                       r = 0,
                       sign_level,
                       param,
                       p_val_type,
                       nthreads,
                       type,
                       full_enumeration,
                       small_sample_correction,
                       seed,
                       object,
                       impose_null) {
  #
  #' WRE13, WRE33, WRU13 and WRU33 bootstraps as in MNW (2022) "Fast and
  #'  reliable"
  #'
  #' @param preprocessed_object A list: output of the preprocess2 function.
  #' @param B number of bootstrap iterations
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
  #' is used if N_G^2 < B for Mammen and Rademacher weights
  #' @param small_sample_correction The small sample correction to be applied.
  #' See ssc().
  #' If FALSE, run wild cluster bootstrap.
  #' @param seed Integer scalar. Either set via boottest()'s seed argument
  #' or inherited from R's global seed (set via set.seed)
  #' @param object the regression object
  #' @param impose_null logical scalar. Should the null be imposed on the
  #' bootstrap dgp or not?
  #' @importFrom MASS ginv
  #' @importFrom sandwich vcovCL
  #' @importFrom summclust vcov_CR3J
  #' @return A list of ...#
  #' @noRd
  
  check_arg(bootstrap_type, "charin(11, 13, 31, 33)")
  #  other checks: only test of one param, no weights, no fixed effects,
  # ...
  #here for debugging
  # preprocessed_object <- preprocess
  
  if (substr(bootstrap_type, 2, 2) == 1) {
    crv_type <- "crv1"
  } else {
    crv_type <- "crv3"
  }
  X <- preprocessed_object$X
  y <- preprocessed_object$Y
  R <- preprocessed_object$R0
  cluster_df <- preprocessed_object$clustid
  clustid <- names(cluster_df)
  cluster <- as.factor(cluster_df[, 1])
  bootcluster <- preprocessed_object$bootcluster
  G <- N_G_bootcluster <- length(unique(bootcluster[[1]]))
  k <- length(R)
  bootstrap_type_x <- paste0(substr(bootstrap_type, 1, 1), "x")
  if (impose_null) {
    # WCR1x or WCR3x
    bootstrap_type <- paste0("WCR", bootstrap_type_x)
  } else {
    # WCU1x or WCU3x
    bootstrap_type <- paste0("WCU", bootstrap_type_x)
  }
  v <- get_weights(
    type = type,
    full_enumeration = full_enumeration,
    N_G_bootcluster = N_G_bootcluster,
    boot_iter = B
  )
  # create X_g's, X1_g's, y_g's etc
  X_list <- matrix_split(X, cluster, "row")
  y_list <- split(y, cluster, drop = FALSE)
  #   # precompute a range of other objects
  tXgXg <- lapply(seq_along(1:G),
                  function(g)
                    crossprod(X_list[[g]]))
  tXgyg <- lapply(seq_along(1:G),
                  function(g)
                    t(X_list[[g]]) %*% y_list[[g]])
  tXX <- Reduce("+", tXgXg) # crossprod(X)
  tXy <- Reduce("+", tXgyg) # t(X) %*% y
  tXXinv <- solve(tXX)
  RtXXinv <- R %*% tXXinv
  
  tXgX1g <- NULL
  beta_hat <- NULL
  beta_tilde <- NULL
  beta_g_hat <- NULL
  beta_1g_tilde <- NULL
  inv_tXX_tXgXg <- NULL
  
  
  if (bootstrap_type %in% c("WCR3x", "WCU3x")) {
    # X1: X without parameter beta for which hypothesis beta = 0 is tested
    X1 <- X[, which(R == 0)]
    X1_list <- matrix_split(X1, cluster, "row")
    
    tX1gX1g <- lapply(seq_along(1:G),
                      function(g)
                        crossprod(X1_list[[g]]))
    
    tX1gyg <- lapply(seq_along(1:G),
                     function(g)
                       t(X1_list[[g]]) %*% y_list[[g]])
    
    tXgX1g <- lapply(seq_along(1:G),
                     function(g)
                       t(X_list[[g]]) %*% X1_list[[g]])
    
    tX1X1 <- Reduce("+", tX1gX1g) # crossprod(X1)
    tX1y <- Reduce("+", tX1gyg) #t(X1) %*% y
    tX1X1inv <- solve(tX1X1)
    
  }
  
  if (bootstrap_type == "WCR1x") {
    beta_hat <- tXXinv %*% tXy
    beta_tilde <- beta_hat -
      tXXinv %*% R %*% solve(t(R) %*% tXXinv %*% R) %*% (R %*% beta_hat - 0)
    
  } else if (bootstrap_type == "WCU1x") {
    beta_hat <- tXXinv %*% tXy
    
  } else if (bootstrap_type == "WCR3x") {
    inv_tXX_tXgXg <- lapply(1:G,
                            function(x)
                              MASS::ginv(tXX - tXgXg[[x]]))
    
    beta_1g_tilde <- lapply(1:G,
                            function(g)
                              MASS::ginv(tX1X1 - tX1gX1g[[g]]) %*% (tX1y - tX1gyg[[g]]))
    
  } else if (bootstrap_type == "WCU3x") {
    beta_g_hat <- lapply(1:G,
                         function(g)
                           MASS::ginv(tXX - tXgXg[[g]]) %*% (tXy - tXgyg[[g]]))
    
  }
  
  if (crv_type == "crv1") {
    Ag <- lapply(1:G,
                 function(g)
                   tXgXg[[g]] %*% tXXinv)
    
  } else {
    if (is.null(inv_tXX_tXgXg)) {
      inv_tXX_tXgXg <- lapply(1:G,
                              function(x)
                                MASS::ginv(tXX - tXgXg[[x]]))
    }
  }
  
  
  
  # compute scores
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
  
  
  #numer <- (( R %*% tXXinv) %*%  (Reduce("cbind", scores_list) %*% v))
  scores_boot <- Reduce("cbind", scores_list) %*% v
  delta_b_star <- numer <- tXXinv %*%  scores_boot
  
  scores_mat <- Reduce("cbind", scores_list)
  
  dim(R) <- c(1, k) # turn R into matrix
  #delta_b_star <- matrix(NA,k, B+1)
  
  
  if (crv_type == "crv1") {
    Ag2 <- array(NA, c(k, k, G))
    for (g in seq_along(Ag)) {
      Ag2[, , g] <- Ag[[g]]
    }
    
    boot_fit <-
      boot_algo3_crv1(
        B = B,
        G = G,
        k = k,
        v = v,
        scores_mat = scores_mat,
        scores_boot = scores_boot,
        tXXinv = tXXinv,
        Ag = Ag2,
        ssc = small_sample_correction,
        cores = nthreads,
        R = R
      )
    
    boot_vcov <- boot_fit$boot_vcov
    se <- boot_fit$se
    
    t_boot <- c(delta_b_star[which(R == 1),] / se)
    
    # # # non-bootstrapped crv1 vcov
    # tXgug <- lapply(1:G,
    #                 function(g) {
    #                   t(X_list[[g]]) %*%
    #                     (y_list[[g]] - X_list[[g]] %*% beta_hat)
    #                 })
    # meat_list <- lapply(1:G,
    #                     function(g)
    #                       tcrossprod(tXgug[[g]]))
    # meat <- Reduce("+", meat_list)
    # vcov <- tXXinv %*% meat %*% tXXinv

    vcov <- sandwich::vcovCL(object,
    cluster = names(clustid), cadjust = FALSE, type = "HC0")
    #coef_selector <- which(rownames(vcov2) %in% rownames(vcov))
    #all.equal(vcov[coef_selector, coef_selector], vcov2)
    
  } else if (crv_type == "crv3") {
    inv_tXX_tXgXg2 <- array(NA, c(k, k, G))
    for (g in seq_along(inv_tXX_tXgXg)) {
      inv_tXX_tXgXg2[, , g] <- inv_tXX_tXgXg[[g]]
    }
    
    # pre-allocate space for bootstrap
    # start the bootstrap loop
    t_boot <- vector(mode = "numeric", B + 1)
    se <- vector(mode = "numeric", B + 1)
    boot_vcov <- array(NA, c(k, k, B + 1))
    
    for (b in 1:(B + 1)) {
      # Step 1: get bootstrapped scores
      scores_g_boot <- matrix(NA, G, k)
      
      v_ <- v[, b]
      
      for (g in 1:G) {
        scores_g_boot[g, ] <- scores_list[[g]] * v_[g] #* v[g, b]
      }
      
      
      delta_diff <- matrix(NA, G, k)
      
      for (g in 1:G) {
        score_diff <- scores_boot[, b] - scores_g_boot[g, ]
        delta_diff[g, ] <-
          
          ((inv_tXX_tXgXg[[g]] %*% score_diff) - delta_b_star[, b]) ^ 2
      }
      
      se[b] <-
        sqrt(((G - 1) / G) *
               colSums(delta_diff))[which(R == 1)]
      
      t_boot[b] <- c(delta_b_star[, b])[which(R == 1)] / se[b]
      
      
    }
    
    vcov <-
      summclust::vcov_CR3J(obj = object,
                           cluster = clustid)
    
    
    
  }
  
  
  se0 <- sqrt(small_sample_correction * R %*% vcov %*% t(R))
  se0 <- as.vector(se0)
  
  t_stat <- as.vector(coef(object)[which(R == 1)] / se0)
  
  t_boot <- t_boot[-1]
  
  p_val <-
    get_bootstrap_pvalue(p_val_type = p_val_type,
                         t_stat = t_stat,
                         t_boot = t_boot)
  
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
    small_sample_correction = small_sample_correction,
    boot_vcov = boot_vcov,
    boot_coef = delta_b_star
  )
  class(res) <- "boot_algo3"
  invisible(res)
}



#
boot_algo3b <- function(preprocessed_object,
                       B,
                       bootstrap_type,
                       r = 0,
                       sign_level,
                       param,
                       p_val_type,
                       nthreads,
                       type,
                       full_enumeration,
                       small_sample_correction,
                       seed,
                       object,
                       impose_null){

  #' WRE13, WRE33, WRU13 and WRU33 bootstraps as in MNW (2022) "Fast and
  #' reliable"
  #'
  #' @param preprocessed_object A list: output of the preprocess2 function.
  #' @param B number of bootstrap iterations
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
  #' is used if N_G^2 < B for Mammen and Rademacher weights
  #' @param small_sample_correction The small sample correction to be applied.
  #' See ssc().
  #' If FALSE, run wild cluster bootstrap.
  #' @param seed Integer scalar. Either set via boottest()'s seed argument
  #' or inherited from R's global seed (set via set.seed)
  #' @param object the regression object
  #' @param impose_null logical scalar. Should the null be imposed on the
  #' bootstrap dgp or not?
  #' @importFrom MASS ginv
  #' @importFrom sandwich vcovCL
  #' @importFrom summclust vcov_CR3J
  #' @return A list of ...
  #' @noRd
  #'


  #here for debugging
  #preprocessed_object <- preprocess


  if(substr(bootstrap_type, 2, 2) == 1){
    crv_type <- "crv1"
  } else {
    crv_type <- "crv3"
  }

  X <- preprocessed_object$X
  y <- preprocessed_object$Y
  R <- preprocessed_object$R0
  cluster_df <- preprocessed_object$clustid
  clustid <- names(cluster_df)
  cluster <- as.factor(cluster_df[,1])
  bootcluster <- preprocessed_object$bootcluster
  G <- N_G_bootcluster <- length(unique(bootcluster[[1]]))
  k <- length(R)

  bootstrap_type_x <- paste0(substr(bootstrap_type, 1, 1), "x")
  if(impose_null){
    # WCR1x or WCR3x
    bootstrap_type <- paste0("WCR", bootstrap_type_x)
  } else {
    # WCU1x or WCU3x
    bootstrap_type <- paste0("WCU", bootstrap_type_x)
  }

  v <- get_weights(
    type = type,
    full_enumeration = full_enumeration,
    N_G_bootcluster = N_G_bootcluster,
    boot_iter = B
  )

  # create X_g's, X1_g's, y_g's etc
  X_list <- matrix_split(X, cluster, "row")
  y_list <- split(y, cluster, drop = FALSE)

  # precompute a range of other objects
  tXgXg <- lapply(
    seq_along(1:G),
    function(g) crossprod(X_list[[g]])
  )

  tXgyg <- lapply(
    seq_along(1:G),
    function(g) t(X_list[[g]]) %*% y_list[[g]]
  )


  tXX <- Reduce("+", tXgXg) # crossprod(X)
  tXy <- Reduce("+", tXgyg) # t(X) %*% y
  tXXinv <- solve(tXX)
  RtXXinv <- R %*% tXXinv

  tXgX1g <- NULL
  beta_hat <- NULL
  beta_tilde <- NULL
  beta_g_hat <- NULL
  beta_1g_tilde <- NULL
  inv_tXX_tXgXg <- NULL


  if(bootstrap_type %in% c("WCR3x", "WCU3x")){
    # X1: X without parameter beta for which hypothesis beta = 0 is tested
    X1 <- X[, which(R == 0)]
    X1_list <- matrix_split(X1, cluster, "row")

    tX1gX1g <- lapply(
      seq_along(1:G),
      function(g) crossprod(X1_list[[g]])
    )

    tX1gyg <- lapply(
      seq_along(1:G),
      function(g) t(X1_list[[g]]) %*% y_list[[g]]
    )

    tXgX1g <- lapply(
      seq_along(1:G),
      function(g) t(X_list[[g]]) %*% X1_list[[g]]
    )

    tX1X1 <- Reduce("+", tX1gX1g) # crossprod(X1)
    tX1y <- Reduce("+", tX1gyg) #t(X1) %*% y
    tX1X1inv <- solve(tX1X1)

  }

  if(bootstrap_type == "WCR1x"){

    beta_hat <- tXXinv %*% tXy
    beta_tilde <- beta_hat -
      tXXinv %*% R %*% solve(t(R) %*% tXXinv %*% R) %*% (R %*% beta_hat - 0)

  } else if (bootstrap_type == "WCU1x"){

    beta_hat <- tXXinv %*% tXy

  } else if (bootstrap_type == "WCR3x"){

    inv_tXX_tXgXg <- lapply(
      1:G,
      function(x) MASS::ginv(tXX - tXgXg[[x]])
    )

    beta_1g_tilde <- lapply(
      1:G,
      function(g) MASS::ginv(tX1X1 - tX1gX1g[[g]]) %*% (tX1y - tX1gyg[[g]])
    )

  } else if(bootstrap_type == "WCU3x"){

    beta_g_hat <- lapply(
      1:G,
      function(g) MASS::ginv(tXX - tXgXg[[g]]) %*% (tXy - tXgyg[[g]])
    )

  }

  if(crv_type == "crv1"){

    Ag <- lapply(
      1:G,
      function(g) tXgXg[[g]] %*% tXXinv
    )

  } else {
    if(is.null(inv_tXX_tXgXg)){
      inv_tXX_tXgXg <- lapply(
        1:G,
        function(x) MASS::ginv(tXX - tXgXg[[x]])
      )
    }
  }



  # compute scores
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
  t_boot <- vector(mode = "numeric", B + 1)
  se <- vector(mode = "numeric", B + 1)

  # numer <- (( R %*% tXXinv) %*%  (Reduce("cbind", scores_list) %*% v))



  dim(R) <- c(1, k) # turn R into matrix

  for(b in 1:(B + 1)){

    # Step 1: get bootstrapped scores

    scores_g_boot <- matrix(NA,G,k)

    v_ <- v[,b]

    for(g in 1:G){
      scores_g_boot[g,] <- scores_list[[g]] * v_[g] #* v[g, b]
    }

    # numerator (both for WCR, WCU)
    scores_boot <- colSums(scores_g_boot)
    delta_b_star <- tXXinv %*% scores_boot

    # Step 2: get bootstrapped vcov's

    if(crv_type == "crv1"){

      score_hat_g_boot <- list()
      for(g in 1:G){
        # see MacKinnon
        # (https://www.econstor.eu/bitstream/10419/247206/1/qed-wp-1465.pdf)
        # equ (20), note this can be accelerated
        score_hat_g_boot[[g]] <-
          tcrossprod(scores_g_boot[g,] - Ag[[g]] %*% scores_boot)
      }

      score_hat_boot <- Reduce("+", score_hat_g_boot)

      se[b] <-
        sqrt(
          small_sample_correction *
            RtXXinv %*% score_hat_boot %*% t(RtXXinv)
        )

      t_boot[b] <- (c(delta_b_star)[which(R == 1)] / se[b])

    } else if (crv_type == "crv3"){

      delta_diff <- matrix(NA, G, k)

      for(g in 1:G){
        score_diff <- scores_boot - scores_g_boot[g,]
        delta_diff[g,] <-

          (
            (inv_tXX_tXgXg[[g]] %*% score_diff) - delta_b_star
          )^2
      }

      se[b] <-
        sqrt(
          ((G-1) / G) *
            colSums(
              delta_diff
            )
        )[which(R == 1)]

      t_boot[b] <- c(delta_b_star)[which(R == 1)] / se[b]
    }

  }


  # get original t-stat.

  if(crv_type == "crv1"){
    vcov <- sandwich::vcovCL(
      object,
      reformulate(clustid),
      type = "HC0",
      cadjust = FALSE
    )
  } else if(crv_type == "crv3"){
    vcov <-
      summclust::vcov_CR3J(
        obj = object,
        cluster = clustid
      )
  }

  se0 <- sqrt(small_sample_correction * R %*% vcov %*% t(R))
  se0 <- as.vector(se0)

  t_stat <- as.vector(
    coef(object)[which(R == 1)] / se0
  )

  t_boot <- t_boot[-1]

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

  class(res) <- "boot_algo3"

  invisible(res)

}
