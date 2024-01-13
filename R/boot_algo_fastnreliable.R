#' WRE13, WRE33, WRU13 and WRU33 bootstraps as in MNW (2022) "Fast and
#' reliable" and MacKinnon "Fast Cluster Bootstraps"
#' (Econometrics & Statistics, 2021)
#'
#' @param preprocessed_object A list: output of the preprocess function.
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
#' @param object the regression object
#' @param impose_null logical scalar. Should the null be imposed on the
#' bootstrap dgp or not?
#' @importFrom Matrix crossprod tcrossprod t Matrix
#' @importFrom summclust vcov_CR3J
#' @return A list of bootstrap results.
#' @noRd



boot_algo_fastnreliable <- function(preprocessed_object,
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
                                    object,
                                    impose_null,
                                    sampling) {
  # here for debugging
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
  fe <- preprocessed_object$fe
  #' @srrstats {G2.4d} *explicit conversion to factor via `as.factor()`*
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
    boot_iter = B,
    sampling = sampling
  )

  # create X_g's, X1_g's, y_g's etc
  X_list <- matrix_split(X, cluster, "row")
  y_list <- split(y, cluster, drop = FALSE)

  # precompute a range of other objects
  tXgXg <- lapply(
    seq_along(1:G),
    function(g) Matrix::crossprod(X_list[[g]])
  )

  tXgyg <- lapply(
    seq_along(1:G),
    function(g) Matrix::t(X_list[[g]]) %*% y_list[[g]]
  )


  tXX <- Matrix::crossprod(X) # Reduce("+", tXgXg) # crossprod(X)
  tXy <- Matrix::crossprod(X, y) # Reduce("+", tXgyg) # t(X) %*% y
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

    tX1gX1g <- lapply(
      seq_along(1:G),
      function(g) Matrix::crossprod(X1_list[[g]])
    )

    tX1gyg <- lapply(
      seq_along(1:G),
      function(g) Matrix::t(X1_list[[g]]) %*% y_list[[g]]
    )

    tXgX1g <- lapply(
      seq_along(1:G),
      function(g) Matrix::t(X_list[[g]]) %*% X1_list[[g]]
    )

    tX1X1 <- Matrix::crossprod(X1) # Reduce("+", tX1gX1g) # crossprod(X1)
    tX1y <- Matrix::crossprod(X1, y) # Reduce("+", tX1gyg) #t(X1) %*% y
    tX1X1inv <- Matrix::solve(tX1X1)
  }

  if (bootstrap_type == "WCR1x") {
    beta_hat <- tXXinv %*% tXy
    beta_tilde <- beta_hat -
      tXXinv %*% R %*% solve(t(R) %*% tXXinv %*% R) %*% (R %*% beta_hat - 0)
  } else if (bootstrap_type == "WCU1x") {
    beta_hat <- tXXinv %*% tXy
  } else if (bootstrap_type == "WCR3x") {
    inv_tXX_tXgXg <- lapply(
      1:G,
      function(x) inv(tXX - tXgXg[[x]], x)
    )

    beta_1g_tilde <- lapply(
      1:G,
      function(g) solve2(tX1X1 - tX1gX1g[[g]], tX1y - tX1gyg[[g]], g)
    )
  } else if (bootstrap_type == "WCU3x") {
    beta_g_hat <- lapply(
      1:G,
      function(g) solve2(tXX - tXgXg[[g]], tXy - tXgyg[[g]], g)
    )
  }

  if (crv_type == "crv1") {
    if (is.null(beta_hat)) {
      beta_hat <- Matrix::solve(tXX, tXy)
    }
  } else {
    if (is.null(inv_tXX_tXgXg)) {
      inv_tXX_tXgXg <- lapply(
        1:G,
        function(x) inv((tXX - tXgXg[[x]]), x)
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
  t_boot <- t_boot2 <- vector(mode = "numeric", B + 1)
  se <- se2 <- vector(mode = "numeric", B + 1)

  dim(R) <- c(1, k) # turn R into matrix

  Cg <- R %*% tXXinv %*% Reduce("cbind", scores_list)

  numer <- Cg %*% v

  if (crv_type == "crv1") {
    tXgXg <- lapply(tXgXg, function(g) as.matrix(g))
    scores_list <- lapply(scores_list, function(g) as.matrix(g))

    H <- compute_H(
      G = G,
      R = matrix(R, 1, k),
      tXXinv = as.matrix(tXXinv),
      tXgXg = tXgXg,
      scores_list = scores_list,
      cores = nthreads
    )

    denom <- boot_algo3_crv1_denom(
      B = B,
      G = G,
      ssc = small_sample_correction,
      H = as.matrix(H),
      Cg = as.matrix(Cg),
      v = v,
      cores = nthreads
    )


    t_boot <- c(as.matrix(numer) / sqrt(c(denom)))
  } else if (crv_type == "crv3") {
    for (b in 1:(B + 1)) {
      # Step 1: get bootstrapped scores

      scores_g_boot <- matrix(NA, G, k)

      v_ <- v[, b]

      for (g in 1:G) {
        scores_g_boot[g, ] <- c(as.matrix(scores_list[[g]])) * v_[g] #* v[g, b]
      }

      # numerator (both for WCR, WCU)
      scores_boot <- colSums(scores_g_boot)
      delta_b_star <- tXXinv %*% scores_boot

      delta_diff <- matrix(NA, G, k)

      for (g in 1:G) {
        score_diff <- scores_boot - scores_g_boot[g, ]
        delta_diff[g, ] <-
          as.vector(
            (inv_tXX_tXgXg[[g]] %*% score_diff) - delta_b_star
          )^2
      }

      se[b] <-
        sqrt(
          ((G - 1) / G) *
            colSums(
              delta_diff
            )
        )[which(R == 1)]

      t_boot[b] <- c(as.matrix(delta_b_star))[which(R == 1)] / se[b]
    }
  }
  # get original t-stat.

  if (crv_type == "crv1") {
    score_all <- lapply(
      1:G, function(g) {
        Matrix::tcrossprod(
          Matrix::crossprod(X_list[[g]], y_list[[g]] - X_list[[g]] %*% beta_hat)
        )
      }
    )
    meat <- Reduce("+", score_all)
    vcov <- tXXinv %*% meat %*% tXXinv
  } else if (crv_type == "crv3") {
    vcov3 <- quote(
      summclust::vcov_CR3J(
        obj = object,
        cluster = clustid
      )
    )

    if (inherits(object, "fixest")) {
      vcov3$absorb_cluster_fixef <- FALSE
    }

    vcov <- eval(vcov3)
  }

  se0 <- sqrt(small_sample_correction * R %*% vcov %*% t(R))
  se0 <- as.vector(se0)

  t_stat <- as.vector(
    coef(object)[which(R == 1)] / se0
  )


  t_boot <- t_boot[-1]

  t_stat <- as.vector(t_stat)
  t_boot <- as.vector(t_boot)

  p_val <-
    get_bootstrap_pvalue(
      p_val_type = p_val_type,
      t_stat = t_stat,
      t_boot = t_boot
    )

  res <- list(
    p_val = as.vector(p_val),
    t_stat = as.vector(t_stat),
    t_boot = as.vector(t_boot),
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


inv <- function(x, g) {
  tryCatch(
    {
      Matrix::solve(x)
    },
    error = function(e) {
      message(
        message =
          paste0(
            "Matrix inversion error when computing beta(g) for cluster ",
            g, ". Using Pseudo-Inverse instead. Potentially, you can suppress",
            "this message by specifying a cluster fixed effect in the bootstrap",
            "via the `fe` argument of `boottest()`."
          )
      )
      eigen_pinv(as.matrix(x))
    }
  )
}

solve2 <- function(x, y, g) {
  tryCatch(
    {
      Matrix::solve(x, y)
    },
    error = function(e) {
      eigen_pinv(as.matrix(x)) %*% y
    }
  )
}
