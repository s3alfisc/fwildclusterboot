boot_algo2 <- function(preprocessed_object, boot_iter, wild_draw_fun, point_estimate, impose_null, beta0, sign_level, param, seed, p_val_type) {

  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2019)
  #' @param preprocessed_object A list: output of the preprocess2 function.
  #' @param boot_iter number of bootstrap iterations
  #' @param wild_draw_fun function. Specifies the type of bootstrap to use.
  #' @param point_estimate The point estimate of the test parameter from the regression model.
  #' @param impose_null If TRUE, the null is not imposed on the bootstrap distribution.
  #'        This is what Roodman et al call the "WCU" bootstrap. With impose_null = FALSE, the
  #'        null is imposed ("WCR").
  #' @param beta0 Shifts the null hypothesis.
  #' @param sign_level The significance level.
  #' @param param name of the test parameter.
  #' @param seed the random seed. controls draw of bootstrap weights.
  #' @param p_val_type type Type of p-value. By default "two-tailed". Other options: "equal-tailed", ">", "<"
  #' @import Matrix.utils
  #' @import Matrix
  #' @export
  #' @return A list of ...


  # 1) preprocess
  # preprocessed_object = preprocess

  X <- preprocessed_object$X
  Y <- preprocessed_object$Y
  N <- preprocessed_object$N
  k <- preprocessed_object$k
  clustid <- preprocessed_object$clustid
  fixed_effect <- preprocessed_object$fixed_effect
  N_G <- preprocessed_object$N_G
  W <- preprocessed_object$W
  n_fe <- preprocessed_object$n_fe
  bootcluster <- preprocessed_object$bootcluster
  vcov_sign <- preprocessed_object$vcov_sign
  weights <- preprocessed_object$weights
  R0 <- preprocessed_object$R0

  # beta0 <- 0.005
  if (!is.data.frame(bootcluster)) {
    stop("bootcluster is not a data.frame. fix this in pre-processing.")
  }


  set.seed(seed)

  N_G_bootcluster <- length(unique(bootcluster[[1]]))


  # bootstrap error
  v <- matrix(wild_draw_fun(n = N_G_bootcluster * (boot_iter + 1)), N_G_bootcluster, boot_iter + 1)
  v[, 1] <- 1


  # impose_null = FALSE
  if (impose_null == TRUE) {
    # error under the null hypothesis. note the Xr is only used to pre-compute
    # P and Q
    Xr <- X[, -which(colnames(X) == param)] # delete rows that will be tested
    # R1 <- 1 - R0
    # note: R0 always a vector of zeros and ones - multiplication with beta0 only later
    # (notation in paper different for R = c(2, 0, 0) if hypothesis e.g. 2 x param1 = c)
    # R != R0
    Xr0 <- X %*% R0
  } else if (impose_null == FALSE) {
    # Xr <- X[, -which(colnames(X) == param)]
    # Xr <- X[, -which(colnames(X) == param)] # delete rows that will be tested
    Xr <- X
    Xr0 <- rep(0, N)
    # note: if impose_null, all the parts that end with b contain only 0's
    # hence   - SuXb matrix of c* x k with zeros ...
    #         - P vector of length N with zeros
    #         - XinvXXrP vector of length N with zeros
    #         - diag_XinvXXRuS_b matrix N x c* with only zeros
    #         - K_b, C, D, CD, DD also only contain zeros. Note: CC non-zero
    #         - numer_b contains only zeros, B as well
  }

  # small sample correction for clusters
  G <- vapply(clustid, function(x) length(unique(x)), 1)
  small_sample_correction <- G / (G - 1)
  # prepare summation of individual terms for multiway clustering
  small_sample_correction <- vcov_sign * small_sample_correction

  # Xr is only used here:
  weights_mat <- Matrix::Diagonal(N, weights)
  weights_sq <- sqrt(weights) 

  Ar <- solve(crossprod(weights_sq * Xr))
  #Ar <- as.matrix(solve(t(Xr) %*% weights_mat %*% Xr))
  # if(sum(weights) == length(weights)){
  #   mean(Ar - solve(crossprod(Xr)))
  # }
  Q <- Y - Xr %*% (Ar %*% (t(Xr) %*% weights_mat %*% Y))
  P <- -Xr0 + Xr %*% (Ar %*% (t(Xr) %*% weights_mat %*% Xr0))
  ## invXrXr <- solve(crossprod(Xr))
  # Q <- Y - Xr %*% (Ar %*% (t(Xr) %*% Y)) # u_hat
  # P <- Xr %*% (Ar %*% (t(Xr) %*% Xr0)) - Xr0
  if (impose_null == FALSE && any(P != 0) == TRUE) {
    stop("P contains non-0 values even though impose_null = FALSE.")
  }

  rm(list = c("Ar", "Xr", "Xr0"))

  A0 <- solve(crossprod(weights_sq * X))
  #A0 <- as.matrix(solve(t(X) %*% weights_mat %*% X))
  WXAr <- weights * as.vector(X %*% (A0 %*% R0))
  WXArX <- WXAr * X
  # invXX <- solve(crossprod(X)) # k x k matrix#
  # XinvXXr <- as.vector(X %*% (invXX %*% R0)) # N x 1
  # XinvXXrX <- XinvXXr * X

  # pre-calculate several objects used in for loop below:
  # splits: a + br
  SuXa <- collapse::fsum(weights * as.vector(Q) * X, bootcluster[[1]])
  SuXb <- collapse::fsum(weights * as.vector(P) * X, bootcluster[[1]])

  # XinvXXrQ <- XinvXXr * Q
  # XinvXXrP <- XinvXXr * P

  WXArQ <- WXAr * Q
  WXArP <- WXAr * P

  diag_XinvXXRuS_a <- Matrix::t(
    Matrix.utils::aggregate.Matrix(
      Matrix::Diagonal(
        N,
        as.vector(WXArQ)
      ),
      bootcluster[[1]]
    )
  ) # N x c*

  diag_XinvXXRuS_b <- Matrix::t(
    Matrix.utils::aggregate.Matrix(
      Matrix::Diagonal(
        N,
        as.vector(WXArP)
      ),
      bootcluster[[1]]
    )
  ) # N x c*

  # calculate numerator:

  numer_a <- collapse::fsum(as.vector(WXArQ), bootcluster[[1]])
  numer_b <- collapse::fsum(as.vector(WXArP), bootcluster[[1]])
  # calculate A, B
  A <- crossprod(numer_a, v)
  B <- crossprod(numer_b, v)


  # prepare list containers for results
  SXinvXXrX <- list()
  SXinvXXrX_invXX <- list()

  S_XinvXXR_F <- list()
  SXinvXXrX <- list()
  J_a <- list()
  K_b <- list()
  K_a <- list()
  C <- list()
  D <- list()
  CC <- list()
  DD <- list()
  CD <- list()

  if (is.null(W)) {
    for (x in names(clustid)) {
      SXinvXXrX[[x]] <- collapse::fsum(WXArX, clustid[x]) # c* x f
      SXinvXXrX_invXX[[x]] <- SXinvXXrX[[x]] %*% A0
      # a
      S_diag_XinvXXRu_S_a <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_a, clustid[x]) # c* x c
      K_a[[x]] <- S_diag_XinvXXRu_S_a - tcrossprod(SXinvXXrX_invXX[[x]], SuXa)
      # b: note that from here, if impose_null = TRUE, _b suffix objects and D, DD, CD need not be computed, they are always objects of 0's only
      S_diag_XinvXXRu_S_b <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_b, clustid[x])
      K_b[[x]] <- S_diag_XinvXXRu_S_b - tcrossprod(SXinvXXrX_invXX[[x]], SuXb)

      C[[x]] <- eigenMatMult(as.matrix(K_a[[x]]), v)
      D[[x]] <- eigenMatMult(as.matrix(K_b[[x]]), v)
      CC[[x]] <- colSums(C[[x]] * C[[x]])
      DD[[x]] <- colSums(D[[x]] * D[[x]])
      CD[[x]] <- colSums(C[[x]] * D[[x]])
    }
  } else if (!is.null(W)) {
    # project out fe
    S_Wu_F_a <- crosstab2(as.matrix(weights * W %*% Q), var1 = bootcluster, var2 = fixed_effect) # f x c*
    S_Wu_F_b <- crosstab2(as.matrix(weights * W %*% P), var1 = bootcluster, var2 = fixed_effect) # f x c*

    for (x in names(clustid)) {
      SXinvXXrX[[x]] <- collapse::fsum(WXArX, clustid[x]) # c* x f
      SXinvXXrX_invXX[[x]] <- SXinvXXrX[[x]] %*% A0
      S_XinvXXR_F <- crosstab2(WXAr, var1 = clustid[x], var2 = fixed_effect) # c x f
      # a
      prod_a <- t(tcrossprod(S_Wu_F_a, S_XinvXXR_F))
      S_diag_XinvXXRu_S_a <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_a, clustid[x]) # c* x c
      S_diag_XinvXXRu_S_a <- S_diag_XinvXXRu_S_a - prod_a
      K_a[[x]] <- S_diag_XinvXXRu_S_a - tcrossprod(SXinvXXrX_invXX[[x]], SuXa)
      # b: note that from here, if impose_null = TRUE, _b suffix objects and D, DD, CD need not be computed, they are always objects of 0's only
      prod_b <- t(tcrossprod(S_Wu_F_b, S_XinvXXR_F))
      S_diag_XinvXXRu_S_b <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_b, clustid[x])
      S_diag_XinvXXRu_S_b <- S_diag_XinvXXRu_S_b - prod_b
      K_b[[x]] <- S_diag_XinvXXRu_S_b - tcrossprod(SXinvXXrX_invXX[[x]], SuXb)

      C[[x]] <- eigenMatMult(as.matrix(K_a[[x]]), v)
      D[[x]] <- eigenMatMult(as.matrix(K_b[[x]]), v)
      CC[[x]] <- colSums(C[[x]] * C[[x]])
      DD[[x]] <- colSums(D[[x]] * D[[x]])
      CD[[x]] <- colSums(C[[x]] * D[[x]])
    }
  }


  # calculate p-val based on A, B, CC, CD, DD
  p_val_res <- p_val_null2(beta0 = beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction = small_sample_correction, impose_null = impose_null, point_estimate = point_estimate, p_val_type = p_val_type)

  p_val <- p_val_res$p_val
  t <- p_val_res$t
  t_boot <- p_val_res$t_boot
  invalid_t <- p_val_res$delete_invalid_t_total

  ABCD <- list(A = A, B = B, CC = CC, CD = CD, DD = DD)

  res <- list(
    p_val = p_val,
    t_stat = t[1],
    t_boot = t_boot,
    B = B,
    R0 = R0,
    param = param,
    clustid = clustid,
    v = v,
    invalid_t = invalid_t,
    ABCD = ABCD
  )
  class(res) <- "boot_algo"

  invisible(res)
}
