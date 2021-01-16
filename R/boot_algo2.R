boot_algo2 <- function(preprocessed_object, boot_iter, wild_draw_fun){
  
  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2009)
  #' @param preprocessed_object A preprocessed object of type preprocessed
  #' @param boot_iter number of bootstrap iterations
  #' @param wild_draw_fun function. Specifies the type of bootstrap to use.
  #' @import Matrix.utils
  #' @import Matrix
  #' @export
  #' @return A list of ... 
  

  # 1) preprocess
  X <- preprocessed_object$X
  Y <- preprocessed_object$Y
  R0 <- preprocessed_object$R0
  data <- preprocessed_object$data
  N <- preprocessed_object$N
  k <- preprocessed_object$k
  clustid <- preprocessed_object$clustid
  fixed_effect <- preprocessed_object$fixed_effect
  beta0 <- preprocessed_object$beta0
  N_G <- preprocessed_object$N_G
  alpha <- preprocessed_object$alpha
  param <- preprocessed_object$param
  W <- preprocessed_object$W
  n_fe <- preprocessed_object$n_fe
  seed <- preprocessed_object$seed
  bootcluster <- preprocessed_object$bootcluster
  vcov_sign <- preprocessed_object$vcov_sign
  
  if(!is.data.frame(bootcluster)){
    stop("bootcluster is not a data.frame. fix this in pre-processing.")
  }
  
  
  set.seed(seed)
  
  N_G_bootcluster <- length(unique(bootcluster[[1]]))
  
  # bootstrap error 
  v <- matrix(wild_draw_fun(n = N_G_bootcluster * (boot_iter + 1)), N_G_bootcluster, boot_iter + 1)
  v[,1] <- 1

  # error under the null hypothesis
  Xr <- X[, -which(R0 == 1)] # delete rows that will be tested
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  
  # Yr for constraint leas squares with beta0 = c
  Yr <- Y - X[, which(R0 == 1)] * beta0
  
  # small sample correction for clusters 
  G <- sapply(clustid, function(x) length(unique(x)))
  small_sample_correction <- G / (G - 1)
  
  # prepare summation of individual terms for multiway clustering
  small_sample_correction <- vcov_sign * small_sample_correction

  
  invXrXr <- solve(crossprod(Xr))
  
  Q <- Y - Xr %*% (invXrXr %*% (t(Xr) %*% Y))
  P <- Xr %*% (invXrXr %*% (t(Xr) %*% Xr0)) - Xr0
  
  invXX <- solve(crossprod(X)) # k x k matrix#
  XinvXXr <- as.vector(X %*% (invXX %*% R0)) # N x 1
  XinvXXrX <- XinvXXr * X
  
  # pre-calculate several objects used in for loop below: 
  # splits: a + br
  SuXa <- collapse::fsum( as.vector(Q) * X, bootcluster[[1]])
  SuXb <- collapse::fsum(as.vector(P) * X, bootcluster[[1]])
  
  XinvXXrQ <- XinvXXr * Q
  XinvXXrP <- XinvXXr * P
  
  
  diag_XinvXXRuS_a <- Matrix::t(
    Matrix.utils::aggregate.Matrix(
      Matrix::Diagonal(N, 
                       as.vector(XinvXXrQ)),
      bootcluster[[1]])) # N x c*
  
  diag_XinvXXRuS_b <- Matrix::t(
    Matrix.utils::aggregate.Matrix(
      Matrix::Diagonal(N, 
                       as.vector(XinvXXrP)),
      bootcluster[[1]])) # N x c*  
  
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
  
  if(is.null(W)){
    for(x in names(clustid)){

      SXinvXXrX[[x]] <-  collapse::fsum(XinvXXrX, clustid[x]) #c* x f
      SXinvXXrX_invXX[[x]] <- SXinvXXrX[[x]] %*% invXX
      # a
      S_diag_XinvXXRu_S_a <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_a, clustid[x]) # c* x c
      K_a[[x]] <- S_diag_XinvXXRu_S_a  - tcrossprod(SXinvXXrX_invXX[[x]], SuXa) 
      # b
      S_diag_XinvXXRu_S_b <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_b, clustid[x])
      K_b[[x]] <- S_diag_XinvXXRu_S_b - tcrossprod(SXinvXXrX_invXX[[x]], SuXb) 
      
      C[[x]] <- eigenMatMult(as.matrix(K_a[[x]]), v) 
      D[[x]] <- eigenMatMult(as.matrix(K_b[[x]]), v) 
      CC[[x]] <- colSums(C[[x]] * C[[x]])
      DD[[x]] <- colSums(D[[x]] * D[[x]])
      CD[[x]] <- colSums(C[[x]] * D[[x]])

    }
  } else if(!is.null(W)){
    # project out fe
    S_Wu_F_a <- crosstab2(as.matrix(W %*% Q), var1 = bootcluster, var2 = fixed_effect) # f x c*
    S_Wu_F_b <- crosstab2(as.matrix(W %*% P), var1 = bootcluster, var2 = fixed_effect) # f x c*
    
    for(x in names(clustid)){

      SXinvXXrX[[x]] <-  collapse::fsum(XinvXXrX, clustid[x]) #c* x f
      SXinvXXrX_invXX[[x]] <- SXinvXXrX[[x]] %*% invXX
      S_XinvXXR_F <- crosstab2(XinvXXr, var1 = clustid[x], var2 = fixed_effect) # c x f
      # a
      prod_a <- t(tcrossprod(S_Wu_F_a, S_XinvXXR_F))
      S_diag_XinvXXRu_S_a <- Matrix.utils::aggregate.Matrix(diag_XinvXXRuS_a, clustid[x]) # c* x c
      S_diag_XinvXXRu_S_a <- S_diag_XinvXXRu_S_a - prod_a
      K_a[[x]] <- S_diag_XinvXXRu_S_a  - tcrossprod(SXinvXXrX_invXX[[x]], SuXa) 
      # b
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
  
  # calculate part a and b of numerator
  numer_a <- collapse::fsum(XinvXXrQ, bootcluster[[1]])
  numer_b <- collapse::fsum(XinvXXrP, bootcluster[[1]])
  # calculate A, B
  A <- crossprod(numer_a, v)
  B <- crossprod(numer_b, v)
  
  # calculate p-val based on A, B, CC, CD, DD
  p_val_res <- p_val_null2(beta0 = beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction= small_sample_correction)
  
  p_val <- p_val_res$p_val
  t <- p_val_res$t
  t_boot <- p_val_res$t_boot
  invalid_t <- p_val_res$delete_invalid_t_total
  
  ABCD <- list(A = A, B = B, CC = CC, CD = CD, DD = DD)
  
  res  <- list(p_val = p_val,
               t_stat = t[1],
               t_boot = t_boot,
               X = X,
               Y = Y, 
               B = B, 
               R0 = R0, 
               param = param, 
               clustid = clustid,
               invXX = invXX,
               v = v,
               Xr = Xr,
               XinvXXr = XinvXXr, 
               invalid_t = invalid_t, 
               ABCD = ABCD)
  class(res) <- "boot_algo"
  
  invisible(res)
  
}