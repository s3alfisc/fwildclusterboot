boot_algo1 <- function(preprocessed_object, boot_iter, point_estimate, impose_null, beta0, sign_level, param, p_val_type, nthreads, type, full_enumeration, small_sample_correction, heteroskedastic, seed) {

  #' Fast wild cluster bootstrap algorithm
  #'
  #' function that implements the fast bootstrap algorithm as described in Roodman et al (2019)
  #'
  #' @param preprocessed_object A list: output of the preprocess2 function.
  #' @param boot_iter number of bootstrap iterations
  #' @param point_estimate The point estimate of the test parameter from the regression model.
  #' @param impose_null If TRUE, the null is not imposed on the bootstrap distribution.
  #'        This is what Roodman et al call the "WCU" bootstrap. With impose_null = FALSE, the
  #'        null is imposed ("WCR").
  #' @param beta0 Shifts the null hypothesis.
  #' @param sign_level The significance level.
  #' @param param name of the test parameter.
  #' @param p_val_type type Type of p-value. By default "two-tailed". Other options: "equal-tailed", ">", "<"
  #' @param nthreads The number of threads. Can be: a) an integer lower than,
  #'                 or equal to, the maximum number of threads; b) 0: meaning
  #'                 all available threads will be used; c) a number strictly
  #'                 between 0 and 1 which represents the fraction of all threads
  #'                 to use. The default is to use 50\% of all threads. You can
  #'                 set permanently the number of threads used within this
  #'                 package using the function ...
  #' @param type character or function. The character string specifies the type
  #'        of boostrap to use: One of "rademacher", "mammen", "norm"
  #'        and "webb". Alternatively, type can be a function(n) for drawing
  #'        wild bootstrap factors. "rademacher" by default.
  #' @param full_enumeration Is full enumeration employed? Full enum. is used if
  #'        N_G^2 < boot_iter for Mammen and Rademacher weights
  #' @param small_sample_correction The small sample correction to be applied. See ssc().
  #' @param heteroskedastic Logical - if TRUE, run a heteroskedastic. If FALSE, run wild cluster bootstrap.
  #' @param seed Integer scalar. Either set via boottest()'s seed argument or inherited from R's global seed (set via set.seed)
  #' @return A list of ...
  #' @importFrom Matrix t Diagonal
  #' @importFrom Matrix.utils aggregate.Matrix
  #' @importFrom collapse fsum GRP
  #' @importFrom stats as.formula coef model.matrix model.response model.weights residuals rlnorm rnorm update
  #' @importFrom gtools permutations
  #' @importFrom dqrng dqsample dqset.seed



  # 1) preprocess
  # preprocessed_object = preprocess

  X <- preprocessed_object$X
  Y <- preprocessed_object$Y
  N <- preprocessed_object$N
  # k <- preprocessed_object$k
  fixed_effect <- preprocessed_object$fixed_effect
  N_G <- preprocessed_object$N_G
  W <- preprocessed_object$W
  clustid <- preprocessed_object$clustid
  # n_fe <- preprocessed_object$n_fe
  #bootcluster <- preprocessed_object$bootcluster
  # vcov_sign <- preprocessed_object$vcov_sign
  weights <- preprocessed_object$weights
  R <- t(as.matrix(preprocessed_object$R0))
  vcov_sign <- preprocessed_object$vcov_sign

  N_G_bootcluster <- N_G

  if(type == "rademacher"){
    type <- 0
  } else if(type == "webb"){
    type <- 1
  } else {
    stop("For the 'lean' bootstrap algorithm, only webb and rademacher weights are supported.")
  }

  if(impose_null == FALSE){
    stop("The 'lean' bootstrap algorithm is currently not supported without imposing the null on the bootstrap dgp.")
  }

  if(sum(R) > 1){
    stop("The 'lean' bootstrap algorithm is currently not supported for hypotheses about more than one parameter.")
  }

  if(is.null(seed)){
    seed <- get_seed()
  }
  
  if(heteroskedastic == TRUE){
    boot_res <-
      wildboottestHC(y = Y,
                     X = X,
                     R = t(R),
                     r = beta0,
                     B = boot_iter,
                     N_G_bootcluster = N,
                     cores = nthreads,
                     type = type, 
                     small_sample_correction = small_sample_correction, 
                     seed = seed)[[1]]
  } else {
    bootcluster <- preprocessed_object$bootcluster[, 1]
    # turn bootcluster into sequence of integers, starting at 0, 1, 2, ..., length(unique(bootcluster)) (required for cpp
    # implementation)
    # if(!class(bootcluster) == "integer"){
    bootcluster <- to_integer(preprocessed_object$bootcluster[, 1])
    #}
    # bootcluster must be integers, starting with 0 (due to cpp implementation)
    bootcluster <- bootcluster - min(bootcluster)

    boot_res <-
      wildboottestCL(y = unname(Y),
                     X = unname(X),
                     R = t(unname(R)),
                     r = beta0,
                     B = boot_iter,
                     N_G_bootcluster = unname(N_G_bootcluster),
                     cores = nthreads,
                     type = type,
                     cluster = bootcluster, 
                     small_sample_correction = small_sample_correction, 
                     seed = seed)[[1]]
  }


  selector <- which(R == 1)
  t_stat <- boot_res[selector,1]
  t_boot <- boot_res[selector, 2:(boot_iter + 1)]

  # p_val_type <- "two-tailed symmetric"
  if (p_val_type == "two-tailed") {
    p_val <- mean(abs(t_stat) < abs(t_boot), na.rm = FALSE)
  } else if (p_val_type == "equal-tailed") {
    p_l <- mean(t_stat < t_boot, na.rm = FALSE)
    p_h <- mean(t_stat > t_boot, na.rm = FALSE)
    p_val <- 2 * min(p_l, p_h, na.rm = FALSE)
  } else if (p_val_type == ">") {
    p_l <- mean(t_stat < t_boot, na.rm = FALSE)
    p_val <- p_l
  } else if (p_val_type == "<") {
    p_h <- mean(t_stat > t_boot, na.rm = FALSE)
    p_val <- p_h
  }


  res <- list(
    p_val = p_val,
    t_stat = t_stat,
    t_boot = t_boot,
    B = boot_iter,
    R0 = R,
    param = param,
    clustid = clustid,
    #v = v,
    invalid_t = NULL,
    ABCD = NULL,
    small_sample_correction = small_sample_correction
  )

  class(res) <- "boot_algo1"

  invisible(res)

}


# for using set.seed() for controlling rcpp's seed, see this 
# blog post http://rorynolan.rbind.io/2018/09/30/rcsetseed/
get_seed <- function() {
  sample.int(.Machine$integer.max, 1)
}