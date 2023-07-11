boot_algo_textbook_cpp <-
  function(preprocessed_object,
           boot_iter,
           point_estimate,
           impose_null,
           r,
           sign_level,
           param,
           p_val_type,
           bootstrap_type,
           nthreads,
           type,
           full_enumeration,
           small_sample_correction,
           heteroskedastic) {
    #' Fast wild cluster bootstrap algorithm
    #'
    #' function that implements the fast bootstrap algorithm as described in
    #' Roodman et al (2019)
    #'
    #' @param preprocessed_object A list: output of the preprocess function.
    #' @param boot_iter number of bootstrap iterations
    #' @param point_estimate The point estimate of the test parameter from the
    #' regression model.
    #' @param impose_null If TRUE, the null is not imposed on the bootstrap
    #' distribution.
    #'        This is what Roodman et al call the "WCU" bootstrap. With
    #'        impose_null = FALSE, the
    #'        null is imposed ("WCR").
    #' @param r Shifts the null hypothesis.
    #' @param sign_level The significance level.
    #' @param param name of the test parameter.
    #' @param p_val_type type Type of p-value. By default "two-tailed".
    #' Other options: "equal-tailed", ">", "<"
    #' @param bootstrap_type Determines which wild bootstrap type should be
    #' run. Options are "11" and "31". For more information,
    #' see the details section.
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
    #' @return A list of ...
    #' @importFrom collapse fsum GRP
    #' @importFrom stats as.formula coef model.matrix model.response
    #' model.weights residuals rlnorm rnorm update
    #' @importFrom dqrng dqsample dqset.seed
    #' @noRd

    dreamerr::check_arg(bootstrap_type, "charin(11, 21, 31)")

    X <- preprocessed_object$X
    Y <- preprocessed_object$Y
    N <- preprocessed_object$N
    fixed_effect <- preprocessed_object$fixed_effect
    N_G <- preprocessed_object$N_G
    W <- preprocessed_object$W
    clustid <- preprocessed_object$clustid
    weights <- preprocessed_object$weights
    R <- t(as.matrix(preprocessed_object$R0))
    vcov_sign <- preprocessed_object$vcov_sign
    bootcluster <- preprocessed_object$bootcluster
    N_G_bootcluster <- length(unique(bootcluster[[1]]))


    v <- NULL
    if(type == "rademacher"){
      if(full_enumeration){

        # get fully enumerated weights matrix
        v <- get_weights(
            type = type,
            full_enumeration = full_enumeration,
            N_G_bootcluster = N_G_bootcluster,
            boot_iter = boot_iter,
            sampling = "standard"
        )

      }
    }

    if (type == "rademacher") {
      type <- 0
    } else if (type == "webb"){
      type <- 1
    } else {
      only_webb_rademacher_for_rlean_error()
    }

    if (impose_null == FALSE) {
      no_wcu_for_rlean_error()
    }

    if ((length(R) - sum(R != 1)) > 1) {
      only_scalar_test_rlean_error()
    }

    if(bootstrap_type == "11"){
      bootstrap_type_int <- 1
    } else if(bootstrap_type == "21"){
      bootstrap_type_int <- 2
    } else if(bootstrap_type == "31"){
      bootstrap_type_int <- 3
    }

    if (heteroskedastic == TRUE) {
      boot_res <-
        wildboottestHC(
          y = Y,
          X = X,
          R = t(R),
          r = r,
          B = boot_iter,
          N_G_bootcluster = N,
          cores = nthreads,
          type = type,
          small_sample_correction = small_sample_correction,
          bootstrap_type = bootstrap_type_int
        )[["t_boot"]]

    } else {

      bootcluster <- preprocessed_object$bootcluster[, 1]
      # turn bootcluster into sequence of integers, starting
      # at 0, 1, 2, ..., length(unique(bootcluster)) (required for cpp
      # implementation)
      # if(!class(bootcluster) == "integer"){
      #' @srrstats {G2.4a}

      bootcluster <-
        to_integer(preprocessed_object$bootcluster[, 1])
      # }
      # bootcluster must be integers, starting with 0
      # (due to cpp implementation)
      bootcluster <- bootcluster - min(bootcluster)
      if (is.null(v)) {
        boot_res <-
          wildboottestCL(
            y = unname(Y),
            X = unname(X),
            R = t(unname(R)),
            r = r,
            B = boot_iter,
            N_G_bootcluster = unname(N_G_bootcluster),
            cores = nthreads,
            type = type,
            cluster = bootcluster,
            small_sample_correction = small_sample_correction
          )[["t_boot"]]
      } else {
        boot_res <-
          wildboottestCL_enum(
            y = Y,
            X = X,
            R = t(unname(R)),
            r = r,
            B = boot_iter,
            N_G_bootcluster = unname(N_G_bootcluster),
            cores = nthreads,
            cluster = bootcluster,
            small_sample_correction = small_sample_correction,
            v = t(v)
          )[["t_boot"]]
      }


    }


    # selector <- which(R == 1)
    t_stat <- boot_res[1]
    t_boot <- boot_res[2:(boot_iter + 1)]
    #t_stat <- boot_res[selector, 1]
    #t_boot <- boot_res[selector, 2:(boot_iter + 1)]

    p_val <- get_bootstrap_pvalue(
      p_val_type = p_val_type,
      t_stat = t_stat,
      t_boot = t_boot
    )

    res <- list(
      p_val = p_val,
      t_stat = t_stat,
      t_boot = t_boot,
      B = boot_iter,
      R0 = R,
      param = param,
      clustid = clustid,
      invalid_t = NULL,
      ABCD = NULL,
      small_sample_correction = small_sample_correction
    )

    class(res) <- "boot_algo_textbook_cpp"

    invisible(res)

  }
