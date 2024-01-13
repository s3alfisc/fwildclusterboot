p_val_null2 <-
  function(r,
           A,
           B,
           CC,
           CD,
           DD,
           clustid,
           boot_iter,
           small_sample_correction,
           impose_null,
           point_estimate,
           p_val_type) {
    #' Calculate p-values based on A, B, CC, CD, DD and other inputs
    #' @param r Scalar. Shifts the null hypothesis.
    #' @param A A list.
    #' @param B A list.
    #' @param CC A list.
    #' @param CD A list.
    #' @param DD A list.
    #' @param clustid A data.frame containing the cluster variables.
    #' @param boot_iter An integer. Number of bootstrap iterations.
    #' @param small_sample_correction A vector of the dimension of
    #' ncol(clustid)
    #' @param impose_null If TRUE, no null hypothesis if imposed on
    #' the bootstrap
    #' @param point_estimate The point estimate of the test parameter
    #' from the regression model.
    #' @param p_val_type type Type of p-value. By default "two-tailed".
    #'  Other options: "equal-tailed", ">", "<"
    #'
    #' @return A list containing the bootstrapped p-value, the bootstrapped
    #'  t-statistics,
    #'         and the number of invalid test statistics.
    #' @noRd

    # Roodman et al on WCU: "Therefore, as we vary ßj0, the bootstrap samples
    # do not change, and hence, only one set of bootstrap samples needs to be
    # constructed."
    # here, note that if impose_null = FALSE -> WCU: B = 0, CD = 0, DD = 0 and
    # hence numer and
    # JJ do not vary in r

    numer <- A + B * r
    names_clustid <- names(clustid)

    JJ <- list()
    for (x in seq_along(names_clustid)) {
      JJ[[x]] <-
        (small_sample_correction[x] * (CC[[x]] + 2 * CD[[x]] * r + DD[[x]] * r^
          2))
    }

    len_names_clustid <- length(names_clustid)
    if (len_names_clustid == 1) {
      JJ_sum <- JJ[[1]]
    } else if (len_names_clustid > 1) {
      JJ_sum <- Reduce("+", JJ)
    }

    denom <- suppressWarnings(sqrt(JJ_sum))

    # cat("ssc:", small_sample_correction, "\n")
    # cat("numer: ", numer[3], "\n")
    # cat("denom:", denom[3], "\n")

    t <- numer / denom
    t_boot <- t[2:(boot_iter + 1)]

    # delete invalid test statistics
    delete_invalid_t_total <- sum(is.na(t_boot))
    t_boot <- t_boot[!is.na(t_boot)]

    if (impose_null == TRUE) {
      t_stat <- t[1]
    } else if (impose_null == FALSE) {
      t_stat <- ((point_estimate - r) / denom[1])
    }


    p_val <- get_bootstrap_pvalue(
      p_val_type = p_val_type,
      t_stat = t_stat,
      t_boot = t_boot
    )

    res <- list(
      p_val = p_val,
      t_stat = t_stat,
      t_boot = t_boot,
      invalid_t = delete_invalid_t_total
    )
  }
