test_that("algorithm performance test", {
  #' @srrstats {G5.7} **Algorithm performance tests** *to test that implementation
  #' performs as expected as properties of data change. For instance, a
  #' test may show that parameters approach correct estimates within tolerance
  #' as data size increases, or that convergence times decrease for higher
  #' convergence thresholds.* See test-convergence.R, which tests if the
  #' bootstrap converges in probability for B -> infinity.
  #' @srrstats {G5.9b} *Running under different random seeds or initial conditions
  #'  does not meaningfully change results* See the convergence tests.



  skip_on_cran()
  skip_on_ci()

  skip_if_not(
    find_proglang("julia"),
    message = "skip test as julia installation not found."
  )

  # convergence in probability of boottest p values when B -> infinity
  seed <- 1230123

  data1 <<- fwildclusterboot:::create_data(
    N = 1000,
    N_G1 = 15,
    icc1 = 0.5,
    N_G2 = 20,
    icc2 = 0.2,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = seed,
    weights = 1:N / N
  )

  mc_res_p <- matrix(NA, 250, 2)
  mc_res_cl <- matrix(NA, 250, 2)
  mc_res_cu <- matrix(NA, 250, 2)

  bootstrap_iter <- c(199, 999)

  for (B in seq_along(bootstrap_iter)) {
    x <- bootstrap_iter[B]

    for (mc_iter in 1:250) {
      fit <- lm(proposition_vote ~ treatment, data = data1)

      boot1 <- boottest(fit,
        param = "treatment",
        B = x,
        clustid = "group_id1"
      )
      boot2 <- boottest(fit,
        param = "treatment",
        B = x,
        clustid = "group_id1"
      )

      mc_res_p[mc_iter, B] <- pval(boot1) - pval(boot2)
      mc_res_cl[mc_iter, B] <- (confint(boot1) - confint(boot2))[1]
      mc_res_cu[mc_iter, B] <- (confint(boot1) - confint(boot2))[2]
    }
  }

  # test that differences get smaller
  expect_equal(sort(abs(diff(
    colMeans(mc_res_p)
  )), decreasing = TRUE), abs(diff(colMeans(mc_res_p))))
  expect_equal(sort(abs(diff(
    colMeans(mc_res_cl)
  )), decreasing = TRUE), abs(diff(colMeans(mc_res_cl))))
  expect_equal(sort(abs(diff(
    colMeans(mc_res_cu)
  )), decreasing = TRUE), abs(diff(colMeans(mc_res_cu))))
})
