test_that("test lean cpp boottest", {
  set.seed(96578)
  dqrng::dqset.seed(9568)

  data1 <<- fwildclusterboot:::create_data(
    N = 1000,
    N_G1 = 1000,
    icc1 = 0.5,
    N_G2 = 10,
    icc2 = 0.01,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 2293
  )
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income,
    data = data1
  )
  feols_fit <- fixest::feols(
    proposition_vote ~ treatment + ideology1 + log_income,
    data = data1
  )
  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income,
    data = data1
  )


  # Test 1: heteroskedadestic wild bootstrap


  set.seed(96578)
  dqrng::dqset.seed(9568)

  boot_lm <- boottest(lm_fit,
    param = "treatment",
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
    nthreads = 1,
    type = "webb"
  )

  set.seed(96578)
  dqrng::dqset.seed(9568)

  boot_lm2 <- boottest(lm_fit,
    param = "treatment",
    B = 999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
    nthreads = 1,
    type = "webb"
  )

  set.seed(96578)
  dqrng::dqset.seed(9568)

  boot_feols <- boottest(feols_fit,
    param = "treatment",
    B = 9999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
    nthreads = 1
  )

  set.seed(96578)
  dqrng::dqset.seed(9568)

  boot_felm <- boottest(felm_fit,
    param = "treatment",
    B = 9999,
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
    nthreads = 1
  )

  # 2.155239
  res <- broom::tidy(
    lmtest::coeftest(
      lm_fit,
      sandwich::vcovHC(lm_fit, type = "HC0")
    )
  )[2, 4:5]

  expect_equal(res$statistic, boot_lm$t_stat)
  expect_equal(res$statistic, boot_feols$t_stat)
  expect_equal(res$statistic, boot_felm$t_stat)

  expect_equal(res$p.value, boot_lm$p_val, tolerance = 0.05)
  expect_equal(res$p.value, boot_feols$p_val, tolerance = 0.05)
  expect_equal(res$p.value, boot_felm$p_val, tolerance = 0.05)


  # HC1

  boot_lm <- boottest(lm_fit,
    param = "treatment",
    B = 9999,
    ssc = boot_ssc(adj = TRUE, cluster.adj = FALSE),
    nthreads = 1
  )
  boot_feols <- boottest(feols_fit,
    param = "treatment",
    B = 9999,
    ssc = boot_ssc(adj = TRUE, cluster.adj = FALSE),
    nthreads = 1
  )
  boot_felm <- boottest(felm_fit,
    param = "treatment",
    B = 9999,
    ssc = boot_ssc(adj = TRUE, cluster.adj = FALSE),
    nthreads = 1
  )

  res <- broom::tidy(
    lmtest::coeftest(
      lm_fit,
      sandwich::vcovHC(lm_fit, type = "HC1")
    )
  )[2, 4:5]

  k <- length(coef(lm_fit))
  N <- nobs(lm_fit)

  # HC1 in sandwich : t / sqrt(n / (n -k))
  # in fwildclusterboot: t / sqrt((n-1) / n-k)
  ssc_corr <- (N - 1) / N

  expect_equal(res$statistic / sqrt(ssc_corr), boot_lm$t_stat)
  expect_equal(res$statistic / sqrt(ssc_corr), boot_feols$t_stat)
  expect_equal(res$statistic / sqrt(ssc_corr), boot_felm$t_stat)

  expect_equal(res$p.value, pval(boot_lm), tolerance = 0.05)
  expect_equal(res$p.value, pval(boot_feols), tolerance = 0.05)
  expect_equal(res$p.value, pval(boot_felm), tolerance = 0.05)




  # test oneway clustering

  boot_lm1 <- boottest(feols_fit,
    param = "treatment",
    clustid = "group_id1",
    B = 2999,
    engine = "R-lean",
    nthreads = 1,
    ssc = boot_ssc(
      adj = FALSE,
      cluster.adj = FALSE
    )
  )

  boot_lm2 <- boottest(lm_fit,
    param = "treatment",
    clustid = "group_id1",
    B = 2999,
    engine = "R",
    nthreads = 1,
    ssc = boot_ssc(
      adj = FALSE,
      cluster.adj = FALSE
    )
  )
  # pracma::toc()

  expect_equal(pval(boot_lm1), pval(boot_lm2), tolerance = 0.05)
  expect_equal(teststat(boot_lm1), teststat(boot_lm2))


  boot_lm1 <- boottest(feols_fit,
    param = "treatment",
    clustid = "group_id1",
    B = 9999,
    engine = "R-lean",
    nthreads = 1
  )

  boot_lm2 <- boottest(feols_fit,
    param = "treatment",
    clustid = "group_id1",
    B = 9999,
    engine = "R",
    nthreads = 1
  )

  expect_equal(pval(boot_lm1), pval(boot_lm2), tolerance = 0.05)
  expect_equal(teststat(boot_lm1), teststat(boot_lm2))


  # test for non-standard hypotheses
})


test_that("r-lean multi-param tests", {
  N <- 2000
  seed <- 7896

  data1 <<- fwildclusterboot:::create_data(
    N = N,
    N_G1 = 8,
    icc1 = 0.5,
    N_G2 = 20,
    icc2 = 0.2,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = seed,
    weights = 1:N / N
  )

  lm_fit <- lm(proposition_vote ~ treatment + log_income,
    data = data1
  )

  boot <- suppressWarnings(
    boottest(
      lm_fit,
      B = 999,
      param = c("treatment", "log_income"),
      R = c(-0.1, 0.1),
      r = -0.1,
      conf_int = FALSE,
      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
    )
  )


  X <- model.matrix(lm_fit)
  u <- resid(lm_fit)
  y <- model.response(model.frame(lm_fit))
  Omega <- diag(tcrossprod(u))
  tXX <- solve(crossprod(X))
  N <- nobs(lm_fit)
  vcov <- tXX %*% (t(X) %*% diag(Omega) %*% X) %*% tXX
  R <- c(0, -0.1, 0.1)
  r <- -0.1
  beta <- coef(lm_fit)

  t <- (R %*% beta - r) / sqrt(t(R) %*% vcov %*% R)

  expect_equal(c(t), teststat(boot))
})


test_that("heteroskedastic 11 vs 21 vs 31", {
  lm_fit <-
    lm(
      proposition_vote ~ treatment + ideology1 + log_income,
      data = fwildclusterboot:::create_data(
        N = 1000,
        N_G1 = 20,
        icc1 = 0.81,
        N_G2 = 10,
        icc2 = 0.01,
        numb_fe1 = 10,
        numb_fe2 = 10,
        seed = 12412
      )
    )

  fit11 <-
    boottest(
      lm_fit,
      param = "treatment",
      bootstrap_type = "11",
      B = 9999,
    )

  fit12 <-
    boottest(
      lm_fit,
      param = "treatment",
      bootstrap_type = "21",
      B = 9999,
    )

  fit13 <-
    boottest(
      lm_fit,
      param = "treatment",
      bootstrap_type = "31",
      B = 9999,
    )

  expect_equal(teststat(fit11), teststat(fit12))
  expect_equal(teststat(fit12), teststat(fit13))

  expect_equal(pval(fit11), pval(fit12), tolerance = 0.02)
  expect_equal(pval(fit12), pval(fit13), tolerance = 0.02)
})
