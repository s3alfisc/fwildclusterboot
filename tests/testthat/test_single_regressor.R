test_that("boottest works with a single regressor (lm, felm)", {
  # With one column, model_matrix.lm and model_matrix.felm used to drop the
  # design matrix to a vector: the restriction vector R0 then came out empty
  # and the bootstrap failed ("non-conformable arguments" for lm, "exactly
  # singular" for felm).
  set.seed(123)
  N <- 600
  data_single <<- data.frame(
    y = rnorm(N),
    x = rnorm(N),
    fe = sample(1:30, N, TRUE),
    clustid = sample(1:20, N, TRUE)
  )

  lm_fit <- lm(y ~ 0 + x, data = data_single)
  felm_fit <- lfe::felm(y ~ x | fe, data = data_single)
  feols_fit <- fixest::feols(y ~ x | fe, data = data_single)

  dqrng::dqset.seed(1)
  boot_lm <- boottest(lm_fit, param = "x", clustid = "clustid", B = 999)
  expect_equal(unname(boot_lm$point_estimate), unname(coef(lm_fit)["x"]))

  dqrng::dqset.seed(1)
  boot_felm <- boottest(felm_fit, param = "x", clustid = "clustid", B = 999)
  dqrng::dqset.seed(1)
  boot_feols <- boottest(feols_fit, param = "x", clustid = "clustid", B = 999)
  expect_equal(boot_felm$t_stat, boot_feols$t_stat)
  expect_equal(boot_felm$p_val, boot_feols$p_val)
})
