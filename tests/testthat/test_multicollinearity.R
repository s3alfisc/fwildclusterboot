test_that("test multicollinearity", {

  
  N <- 1000
  X1 <- rnorm(N)
  X2 <- X1
  Y <- rnorm(N)
  data2 <- data.frame(
    Y = Y,
    X1 = X1,
    X2 = X2,
    clustid = sample(1:10, N, TRUE)
  )

  lm_fit <- lm(Y ~ X1 + X2, data = data2)
  suppressWarnings(feols_fit <- fixest::feols(Y ~ X1 + X2, data = data2))
  suppressWarnings(felm_fit <- lfe::felm(Y ~ X1 + X2, data = data2))

  boot_lm <-
    boottest(
      object = lm_fit,
      param = "X1",
      B = 999,
      clustid = "clustid"
    )
  boot_felm <-
    boottest(
      object = felm_fit,
      param = "X1",
      B = 999,
      clustid = "clustid"
    )
  boot_feols <-
    boottest(
      object = feols_fit,
      param = "X1",
      B = 999,
      clustid = "clustid"
    )

  expect_equal(boot_lm$t_stat, boot_felm$t_stat)
  expect_equal(boot_lm$t_stat, boot_feols$t_stat)
})
