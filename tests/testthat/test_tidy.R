test_that("test tidiers with q = 1", {
  skip_on_cran()

  library(generics)
  lm_fit <<-
    lm(
      proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
      data = fwildclusterboot:::create_data(
        N = 1000,
        N_G1 = 10,
        icc1 = 0.01,
        N_G2 = 10,
        icc2 = 0.01,
        numb_fe1 = 10,
        numb_fe2 = 10,
        seed = 1234
      )
    )

  boottest_r <- boottest(lm_fit,
    param = "treatment",
    B = 9999,
    clustid = "group_id1"
  )
  boottest_rlean <- boottest(
    lm_fit,
    param = "treatment",
    B = 9999,
    clustid = "group_id1",
    boot_algo = "R-lean"
  )
  boottest_julia <- boottest(
    lm_fit,
    param = "treatment",
    B = 9999,
    clustid = "group_id1",
    boot_algo = "WildBootTests.jl"
  )


  expect_equal(tidy(boottest_r), tidy(boottest_julia), tolerance = 0.02)
  expect_equal(glance(boottest_r), glance(boottest_julia), tolerance = 0.02)
  expect_equal(plot(boottest_r), plot(boottest_julia), tolerance = 0.02)

  expect_equal(tidy(boottest_rlean)[, 1:4],
    tidy(boottest_r)[, 1:4],
    tolerance = 0.02
  )
})


test_that("test tidiers with q > 1", {
  skip_on_cran()

  library(generics)

  lm_fit <<-
    lm(
      proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
      data = fwildclusterboot:::create_data(
        N = 1000,
        N_G1 = 10,
        icc1 = 0.01,
        N_G2 = 10,
        icc2 = 0.01,
        numb_fe1 = 10,
        numb_fe2 = 10,
        seed = 1234
      )
    )

  R <- clubSandwich::constrain_equal(1:2, coef(lm_fit))

  mboottest_julia <-
    mboottest(
      lm_fit,
      R = R,
      B = 999,
      clustid = "group_id1",
      seed = 123
    )

  expect_equal(tidy(mboottest_julia)$teststat, -14.02107)
  expect_equal(summary(mboottest_julia)$p_val, 0)
})
