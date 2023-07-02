test_that("test sampling", {
  
  data1 <<- fwildclusterboot:::create_data(
    N = 10000,
    N_G1 = 20,
    icc1 = 0.5,
    N_G2 = 20,
    icc2 = 0.2,
    numb_fe1 = 10,
    numb_fe2 = 10,
    # seed = 908361239,
    seed = 123123,
    weights = 1:N / N
  )

  lm_fit <- lm(proposition_vote ~ treatment + log_income,
    data = data1
  )

  boot1 <- boottest(lm_fit,
    param = c("log_income"),
    clustid = c("group_id2"),
    B = 99999,
    sampling = "dqrng"
  )

  boot2 <- boottest(lm_fit,
    param = c("log_income"),
    clustid = c("group_id2"),
    B = 99999,
    sampling = "standard"
  )

  expect_equal(pval(boot1), pval(boot2), tolerance = 0.05)
  expect_equal(teststat(boot1), teststat(boot2))
  expect_equal(confint(boot1), confint(boot2), tolerance = 0.005)
})
