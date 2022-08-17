test_that("test fixest formula sugar", {
  

  voters <- fwildclusterboot:::create_data(
    N = 10000,
    N_G1 = 20,
    icc1 = 0.5,
    N_G2 = 20,
    icc2 = 0.2,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 908369,
    weights = 1:N / N
  )

  fit <-
    fixest::feols(c(income, proposition_vote) ~ treatment, data = voters)
  res <-
    lapply(
      fit,
      \(x) boottest(
        x,
        B = 999,
        param = "treatment",
        clustid = "group_id1"
      )
    )

  fit <-
    fixest::feols(proposition_vote ~ csw(treatment, ideology1), data = voters)
  res <-
    lapply(
      fit,
      \(x) boottest(
        x,
        B = 999,
        param = "treatment",
        clustid = "group_id1"
      )
    )

  fit <-
    fixest::feols(proposition_vote ~ fixest::i(treatment, ideology1), data = voters)
  res <-
    boottest(fit,
      B = 999,
      param = "fixest::treatment::0:ideology1",
      clustid = "group_id1"
    )


  fit <- fixest::feols(
    proposition_vote ~ treatment,
    split = ~Q2_defense,
    cluster = ~group_id1,
    data = voters
  )

  res <- lapply(
    fit,
    \(x) boottest(
      x,
      B = 999,
      param = "treatment",
      clustid = "group_id1"
    )
  )

  expect_equal(unlist(lapply(res, function(x) {
    x$t_stat
  })),
  unlist(lapply(fit, function(x) {
    fixest::tstat(x)["treatment"]
  })),
  ignore_attr = TRUE
  )
  
  
})
