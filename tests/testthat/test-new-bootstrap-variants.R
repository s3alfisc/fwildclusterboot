test_that("test r-fnw vs r-, stochastic", {
  skip_on_cran()
  skip_if_not(
    fwildclusterboot:::find_proglang("julia"),
    message = "skip test as julia installation not found."
  )
  reltol <- 0.05
  B <- 9999

  seed <- 123123
  set.seed(seed)

#' @srrstats {G5.1} *Data sets created within, and used to test, a package
#' should be exported (or otherwise made generally available) so that users
#'  can confirm tests and run examples.* Data sets used internally can be
#'  recreated via a non-exported `fwildclusterboot:::create_data()` function.
#' @srrstats {G5.4} **Correctness tests** *to test that statistical algorithms
#' produce expected results to some fixed test data sets (potentially through
#' comparisons using binding frameworks such as
#' [RStata](https://github.com/lbraglia/RStata)).* Several correctness
#' tests are implemented. First, it is tested if the non-bootstrapped
#' t-statistics
#' produced via boottest() *exactly* match those computed by the fixest package
#' (see test_tstat_equivalence). Second, `fwildclusterboot` is heavily tested
#' against `WildBootTests.jl` - see "test-r-vs-julia". Last, multiple R
#' implementations of the WCB are tested against each other.


  data1 <<- fwildclusterboot:::create_data(
    N = 1000,
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

  lm_fit_weights <- lm(
    proposition_vote ~ treatment + log_income,
    weights = data1$weights,
    data = data1
  )
  lm_fits <- list(
    ols = lm_fit # ,
    #  wls = lm_fit_weights
  )
  #
  # object <- lm_fit
  # type <- "mammen"
  # p_val_type = "two-tailed"


  for (object in lm_fits) {
    for (type in c("rademacher", "webb", "mammen", "norm")) {
      for (p_val_type in c("two-tailed", "equal-tailed", ">", "<")) {
        seed <- sample(1:100000, 1)
        dqrng::dqset.seed(seed)
        set.seed(seed)
        # test the wcr
        boot1 <- boottest(object,
          param = c("log_income"),
          clustid = c("group_id2"),
          B = B,
          impose_null = TRUE,
          engine = "R",
          bootstrap_type = "fnw11",
          type = type,
          p_val_type = p_val_type,
          conf_int = FALSE,
          ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
        )

        # reset seed to make sure same weights are applied
        dqrng::dqset.seed(seed)
        set.seed(seed)
        boot2 <- boottest(object,
          param = c("log_income"),
          clustid = c("group_id2"),
          B = B,
          impose_null = TRUE,
          engine = "R",
          bootstrap_type = "11", ,
          type = type,
          p_val_type = p_val_type,
          conf_int = FALSE,
          ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
        )

        expect_equal(
          teststat(boot1), teststat(boot2),
          ignore_attr = TRUE
        )

        expect_equal(
          pval(boot1), pval(boot2),
          ignore_attr = TRUE
        )
        expect_equal(
          boot1$t_boot, boot2$t_boot,
          ignore_attr = TRUE
        )
        expect_equal(
          nobs(boot1), nobs(boot2),
          ignore_attr = TRUE
        )

        # test the wcu

        # new WCU11 ("fast and reliable") vs old WCR11 ("fast and wild")

        seed <- sample(1:100000, 1)
        dqrng::dqset.seed(seed)
        set.seed(seed)

        boot1 <- boottest(object,
          param = "log_income",
          clustid = c("group_id2"),
          B = B,
          impose_null = FALSE,
          bootstrap_type = "11",
          engine = "R", ,
          type = type,
          conf_int = FALSE,
          ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
        )

        dqrng::dqset.seed(seed)
        set.seed(seed)
        boot2 <- boottest(object,
          param = "log_income",
          clustid = c("group_id2"),
          B = B,
          impose_null = FALSE,
          engine = "R",
          bootstrap_type = "fnw11", ,
          type = type,
          conf_int = FALSE,
          ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
        )

        expect_equal(
          teststat(boot1), teststat(boot2),
          ignore_attr = TRUE
        )
        expect_equal(
          pval(boot1), pval(boot2),
          ignore_attr = TRUE
        )
        expect_equal(
          boot1$t_boot, boot2$t_boot,
          ignore_attr = TRUE
        )
      }
    }
  }
})



test_that("new bootstrap variants II - t_stat equivalence", {
  skip_on_cran()
  skip_if_not(
    fwildclusterboot:::find_proglang("julia"),
    message = "skip test as julia installation not found."
  )

  N <- 1000
  N_G1 <- 17
  data <- fwildclusterboot:::create_data(
    N = N,
    N_G1 = N_G1,
    icc1 = 0.8,
    N_G2 = N_G1,
    icc2 = 0.8,
    numb_fe1 = 10,
    numb_fe2 = 5,
    seed = 123121,
    weights = 1:N / N
  )

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data
  )

  # WCR
  wcr_algos <- wcu_algos <- c(
    "fnw11",
    "11",
    "13",
    "31",
    "33"
  )
  p_val <- t_stat <-
    list()

  for (x in wcr_algos) {
    cat(x)
    res <-
      suppressWarnings(
        boottest(
          lm_fit,
          param = ~treatment,
          clustid = ~group_id1,
          B = 9999,
          impose_null = TRUE,
          bootstrap_type = x,
          ssc = boot_ssc(
            adj = FALSE,
            cluster.adj = FALSE
          )
        )
      )

    p_val[[x]] <- pval(res)
    t_stat[[x]] <- teststat(res)
  }

  df <- data.frame(
    "p_values" = unlist(p_val),
    "t_statistics" = unlist(t_stat)
  )

  expect_equal(df$t_statistics[1], df$t_statistics[2])
  expect_equal(df$t_statistics[2], df$t_statistics[4])
  expect_equal(df$t_statistics[3], df$t_statistics[5])


  # WCU algos

  p_val <- t_stat <-
    list()

  for (x in wcu_algos) {
    res <-
      suppressWarnings(
        boottest(
          lm_fit,
          param = ~treatment,
          clustid = ~group_id1,
          B = 9999,
          impose_null = FALSE,
          bootstrap_type = x,
          ssc = boot_ssc(
            adj = FALSE,
            cluster.adj = FALSE
          )
        )
      )

    p_val[[x]] <- pval(res)
    t_stat[[x]] <- teststat(res)
  }

  df <- data.frame(
    "p_values" = unlist(p_val),
    "t_statistics" = unlist(t_stat)
  )

  expect_equal(df$t_statistics[1], df$t_statistics[2])
  expect_equal(df$t_statistics[2], df$t_statistics[4])
  expect_equal(df$t_statistics[3], df$t_statistics[5])
})




test_that("variants 31 R vs Julia", {
  skip_on_cran()
  skip_if_not(
    fwildclusterboot:::find_proglang("julia"),
    message = "skip test as julia installation not found."
  )

  if (TRUE) {
    # fully enumerated - deterministic - tests
    N_G1 <- 10
    B <- 9999

    data2 <- fwildclusterboot:::create_data(
      N = 1000,
      N_G1 = N_G1,
      icc1 = 0.8,
      N_G2 = N_G1,
      icc2 = 0.8,
      numb_fe1 = 10,
      numb_fe2 = 5,
      seed = 41224,
      # seed = 123,
      weights = 1:N / N
    )



    lm_fit <- lm(
      proposition_vote ~ treatment + ideology1 + log_income +
        Q1_immigration,
      data = data2
    )



    # 1) test WCR31

    suppressWarnings(
      boot31_jl <- boottest(lm_fit,
        B = 9999,
        param = "treatment",
        clustid = "group_id1",
        engine = "WildBootTests.jl",
        bootstrap_type = "31"
      )
    )

    suppressWarnings(
      boot31_r <- boottest(lm_fit,
        B = 9999,
        param = "treatment",
        clustid = "group_id1",
        engine = "R",
        bootstrap_type = "31"
      )
    )

    testthat::expect_equal(
      pval(boot31_jl),
      pval(boot31_r)
    )

    testthat::expect_equal(
      sort(boot31_jl$t_boot),
      sort(boot31_r$t_boot)
    )

    testthat::expect_equal(
      teststat(boot31_jl),
      teststat(boot31_r)
    )

    # 2) WCU31

    suppressWarnings(
      boot31_jl <- boottest(lm_fit,
        B = 9999,
        param = "treatment",
        clustid = "group_id1",
        impose_null = FALSE,
        engine = "WildBootTests.jl",
        bootstrap_type = "31"
      )
    )
    pval(boot31_jl)

    suppressWarnings(
      boot31_r <- boottest(lm_fit,
        B = 9999,
        param = "treatment",
        clustid = "group_id1",
        impose_null = FALSE,
        engine = "WildBootTests.jl",
        bootstrap_type = "31"
      )
    )


    testthat::expect_equal(
      pval(boot31_jl),
      pval(boot31_r)
    )

    testthat::expect_equal(
      sort(boot31_jl$t_boot),
      sort(boot31_r$t_boot)
    )

    testthat::expect_equal(
      teststat(boot31_jl),
      teststat(boot31_r)
    )
  }
})


test_that("new variants and fixed effects", {
  skip_on_cran()
  skip_if_not(
    fwildclusterboot:::find_proglang("julia"),
    message = "skip test as julia installation not found."
  )

  library(fixest)
  library(fwildclusterboot)

  B <- 9999

  data1 <<- fwildclusterboot:::create_data(
    N = 1000,
    N_G1 = 20,
    icc1 = 0.5,
    N_G2 = 20,
    icc2 = 0.2,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 961239,
    weights = 1:N / N
  )

  feols_fit <- feols(proposition_vote ~ treatment + log_income | group_id1,
    data = data1
  )

  lm_fit <- lm(proposition_vote ~ treatment + log_income + as.factor(group_id1),
    data = data1
  )

  # x1 variants

  set.seed(2345234)
  dqrng::dqset.seed(6756)

  boot31_lm <- boottest(lm_fit,
    B = 9999,
    param = "treatment",
    clustid = "group_id1",
    bootstrap_type = "31",
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )

  set.seed(2345234)
  dqrng::dqset.seed(6756)

  boot31_fe <- boottest(feols_fit,
    B = 9999,
    param = "treatment",
    clustid = "group_id1",
    bootstrap_type = "31",
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )

  expect_equal(
    pval(boot31_lm),
    pval(boot31_fe)
  )

  expect_equal(
    teststat(boot31_lm),
    teststat(boot31_fe)
  )

  expect_equal(
    boot31_lm$t_boot,
    boot31_fe$t_boot
  )

  set.seed(2345234)
  dqrng::dqset.seed(6756)

  # x3 variants
  boot13_lm <- boottest(lm_fit,
    B = 9999,
    param = "treatment",
    clustid = "group_id1",
    bootstrap_type = "13",
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )

  set.seed(2345234)
  dqrng::dqset.seed(6756)

  boot13_fe <- boottest(feols_fit,
    B = 9999,
    param = "treatment",
    clustid = "group_id1",
    bootstrap_type = "13",
    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )

  expect_equal(
    pval(boot13_lm),
    pval(boot13_fe)
  )

  expect_equal(
    teststat(boot13_lm),
    teststat(boot13_fe)
  )

  expect_equal(
    boot13_lm$t_boot,
    boot13_fe$t_boot
  )
})
