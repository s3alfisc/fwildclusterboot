test_that("t-stat equivalence OLS - R and R-lean, against fixest", {

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
#' @srrstats {G5.4b} *For new implementations of existing methods, correctness
#' tests should include tests against previous implementations. Such testing
#' may explicitly call those implementations in testing, preferably from
#' fixed-versions of other software, or use stored outputs from those where
#' that is not possible.* Extensive tests against WildBootTests.jl and
#' alternative R implementations provided by fwildclusterboot. Also, the
#' Python package wildboottest tests against fwildclusterboot.
#' @srrstats {G5.6} **Parameter recovery tests** *to test that the
#' implementation produce expected results given data with known properties.
#' For instance, a linear regression algorithm should return expected
#' coefficient values for a simulated data set generated from a linear model.*
#' Done. Non-bootstrapped t-stats are tested against t-stats and F-stats
#' computed by the fixest package (see test_tstat_equivalence.R). Also, tests
#'  if bootstrapped p-values are deterministic under "full enumeration"
#'  (test-seed.R).
#' @srrstats {G5.6a} *Parameter recovery tests should generally be expected
#' to succeed within a defined tolerance rather than recovering exact values.*
#'  t-stat equivalence is tested "exactly", r vs Julia is tested with tolerance.




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
        seed = 970691
      )
    )

  B <- 999

  # adj= FALSE
  # cluster.adj= FALSE
  # cluster.df= "conventional"
  # impose_null= TRUE
  # engine = "R"

  for (engine in c("R")) {
    for (adj in c(TRUE, FALSE)) {
      for (cluster.adj in c(TRUE, FALSE)) {
        for (cluster.df in c("conventional", "min")) {
          for (impose_null in c(TRUE, FALSE)) {
            # oneway clustering
            feols_fit <-
              fixest::feols(
                proposition_vote ~ treatment + ideology1 + log_income,
                data = fwildclusterboot:::create_data(
                  N = 1000,
                  N_G1 = 20,
                  icc1 = 0.81,
                  N_G2 = 10,
                  icc2 = 0.01,
                  numb_fe1 = 10,
                  numb_fe2 = 10,
                  seed = 970691
                ),
                cluster = ~group_id1,
                ssc = fixest::ssc(
                  adj = adj,
                  cluster.adj = cluster.adj,
                  cluster.df = cluster.df
                )
              )

            dof_tstat <-
              fixest::tstat(feols_fit)[c(
                "treatment", "log_income",
                "ideology1"
              )]


            boot1 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "treatment",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )

            boot2 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "log_income",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )
            boot3 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "ideology1",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )

            expect_equal(abs(teststat(boot1)),
              abs(dof_tstat[1]),
              ignore_attr = TRUE
            )
            expect_equal(abs(teststat(boot2)),
              abs(dof_tstat[2]),
              ignore_attr = TRUE
            )
            expect_equal(abs(teststat(boot3)),
              abs(dof_tstat[3]),
              ignore_attr = TRUE
            )


            if (engine != "R-lean") {

            }
            # twoway clustering
            feols_fit <-
              fixest::feols(
                proposition_vote ~ treatment + ideology1 + log_income,
                data = fwildclusterboot:::create_data(
                  N = 1000,
                  N_G1 = 20,
                  icc1 = 0.81,
                  N_G2 = 10,
                  icc2 = 0.01,
                  numb_fe1 = 10,
                  numb_fe2 = 10,
                  seed = 970691
                ),
                cluster = ~ group_id1 + group_id2,
                ssc = fixest::ssc(
                  adj = adj,
                  # fixef.K = "full",
                  cluster.adj = cluster.adj,
                  cluster.df = cluster.df
                )
              )


            dof_tstat <-
              fixest::coeftable(feols_fit)[c(
                "treatment", "log_income",
                "ideology1"
              ), 3]


            boot1 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1", "group_id2"),
                  B = B,
                  param = "treatment",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )
            boot2 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1", "group_id2"),
                  B = B,
                  param = "log_income",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )
            boot3 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1", "group_id2"),
                  B = B,
                  param = "ideology1",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )
            expect_equal(abs(teststat(boot1)),
              abs(dof_tstat[1]),
              ignore_attr = TRUE
            )
            expect_equal(abs(teststat(boot2)),
              abs(dof_tstat[2]),
              ignore_attr = TRUE
            )
            expect_equal(abs(teststat(boot3)),
              abs(dof_tstat[3]),
              ignore_attr = TRUE
            )
          }
        }
      }
    }
  }

  for (engine in c("R-lean")) {
    for (adj in c(TRUE, FALSE)) {
      for (cluster.adj in c(TRUE, FALSE)) {
        for (cluster.df in c("conventional", "min")) {
          for (impose_null in c(TRUE)) {
            # oneway clustering
            feols_fit <-
              fixest::feols(
                proposition_vote ~ treatment + ideology1 + log_income,
                data = fwildclusterboot:::create_data(
                  N = 1000,
                  N_G1 = 20,
                  icc1 = 0.81,
                  N_G2 = 10,
                  icc2 = 0.01,
                  numb_fe1 = 10,
                  numb_fe2 = 10,
                  seed = 970691
                ),
                cluster = ~group_id1,
                ssc = fixest::ssc(
                  adj = adj,
                  cluster.adj = cluster.adj,
                  cluster.df = cluster.df
                )
              )

            dof_tstat <-
              fixest::tstat(feols_fit)[c(
                "treatment", "log_income",
                "ideology1"
              )]


            boot1 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "treatment",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )

            boot2 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "log_income",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )
            boot3 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "ideology1",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )

            expect_equal(abs(teststat(boot1)),
              abs(dof_tstat[1]),
              ignore_attr = TRUE
            )
            expect_equal(abs(teststat(boot2)),
              abs(dof_tstat[2]),
              ignore_attr = TRUE
            )
            expect_equal(abs(teststat(boot3)),
              abs(dof_tstat[3]),
              ignore_attr = TRUE
            )
          }
        }
      }
    }
  }
})


test_that("t-stat equivalence OLS - WildBootTests", {
  skip_on_cran()
  skip_if_not(
    fwildclusterboot:::find_proglang("julia"),
    message = "skip test as julia installation not found."
  )

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
        seed = 970691
      )
    )

  B <- 999

  for (engine in c("WildBootTests.jl")) {
    for (adj in c(TRUE, FALSE)) {
      for (cluster.adj in c(TRUE, FALSE)) {
        for (cluster.df in c("conventional", "min")) {
          for (impose_null in c(TRUE, FALSE)) {
            # oneway clustering
            feols_fit <-
              fixest::feols(
                proposition_vote ~ treatment + ideology1 + log_income,
                data = fwildclusterboot:::create_data(
                  N = 1000,
                  N_G1 = 20,
                  icc1 = 0.81,
                  N_G2 = 10,
                  icc2 = 0.01,
                  numb_fe1 = 10,
                  numb_fe2 = 10,
                  seed = 970691
                ),
                cluster = ~group_id1,
                ssc = fixest::ssc(
                  adj = adj,
                  cluster.adj = cluster.adj,
                  cluster.df = cluster.df
                )
              )

            dof_tstat <-
              fixest::tstat(feols_fit)[c(
                "treatment", "log_income",
                "ideology1"
              )]


            boot1 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "treatment",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )

            boot2 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "log_income",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )
            boot3 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1"),
                  B = B,
                  param = "ideology1",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )

            expect_equal(abs(boot1$t_stat),
              abs(dof_tstat[1]),
              ignore_attr = TRUE
            )
            expect_equal(abs(boot2$t_stat),
              abs(dof_tstat[2]),
              ignore_attr = TRUE
            )
            expect_equal(abs(boot3$t_stat),
              abs(dof_tstat[3]),
              ignore_attr = TRUE
            )


            # twoway clustering
            feols_fit <-
              fixest::feols(
                proposition_vote ~ treatment + ideology1 + log_income,
                data = fwildclusterboot:::create_data(
                  N = 1000,
                  N_G1 = 20,
                  icc1 = 0.81,
                  N_G2 = 10,
                  icc2 = 0.01,
                  numb_fe1 = 10,
                  numb_fe2 = 10,
                  seed = 970691
                ),
                cluster = ~ group_id1 + group_id2,
                ssc = fixest::ssc(
                  adj = adj,
                  # fixef.K = "full",
                  cluster.adj = cluster.adj,
                  cluster.df = cluster.df
                )
              )


            dof_tstat <-
              fixest::coeftable(feols_fit)[c(
                "treatment", "log_income",
                "ideology1"
              ), 3]


            boot1 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1", "group_id2"),
                  B = B,
                  param = "treatment",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )
            boot2 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1", "group_id2"),
                  B = B,
                  param = "log_income",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )
            boot3 <-
              suppressWarnings(
                fwildclusterboot::boottest(
                  lm_fit,
                  clustid = c("group_id1", "group_id2"),
                  B = B,
                  param = "ideology1",
                  ssc = boot_ssc(
                    adj = adj,
                    cluster.adj = cluster.adj,
                    cluster.df = cluster.df
                  ),
                  impose_null = impose_null,
                  engine = engine
                )
              )

            expect_equal(abs(teststat(boot1)),
              abs(dof_tstat[1]),
              ignore_attr = TRUE
            )
            expect_equal(abs(teststat(boot2)),
              abs(dof_tstat[2]),
              ignore_attr = TRUE
            )
            expect_equal(abs(teststat(boot3)),
              abs(dof_tstat[3]),
              ignore_attr = TRUE
            )
          }
        }
      }
    }
  }
})
# exact tests

test_that("t-stat equivalence OLS q > 1", {
  skip_on_cran()
  skip_if_not(
    fwildclusterboot:::find_proglang("julia"),
    message = "skip test as julia installation not found."
  )

  wald_test <- function(run_this_test) {
    if (run_this_test) {
      reltol <- 0.002

      N <- 1000
      data1 <<- fwildclusterboot:::create_data(
        N = 1000,
        N_G1 = 20,
        icc1 = 0.5,
        N_G2 = 20,
        icc2 = 0.2,
        numb_fe1 = 10,
        numb_fe2 = 10,
        seed = 90864369,
        weights = 1:N / N
      )


      feols_fit <-
        fixest::feols(proposition_vote ~ treatment + log_income + year,
          data = data1
        )
      lm_fit <- lm(proposition_vote ~ treatment + log_income + year,
        data = data1
      )

      feols_fit_weights <-
        fixest::feols(
          proposition_vote ~ treatment + log_income + year,
          data = data1,
          weights = data1$weights
        )
      lm_fit_weights <-
        lm(
          proposition_vote ~ treatment + log_income + year,
          data = data1,
          weights = data1$weights
        )



      N <- nrow(data1)
      k <- length(coef(lm_fit))
      G <- length(unique(data1$group_id1))



      type <- "rademacher"
      p_val_type <- "two-tailed"
      # impose_null <- TRUE

      # OLS


      # 1) oneway clustering

      # one hypothesis
      R <-
        clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit))
      boot_jl <- suppressWarnings(mboottest(
        lm_fit,
        R = R,
        clustid = "group_id1",
        B = 999
      ))

      # sW <- coeftest(object, vcov = sandwich::vcovCL(object,
      # cluster = ~ group_id1))
      wald_stat <-
        fixest::wald(feols_fit, "treatment", cluster = ~group_id1)

      expect_equal(boot_jl$teststat, sqrt(wald_stat$stat), ignore_attr = TRUE)

      # two hypotheses
      R <-
        clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit))
      boot_jl <- suppressWarnings(suppressWarnings(
        fwildclusterboot::mboottest(
          floattype = "Float64",
          object = lm_fit,
          R = R,
          clustid = "group_id1",
          B = 999,
          ssc = fwildclusterboot::boot_ssc(
            adj = TRUE,
            cluster.adj = TRUE
          )
        )
      ))

      wald_stat <-
        fixest::wald(feols_fit, "Inter|treatment", cluster = ~group_id1)

      expect_equal(boot_jl$teststat, wald_stat$stat, ignore_attr = TRUE)


      # one hypothesis
      R <-
        clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit))
      boot_jl <- suppressWarnings(
        fwildclusterboot::mboottest(
          floattype = "Float64",
          lm_fit,
          R = R,
          clustid = c("group_id1", "group_id2"),
          B = 999,
          ssc = boot_ssc(cluster.df = "min")
        )
      )

      # sW <- coeftest(object, vcov = sandwich::vcovCL(object,
      # cluster = ~ group_id1))
      wald_stat <- fixest::wald(
        feols_fit,
        "treatment",
        cluster = ~ group_id1 + group_id2,
        ssc = fixest::ssc(cluster.df = "min")
      )

      expect_equal(teststat(boot_jl), sqrt(wald_stat$stat),
        ignore_attr = TRUE
      )

      # two hypotheses
      R <-
        clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit))
      boot_jl <- suppressWarnings(
        fwildclusterboot::mboottest(
          floattype = "Float64",
          object = lm_fit,
          R = R,
          clustid = "group_id1",
          B = 999,
          ssc = fwildclusterboot::boot_ssc(
            adj = TRUE,
            cluster.adj = TRUE
          )
        )
      )

      wald_stat <- fixest::wald(
        feols_fit,
        "Inter|treatment",
        cluster = ~group_id1,
        cluster.df = "conventional"
      )

      expect_equal(teststat(boot_jl), wald_stat$stat, ignore_attr = TRUE)



      # WLS


      # 1) oneway clustering

      # one hypothesis
      R <-
        clubSandwich::constrain_zero(
          constraints = 2,
          coefs = coef(lm_fit_weights)
        )
      boot_jl <-
        suppressWarnings(
          fwildclusterboot::mboottest(
            floattype = "Float64",
            lm_fit_weights,
            R = R,
            clustid = "group_id1",
            B = 999
          )
        )


      wald_stat <-
        fixest::wald(feols_fit_weights, "treatment", cluster = ~group_id1)


      expect_equal(teststat(boot_jl), sqrt(wald_stat$stat),
        ignore_attr = TRUE
      )

      # two hypotheses
      R <-
        clubSandwich::constrain_zero(
          constraints = 1:2,
          coefs = coef(lm_fit_weights)
        )
      boot_jl <- suppressWarnings(
        fwildclusterboot::mboottest(
          floattype = "Float64",
          object = lm_fit_weights,
          R = R,
          clustid = "group_id1",
          B = 999,
          ssc = fwildclusterboot::boot_ssc(
            adj = TRUE,
            cluster.adj = TRUE
          )
        )
      )

      wald_stat <-
        fixest::wald(feols_fit_weights,
          "Inter|treatment",
          cluster = ~group_id1
        )

      expect_equal(teststat(boot_jl), wald_stat$stat, ignore_attr = TRUE)


      # one hypothesis
      R <-
        clubSandwich::constrain_zero(
          constraints = 2,
          coefs = coef(lm_fit_weights)
        )
      boot_jl <- suppressWarnings(
        fwildclusterboot::mboottest(
          floattype = "Float64",
          lm_fit_weights,
          R = R,
          clustid = c("group_id1", "group_id2"),
          B = 999,
          type = type,
          p_val_type = p_val_type,
          ssc = boot_ssc(cluster.df = "min")
        )
      )

      wald_stat <- fixest::wald(
        feols_fit_weights,
        "treatment",
        cluster = ~ group_id1 + group_id2,
        ssc = fixest::ssc(cluster.df = "min")
      )

      expect_equal(teststat(boot_jl), sqrt(wald_stat$stat),
        ignore_attr = TRUE
      )

      # two hypotheses
      R <-
        clubSandwich::constrain_zero(
          constraints = 1:2,
          coefs = coef(lm_fit_weights)
        )
      boot_jl <- suppressWarnings(
        fwildclusterboot::mboottest(
          floattype = "Float64",
          object = lm_fit_weights,
          R = R,
          clustid = "group_id1",
          B = 999,
          ssc = boot_ssc(cluster.adj = TRUE)
        )
      )

      wald_stat <-
        fixest::wald(
          feols_fit_weights,
          "Inter|treatment",
          cluster = ~group_id1,
          cluster.df = "conventional"
        )

      expect_equal(teststat(boot_jl), wald_stat$stat, ignore_attr = TRUE)
    }
  }

  wald_test(run_this_test = TRUE)
})

test_that("t-stat equivalence IV", {
  skip_on_cran()
  skip_if_not(
    fwildclusterboot:::find_proglang("julia"),
    message = "skip test as julia installation not found."
  )


  iv_test <- function(run_this_test) {
    # Note: Test with Float64 for exact match
    if (run_this_test) {
      set.seed(123)
      data("SchoolingReturns", package = "ivreg")

      # drop all NA values from SchoolingReturns
      data1 <<- na.omit(SchoolingReturns)
      ivreg_fit <-
        ivreg::ivreg(
          log(wage) ~ education + age + ethnicity + smsa + south + parents14 |
            nearcollege + age + ethnicity + smsa + south + parents14,
          data = data1
        )
      vcov1 <- sandwich::vcovCL(
        ivreg_fit,
        cluster = ~kww,
        cadjust = TRUE,
        type = "HC1"
      )
      vcov2 <- sandwich::vcovCL(
        ivreg_fit,
        cluster = ~ smsa + kww,
        cadjust = TRUE,
        type = "HC1"
      )

      res1 <- lmtest::coeftest(ivreg_fit, vcov1)
      res_df1 <- as.data.frame(broom::tidy(res1))

      res2 <- lmtest::coeftest(ivreg_fit, vcov2)
      res_df2 <- as.data.frame(broom::tidy(res2))

      boot_ivreg1 <- suppressWarnings(
        boottest(
          floattype = "Float64",
          object = ivreg_fit,
          B = 999,
          param = "education",
          clustid = "kww",
          type = "mammen",
          impose_null = TRUE
        )
      )

      expect_equal(teststat(boot_ivreg1),
        as.vector(res_df1[res_df1$term == "education", "statistic"]),
        ignore_attr = TRUE
      )


      boot_ivreg2 <- boottest(
        floattype = "Float64",
        object = ivreg_fit,
        B = 999,
        param = "education",
        clustid = c("smsa", "kww"),
        type = "rademacher"
      )


      expect_equal(teststat(boot_ivreg2), res_df2[
        res_df2$term == "education",
        "statistic"
      ])
    }
  }

  iv_test(run_this_test = TRUE)
})
