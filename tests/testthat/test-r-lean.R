test_that("test lean cpp boottest", {

  set.seed(9809873)
  library(fixest)
  library(lfe)
  library(fwildclusterboot)
  # skip()
  # devtools::load_all()
  data1 <<- fwildclusterboot:::create_data(N = 1000,
                                         N_G1 = 1000,
                                         icc1 = 0.5,
                                         N_G2 = 10,
                                         icc2 = 0.01,
                                         numb_fe1 = 10,
                                         numb_fe2 = 10,
                                         seed = 2293)
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income ,
                data = data1)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income ,
                data = data1)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income ,
                   data = data1)

  
  # Test 1: heteroskedastic wild bootstrap
  # 
  # HC0
  # pracma::tic()
  boot_lm <- boottest(lm_fit,
                      param = "treatment",
                      B = 9999,
                      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                      nthreads = 1, 
                      seed = 1)
  boot_lm2 <- boottest(lm_fit,
                      param = "treatment",
                      B = 9999,
                      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                      nthreads = 1, 
                      seed = 2)
  boot_lm$p_val; boot_lm2$p_val
  # pracma::toc()
  
  boot_feols <- boottest(feols_fit,
                      param = "treatment", 
                      B = 9999,
                      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), 
                      nthreads = 1)
  boot_felm <- boottest(felm_fit,
                      param = "treatment", 
                      B = 9999,
                      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), 
                      nthreads = 1)
  
  # 2.155239
  res <- broom::tidy(
              lmtest::coeftest(
                lm_fit,
                sandwich::vcovHC(lm_fit, type = "HC0")
                )
  )[2,4:5]
  
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
                      nthreads = 1)
  boot_feols <- boottest(feols_fit,
                         param = "treatment", 
                         B = 9999,
                         ssc = boot_ssc(adj = TRUE, cluster.adj = FALSE), 
                         nthreads = 1)
  boot_felm <- boottest(felm_fit,
                        param = "treatment", 
                        B = 9999,
                        ssc = boot_ssc(adj = TRUE, cluster.adj = FALSE), 
                        nthreads = 1)
  
  res <- broom::tidy(
    lmtest::coeftest(
      lm_fit,
      sandwich::vcovHC(lm_fit, type = "HC1")
    )
  )[2,4:5]
  
  k <- length(coef(lm_fit))
  N <- nobs(lm_fit)
  
  # HC1 in sandwich : t / sqrt(n / (n -k))
  # in fwildclusterboot: t / sqrt((n-1) / n-k)
  ssc_corr <- (N-1) / N

  expect_equal(res$statistic / sqrt(ssc_corr), boot_lm$t_stat)
  expect_equal(res$statistic / sqrt(ssc_corr), boot_feols$t_stat)
  expect_equal(res$statistic / sqrt(ssc_corr), boot_felm$t_stat)
  
  expect_equal(res$p.value, boot_lm$p_val, tolerance = 0.05)
  expect_equal(res$p.value, boot_feols$p_val, tolerance = 0.05)
  expect_equal(res$p.value, boot_felm$p_val, tolerance = 0.05)
  
  
  
  
  # test oneway clustering
  # pracma::tic()
  boot_lm1 <-  boottest(feols_fit,
                        param = "treatment",
                        clustid = "group_id1",
                        B = 2999, 
                        boot_algo = "R-lean",
                        nthreads = 1, 
                        ssc = boot_ssc(adj = FALSE, 
                                       cluster.adj = FALSE))
  # pracma::toc()
  
  # pracma::tic()
  boot_lm2 <-  boottest(lm_fit,
                        param = "treatment",
                        clustid = "group_id1",
                        B = 2999, 
                        boot_algo = "R",
                        nthreads = 1, 
                        ssc = boot_ssc(adj = FALSE, 
                                       cluster.adj = FALSE))
  # pracma::toc()
  
  expect_equal(boot_lm1$p_val, boot_lm2$p_val, tolerance = 0.05)
  expect_equal(boot_lm1$t_stat, boot_lm2$t_stat)
  
  
  boot_lm1 <-  boottest(feols_fit,
                        param = "treatment",
                        clustid = "group_id1",
                        B = 9999, 
                        boot_algo = "R-lean",
                        nthreads = 1)
  # pracma::toc()
  
  # pracma::tic()
  boot_lm2 <-  boottest(feols_fit,
                        param = "treatment",
                        clustid = "group_id1",
                        B = 9999, 
                        boot_algo = "R",
                        nthreads = 1)
  # pracma::toc()
  
  expect_equal(boot_lm1$p_val, boot_lm2$p_val, tolerance = 0.05)
  expect_equal(boot_lm1$t_stat, boot_lm2$t_stat)

})
