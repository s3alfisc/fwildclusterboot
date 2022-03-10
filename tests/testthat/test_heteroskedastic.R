test_that("test heteroskedastic boottest", {
  
  set.seed(98013)
  library(fixest)
  library(fwildclusterboot)
  devtools::load_all()
  data <- fwildclusterboot:::create_data(N = 1000,
                                         N_G1 = 1000,
                                         icc1 = 0.81,
                                         N_G2 = 10,
                                         icc2 = 0.01,
                                         numb_fe1 = 10,
                                         numb_fe2 = 10,
                                         seed = 97069)
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income ,
                data = data)

  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income ,
                data = data)
  
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income ,
                   data = data)
  
  pracma::tic()
  boot_lm <- boottest(lm_fit, param = "treatment", B = 9999, nthreads = 1)
  pracma::toc()
  
  pracma::tic()
  boot_feols <- boottest(feols_fit, param = "treatment", B = 9999, nthreads = 1)
  pracma::toc()
  
  pracma::tic()
  boot_felm <- boottest(felm_fit, param = "treatment", B = 9999, nthreads = 1)
  pracma::toc()
  
  tidy(boot_lm)
  tidy(boot_feols)
  tidy(boot_felm)
  
  
  pracma::tic()
  sandwich::vcovBS(lm_fit, R = 9999)
  pracma::toc()
  
  p_boot <- generics::tidy(boot_lm)
  p_hc <- broom::tidy(lmtest::coeftest(lm_fit, sandwich::vcovHC(lm_fit)))[2, 5]
  expect_equal(p_boot, p_hc)
})