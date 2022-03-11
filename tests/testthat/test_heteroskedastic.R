# test_that("test heteroskedastic boottest", {
#   
#   set.seed(98013)
#   library(fixest)
#   library(lfe)
#   library(fwildclusterboot)
#   skip()
#   # devtools::load_all()
#   data <- fwildclusterboot:::create_data(N = 1000,
#                                          N_G1 = 1000,
#                                          icc1 = 0.81,
#                                          N_G2 = 10,
#                                          icc2 = 0.01,
#                                          numb_fe1 = 10,
#                                          numb_fe2 = 10,
#                                          seed = 97069)
#   lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income ,
#                 data = data)
# 
#   feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income ,
#                 data = data)
#   
#   felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income ,
#                    data = data)
#   
#   boot_lm <- boottest(lm_fit, param = "treatment", B = 9999, nthreads = 1)
#   boot_feols <- boottest(feols_fit, param = "treatment", B = 9999, nthreads = 1)
#   boot_felm <- boottest(felm_fit, param = "treatment", B = 9999, nthreads = 1)
# 
#   # tidy(boot_lm)
#   # tidy(boot_feols)
#   # tidy(boot_felm)
#   
#   
#   # p_boot <- generics::tidy(boot_lm)
#   p_hc <- broom::tidy(lmtest::coeftest(lm_fit, sandwich::vcovHC(lm_fit)))[2, 5]
#   #expect_equal(boot_lm$p_val, p_hc, tolerance = 0.02, ignore_attr = TRUE)
# })
