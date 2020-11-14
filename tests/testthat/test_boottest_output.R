# general error: does not find voters -> within testthat, does not
# search in the right

# test if boottest() results in different results for the three
# supported regression objects and compares with sandwich standard errors

B <- 1000
seed <- 218019
set.seed(seed)
voters <- create_data_1(N = 2000, N_G = 50, icc = 0.01)

test_that("boottest() returns similar results for the same input, independent of regression function used",{
  
  
  # # estimate regressions
  lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration , weights = NULL, data = voters)
  felm_fit1 <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income  + Q1_immigration, weights = NULL, data = voters)
  feols_fit1 <- feols(proposition_vote ~ treatment + ideology + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income + Q1_immigration, weights = NULL, data = voters)
  # 
  boot_lm = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
  #expect_equal(boot_lm, 1)
  boot_fixest = boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
  #expect_equal(boot_fixest, 1)
  boot_fixest1 = boottest(feols_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0)
  boot_felm = boottest(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
  boot_felm1 = boottest(felm_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
  # # 
  expect_equivalent(tidy(boot_lm), tidy(boot_fixest), tolerance = 1e-2)
  # # expect_equivalent(tidy(boot_fixest), tidy(boot_fixest1), tolerance = 1e-2)
  # expect_equivalent(tidy(boot_fixest1), tidy(boot_felm), tolerance = 1e-2)
  # expect_equivalent(tidy(boot_felm), tidy(boot_felm1), tolerance = 1e-2)
  # 
  # in fact boot_lm, boot_fixest1 & boot_felm1 should be identical
  # expect_equal(tidy(boot_lm), tidy(boot_felm))
  # expect_equal(tidy(boot_felm), tidy(boot_fixest))
  
  # ... and so should boot_felm1 and boot_fixest1
  # expect_equal(tidy(boot_felm1), tidy(boot_fixest1))

})


test_that("boottest() returns similar results to sandwich se if icc is low",{
  
  
  #for(i in 1:3){
    
  B <- 10000
  seed <- 1
  set.seed(seed)
  voters <- create_data_1(N = 10000, N_G = 50, icc = 0.01)
  
  # estimate regressions
  lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration , weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration, weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)

  boot_lm = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
  boot_fixest = boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
  boot_felm = boottest(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)

  p_val_fixest <- coeftable(feols_fit, se = "cluster", cluster = voters$group_id)[1, 4]
   
  expect_equivalent(boot_lm$p_val, p_val_fixest, tolerance = 1e-2)
  expect_equivalent(boot_fixest$p_val, p_val_fixest, tolerance = 1e-2)
  expect_equivalent(boot_felm$p_val, p_val_fixest, tolerance = 1e-2)
  
  #}
  
})
