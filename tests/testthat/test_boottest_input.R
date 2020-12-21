# test the interface of the boottest() function

library("fwildclusterboot")
# setwd("C:/Users/alexa/Dropbox/fwildclusterboot/R")
# file.sources = list.files(pattern="*.R")
# sapply(file.sources, source, .GlobalEnv)

test_that("test without any fixed effects in regression objects",{
  # Step 1: create data
  B <- 10000
  seed <- 94241
  set.seed(seed)
  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 40, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)

  # -------------------------------------------------------------------------------------------------------------------------- # 
  # Test 1: no fixed effects
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
  
  boot_lm <-  boottest(object = lm_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = FALSE)
  boot_fixest <- boottest(object = feols_fit, clustid = c("group_id1"), B = B, seed = seed, param = "treatment", conf_int = FALSE, beta = 0)
  boot_felm <- boottest(object = felm_fit, clustid =  "group_id1", B = B, seed = seed, param = "treatment", conf_int = FALSE)

  expect_is(boot_lm, "boottest")  
  expect_is(boot_fixest, "boottest")  
  expect_is(boot_felm, "boottest")  
  
  expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2)
  expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2)
  
})

test_that("test with one fixed effects in regression objects",{
  # Step 1: create data
  B <- 10000
  seed <- 94411
  set.seed(seed)
  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 40, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)
  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  # Test 1: no fixed effects
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)
  
  boot_lm <-  boottest(object = lm_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = FALSE)
  boot_fixest <- boottest(object = feols_fit, clustid = c("group_id1"), B = B, seed = seed, param = "treatment", conf_int = FALSE, beta = 0)
  boot_felm <- boottest(object = felm_fit, clustid =  "group_id1", B = B, seed = seed, param = "treatment", conf_int = FALSE)
  
  boot_fixest_fe <- boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = B, seed = seed, param = "treatment", conf_int = FALSE, beta = 0)
  boot_felm_fe <- boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration",B = B, seed = seed, param = "treatment", conf_int = FALSE)
  
  
  expect_is(boot_lm, "boottest")  
  expect_is(boot_fixest, "boottest")  
  expect_is(boot_felm, "boottest")  
  expect_is(boot_fixest_fe, "boottest")  
  expect_is(boot_felm_fe, "boottest")  
  
  expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2)
  expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2)
  expect_equal(boot_felm$p_val, boot_fixest_fe$p_val, tolerance = 1e-2)
  expect_equal(boot_fixest_fe$p_val, boot_felm_fe$p_val, tolerance = 1e-2)
  
})


test_that("test handling of missing values in cluster variables",{
  # Step 1: create data
  B <- 10000
  seed <- 94411
  set.seed(seed)
  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 40, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)
  voters[1, group_id1 := NA]
  voters[1, group_id2 := NA]
  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)
  
  expect_warning(boottest(object = lm_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = FALSE))
  expect_warning(boottest(object = feols_fit, clustid = c("group_id1"), B = B, seed = seed, param = "treatment", conf_int = FALSE, beta = 0))
  expect_warning(boottest(object = felm_fit, clustid =  "group_id1", B = B, seed = seed, param = "treatment", conf_int = FALSE))
  
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | group_id1, weights = NULL, data = voters)
  expect_warning(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = B, seed = seed, param = "treatment", conf_int = FALSE))
  
  # it is not allowed to delete a cluster variable specified in felm from boottest()
  expect_error(boottest(object = felm_fit, clustid =  c("group_id2"), B = B, seed = seed, param = "treatment", conf_int = FALSE))

})



test_that("check if argument checks via dreamerr work properly", {
  B <- 10000
  seed <- 9441
  set.seed(seed)
  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 40, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)
  voters[1, group_id1 := NA]
  voters[1, group_id2 := NA]
  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  
  
  
})



