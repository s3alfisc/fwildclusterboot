# test the interface of the boottest() function

library("fwildclusterboot")

test_that("test output of boottest() without any fixed effects in regression objects, oneway clustering",{
  # Step 1: create data
 
  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)

  # -------------------------------------------------------------------------------------------------------------------------- # 
  # Test 1: no fixed effects
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
  
  boot_lm <-  boottest(object = lm_fit, clustid =  "group_id1", B = 100000, seed = 911, param = "treatment", conf_int = TRUE)
  boot_fixest <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 911, param = "treatment", conf_int = TRUE, beta = 0)
  boot_felm <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 911, param = "treatment", conf_int = TRUE)

  expect_is(boot_lm, "boottest")  
  expect_is(boot_fixest, "boottest")  
  expect_is(boot_felm, "boottest")  
  
  # equality of p-vals 
  expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2 / 5)
  expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2 / 5)
  
  # equality of confidence sets 
  expect_equal(boot_lm$conf_int, boot_fixest$conf_int, tolerance = 1e-2 / 5)
  expect_equal(boot_fixest$conf_int, boot_felm$conf_int, tolerance = 1e-2 / 5)
  
  # equality of point estimate
  expect_equal(boot_lm$point_estimate, boot_fixest$point_estimate)
  expect_equal(as.numeric(boot_fixest$point_estimate[1]), boot_felm$point_estimate)
  
  # t-stats 
  expect_equal(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 1e-2 / 5)
  expect_equal(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 1e-2 / 5)
  
  # N, B, N_G, clustid
  expect_identical(boot_lm$N, boot_fixest$N)
  expect_identical(boot_fixest$N, boot_felm$N)
  
  # B
  expect_identical(boot_lm$B, boot_fixest$B)
  expect_identical(boot_fixest$B, boot_felm$B)
  
  # N_G
  expect_identical(as.numeric(boot_lm$N_G), as.numeric(boot_fixest$N_G))
  expect_identical(as.numeric(boot_fixest$N_G), as.numeric(boot_felm$N_G))
  
  # clustid
  expect_identical(boot_lm$clustid, boot_fixest$clustid)
  expect_identical(boot_fixest$clustid, boot_felm$clustid)
  
  
  
})


test_that("test output of boottest() without any fixed effects in regression objects, twoway clustering",{
  # Step 1: create data
  voters <- create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)
  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  # Test 1: no fixed effects
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
  
  boot_lm <-  boottest(object = lm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 911, param = "treatment", conf_int = TRUE)
  boot_fixest <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 911, param = "treatment", conf_int = TRUE, beta = 0)
  boot_felm <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 911, param = "treatment", conf_int = TRUE)
  
  expect_is(boot_lm, "boottest")  
  expect_is(boot_fixest, "boottest")  
  expect_is(boot_felm, "boottest")  
  
  # equality of p-vals 
  expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2 / 5)
  expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2 / 5)
  
  # equality of confidence sets 
  expect_equal(boot_lm$conf_int, boot_fixest$conf_int, tolerance = 1e-2 / 5)
  expect_equal(boot_fixest$conf_int, boot_felm$conf_int, tolerance = 1e-2 / 5)
  
  # equality of point estimate
  expect_equal(boot_lm$point_estimate, boot_fixest$point_estimate)
  expect_equal(as.numeric(boot_fixest$point_estimate), as.numeric(boot_felm$point_estimate))
  
  # t-stats 
  expect_equal(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 1e-2 )
  expect_equal(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 1e-2)
  
  # N, B, N_G, clustid
  expect_identical(boot_lm$N, boot_fixest$N)
  expect_identical(boot_fixest$N, boot_felm$N)
  
  # B
  expect_identical(boot_lm$B, boot_fixest$B)
  expect_identical(boot_fixest$B, boot_felm$B)
  
  # N_G
  expect_identical(boot_lm$N_G, boot_fixest$N_G)
  expect_identical(boot_fixest$N_G, boot_felm$N_G)
  
  # clustid
  expect_identical(boot_lm$clustid, boot_fixest$clustid)
  expect_identical(boot_fixest$clustid, boot_felm$clustid)
  
  
  
})

test_that("test with one fixed effects in regression objects, oneway clustering",{
  # Step 1: create data
  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)
  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  # Test 1: no fixed effects
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)
  
  boot_lm <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 911, param = "treatment", conf_int = FALSE)
  boot_fixest <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 911, param = "treatment", conf_int = FALSE, beta = 0)
  boot_felm <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 911, param = "treatment", conf_int = FALSE)
  
  boot_fixest_fe <- boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 100000, seed = 911, param = "treatment", conf_int = FALSE, beta = 0)
  boot_felm_fe <- boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration",B = 100000, seed = 911, param = "treatment", conf_int = FALSE)
  
  
  expect_is(boot_lm, "boottest")  
  expect_is(boot_fixest, "boottest")  
  expect_is(boot_felm, "boottest")  
  expect_is(boot_fixest_fe, "boottest")  
  expect_is(boot_felm_fe, "boottest")  
  
  # p-val
  expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2 / 2)
  expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2 / 2)
  expect_equal(boot_felm$p_val, boot_fixest_fe$p_val, tolerance = 1e-2 / 2)
  expect_equal(boot_fixest_fe$p_val, boot_felm_fe$p_val, tolerance = 1e-2 / 2)
  
  # equality of confidence sets 
  expect_equal(boot_lm$conf_int, boot_fixest$conf_int, tolerance = 1e-2 / 2)
  expect_equal(boot_fixest$conf_int, boot_felm$conf_int, tolerance = 1e-2 / 2)
  expect_equal(boot_felm$conf_int, boot_fixest_fe$conf_int, tolerance = 1e-2 / 2)
  expect_equal(boot_fixest_fe$conf_int, boot_felm_fe$conf_int, tolerance = 1e-2 / 2)
  
  # equality of point estimate
  expect_equal(boot_lm$point_estimate, boot_fixest$point_estimate)
  expect_equal(as.numeric(boot_fixest$point_estimate), as.numeric(boot_felm$point_estimate))
  expect_equal(as.numeric(boot_felm$point_estimate), as.numeric(boot_fixest_fe$point_estimate))
  expect_equal(as.numeric(boot_fixest_fe$point_estimate), as.numeric(boot_felm_fe$point_estimate))
  
  # t-stats 
  expect_equal(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 1e-2 )
  expect_equal(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 1e-2)
  expect_equal(boot_felm$t_stat, boot_fixest_fe$t_stat, tolerance = 1e-2)
  expect_equal(boot_fixest_fe$t_stat, boot_felm_fe$t_stat, tolerance = 1e-2)  
  
  # N, B, N_G, clustid
  expect_equal(boot_lm$N, boot_fixest$N)
  expect_equal(boot_fixest$N, boot_felm$N)
  expect_equal(boot_felm$N, boot_fixest_fe$N)
  expect_equal(boot_fixest_fe$N, boot_felm_fe$N)  
  
  # B
  expect_equal(boot_lm$B, boot_fixest$B)
  expect_equal(boot_fixest$B, boot_felm$B)
  expect_equal(boot_felm$B, boot_fixest_fe$B)
  expect_equal(boot_fixest_fe$B, boot_felm_fe$B)   
  
  # N_G
  expect_equal(as.numeric(boot_lm$N_G), as.numeric(boot_fixest$N_G))
  expect_equal(as.numeric(boot_fixest$N_G), as.numeric(boot_felm$N_G))
  expect_equal(as.numeric(boot_felm$N_G), as.numeric(boot_fixest_fe$N_G))
  expect_equal(as.numeric(boot_fixest_fe$N_G), as.numeric(boot_felm_fe$N_G))
  
  # clustid
  expect_equal(boot_lm$clustid, boot_fixest$clustid)
  expect_equal(boot_fixest$clustid, boot_felm$clustid)
  expect_equal(boot_felm$clustid, boot_fixest_fe$clustid)
  expect_equal(boot_fixest_fe$clustid, boot_felm_fe$clustid)  
})


test_that("test handling of missing values in cluster variables",{
  # Step 1: create data
  voters <- create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)
  voters[1, group_id1 := NA]
  voters[1, group_id2 := NA]
  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)
  
  expect_warning(boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 911, param = "treatment", conf_int = FALSE))
  expect_warning(boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 911, param = "treatment", conf_int = FALSE, beta = 0))
  expect_warning(boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 911, param = "treatment", conf_int = FALSE))
  
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | group_id1, weights = NULL, data = voters)
  expect_warning(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 911, param = "treatment", conf_int = FALSE))
  
  # it is not allowed to delete a cluster variable specified in felm from boottest()
  expect_error(boottest(object = felm_fit, clustid =  c("group_id2"), B = 100000, seed = 911, param = "treatment", conf_int = FALSE))

})



test_that("check if argument checks via dreamerr work properly", {
  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)
  voters[1, group_id1 := NA]
  voters[1, group_id2 := NA]
  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  
  
  
})



test_that("Different seeds lead to similar results", {
  
  voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 94241)
  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  # Test 1: oneway clustering
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
  felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
  
  boot_lm1 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 1, param = "treatment", conf_int = FALSE)
  boot_lm2 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 2, param = "treatment", conf_int = FALSE)
  boot_lm3 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 3, param = "treatment", conf_int = FALSE)
  
  boot_fixest1 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 4, param = "treatment", conf_int = FALSE, beta = 0)
  boot_fixest2 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 5, param = "treatment", conf_int = FALSE, beta = 0)
  boot_fixest3 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 6, param = "treatment", conf_int = FALSE, beta = 0)
  
  boot_felm1 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 7, param = "treatment", conf_int = FALSE)
  boot_felm2 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 8, param = "treatment", conf_int = FALSE)
  boot_felm3 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 9, param = "treatment", conf_int = FALSE)
  
  expect_equal(boot_lm1$p_val, boot_lm2$p_val, tol = 1e-2 / 2)
  expect_equal(boot_lm2$p_val, boot_lm3$p_val, tol = 1e-2/ 2)
  
  expect_equal(boot_fixest1$p_val, boot_fixest2$p_val, tol = 1e-2/ 2)
  expect_equal(boot_fixest2$p_val, boot_fixest3$p_val, tol = 1e-2/ 2)
  
  expect_equal(boot_felm1$p_val, boot_felm2$p_val, tol = 1e-2/ 2)
  expect_equal(boot_felm2$p_val, boot_felm3$p_val, tol = 1e-2/ 2)
  
  # ------------------------------------------------------------------------------------------ 
  # test 2: twoway clustering 

  boot_lm1 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 1, param = "treatment", conf_int = FALSE)
  boot_lm2 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 2, param = "treatment", conf_int = FALSE)
  boot_lm3 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 3, param = "treatment", conf_int = FALSE)
  
  boot_fixest1 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 4, param = "treatment", conf_int = FALSE, beta = 0)
  boot_fixest2 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 5, param = "treatment", conf_int = FALSE, beta = 0)
  boot_fixest3 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 6, param = "treatment", conf_int = FALSE, beta = 0)
  
  boot_felm1 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 7, param = "treatment", conf_int = FALSE)
  boot_felm2 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 8, param = "treatment", conf_int = FALSE)
  boot_felm3 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 9, param = "treatment", conf_int = FALSE)
  
  expect_equal(boot_lm1$p_val, boot_lm2$p_val, tol = 1e-2 / 2)
  expect_equal(boot_lm2$p_val, boot_lm3$p_val, tol = 1e-2/ 2)
  
  expect_equal(boot_fixest1$p_val, boot_fixest2$p_val, tol = 1e-2/ 2)
  expect_equal(boot_fixest2$p_val, boot_fixest3$p_val, tol = 1e-2/ 2)
  
  expect_equal(boot_felm1$p_val, boot_felm2$p_val, tol = 1e-2/ 2)
  expect_equal(boot_felm2$p_val, boot_felm3$p_val, tol = 1e-2/ 2)
  
})



