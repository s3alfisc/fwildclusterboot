# # test the interface of the boottest() function
# 
library(fwildclusterboot)
library(fixest)
#library(lfe)

# ------------------------------------------------------------------------------------------------------  
# Test 1 
# test output of boottest() without any fixed effects in regression objects, oneway clustering
# ------------------------------------------------------------------------------------------------------ 
# Test 1: no fixed effects
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                           data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
#felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
#                      data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 1000, seed = 911, param = "treatment", conf_int = TRUE))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 1000, seed = 911, param = "treatment", conf_int = TRUE, beta = 0))
#boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 1000, seed = 911, param = "treatment", conf_int = TRUE))

expect_true(class(boot_lm) == "boottest")  
expect_true(class(boot_fixest) == "boottest")  
#expect_true(class(boot_felm) == "boottest")  

# equality of p-vals 
expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2 / 5)
#expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2 / 5)

# equality of confidence sets 
expect_equal(boot_lm$conf_int, boot_fixest$conf_int, tolerance = 1e-2 / 5)
#expect_equal(boot_fixest$conf_int, boot_felm$conf_int, tolerance = 1e-2 / 5)

# equality of point estimate
expect_equal(boot_lm$point_estimate, boot_fixest$point_estimate)
#expect_equal(as.numeric(boot_fixest$point_estimate[1]), boot_felm$point_estimate)

# t-stats 
expect_equal(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 1e-2 / 5)
#expect_equal(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 1e-2 / 5)

# N, B, N_G, clustid
expect_identical(boot_lm$N, boot_fixest$N)
#expect_identical(boot_fixest$N, boot_felm$N)

# B
expect_identical(boot_lm$B, boot_fixest$B)
#expect_identical(boot_fixest$B, boot_felm$B)

# N_G
expect_identical(as.numeric(boot_lm$N_G), as.numeric(boot_fixest$N_G))
#expect_identical(as.numeric(boot_fixest$N_G), as.numeric(boot_felm$N_G))

# clustid
expect_identical(boot_lm$clustid, boot_fixest$clustid)
#expect_identical(boot_fixest$clustid, boot_felm$clustid)


# 
# 
# test_that("test output of boottest() without any fixed effects in regression objects, twoway clustering",{
#   # Step 1: create data
#   #voters <- fwildclusterboot::create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)
#   
#   # -------------------------------------------------------------------------------------------------------------------------- # 
#   # Test 1: no fixed effects
#   lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)
#   feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#   felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#   
#   boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  c("group_id1", "group_id2"), B = 1000, seed = 911, param = "treatment", conf_int = TRUE))
#  
#   #voters <- fwildclusterboot::create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)
#   boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 1000, seed = 911, param = "treatment", conf_int = TRUE, beta = 0))
#   
#   #voters <- fwildclusterboot::create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)
#   boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 1000, seed = 911, param = "treatment", conf_int = TRUE))
#   # 
#   expect_is(boot_lm, "boottest")  
#   expect_is(boot_fixest, "boottest")  
#   expect_is(boot_felm, "boottest")  
#   # 
#   # equality of p-vals 
#   expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2 / 5)
#   expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2 / 5)
#   
#   # equality of confidence sets 
#   expect_equal(boot_lm$conf_int, boot_fixest$conf_int, tolerance = 1e-2 / 5)
#   expect_equal(boot_fixest$conf_int, boot_felm$conf_int, tolerance = 1e-2 / 5)
#   
#   # equality of point estimate
#   expect_equal(boot_lm$point_estimate, boot_fixest$point_estimate)
#   expect_equal(as.numeric(boot_fixest$point_estimate), as.numeric(boot_felm$point_estimate))
#   
#   # t-stats 
#   expect_equal(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 1e-2 )
#   expect_equal(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 1e-2)
#   
#   # N, B, N_G, clustid
#   expect_identical(boot_lm$N, boot_fixest$N)
#   expect_identical(boot_fixest$N, boot_felm$N)
#   
#   # B
#   expect_identical(boot_lm$B, boot_fixest$B)
#   expect_identical(boot_fixest$B, boot_felm$B)
#   
#   # N_G
#   expect_identical(boot_lm$N_G, boot_fixest$N_G)
#   expect_identical(boot_fixest$N_G, boot_felm$N_G)
#   
#   # clustid
#   expect_identical(boot_lm$clustid, boot_fixest$clustid)
#   expect_identical(boot_fixest$clustid, boot_felm$clustid)
#   
# }) 
# # 
# 
# test_that("test with one fixed effects in regression objects, oneway clustering",{
#  # Step 1: create data
# 
#  
#  # -------------------------------------------------------------------------------------------------------------------------- # 
#  # Test 1: no fixed effects
#  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, data = voters)
#  
#  boot_lm <-  boottest(object = lm_fit, clustid = "group_id1", B = 1000, seed = 911, param = "treatment", conf_int = FALSE)
#  
# 
#  boot_fixest <- boottest(object = feols_fit, clustid = c("group_id1"), B = 1000, seed = 911, param = "treatment", conf_int = FALSE, beta = 0)
#   
# 
#  boot_felm <- boottest(object = felm_fit, clustid =  "group_id1", B = 1000, seed = 911, param = "treatment", conf_int = FALSE)
#  
# 
#  boot_fixest_fe <- boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 1000, seed = 911, param = "treatment", conf_int = FALSE, beta = 0)
# 
# 
#  boot_felm_fe <- boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration",B = 1000, seed = 911, param = "treatment", conf_int = FALSE)
#  
#  
#  expect_is(boot_lm, "boottest")  
#  expect_is(boot_fixest, "boottest")  
#  expect_is(boot_felm, "boottest")  
#  expect_is(boot_fixest_fe, "boottest")  
#  expect_is(boot_felm_fe, "boottest")  
#  
#  # p-val
#  expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2 / 2)
#  expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2 / 2)
#  expect_equal(boot_felm$p_val, boot_fixest_fe$p_val, tolerance = 1e-2 / 2)
#  expect_equal(boot_fixest_fe$p_val, boot_felm_fe$p_val, tolerance = 1e-2 / 2)
#  
#  # equality of confidence sets 
#  expect_equal(boot_lm$conf_int, boot_fixest$conf_int, tolerance = 1e-2 / 2)
#  expect_equal(boot_fixest$conf_int, boot_felm$conf_int, tolerance = 1e-2 / 2)
#  expect_equal(boot_felm$conf_int, boot_fixest_fe$conf_int, tolerance = 1e-2 / 2)
#  expect_equal(boot_fixest_fe$conf_int, boot_felm_fe$conf_int, tolerance = 1e-2 / 2)
#  
#  # equality of point estimate
#  expect_equal(boot_lm$point_estimate, boot_fixest$point_estimate)
#  expect_equal(as.numeric(boot_fixest$point_estimate), as.numeric(boot_felm$point_estimate))
#  expect_equal(as.numeric(boot_felm$point_estimate), as.numeric(boot_fixest_fe$point_estimate))
#  expect_equal(as.numeric(boot_fixest_fe$point_estimate), as.numeric(boot_felm_fe$point_estimate))
#  
#  # t-stats 
#  expect_equal(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 1e-2 )
#  expect_equal(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 1e-2)
#  expect_equal(boot_felm$t_stat, boot_fixest_fe$t_stat, tolerance = 1e-2)
#  expect_equal(boot_fixest_fe$t_stat, boot_felm_fe$t_stat, tolerance = 1e-2)  
#  
#  # N, B, N_G, clustid
#  expect_equal(boot_lm$N, boot_fixest$N)
#  expect_equal(boot_fixest$N, boot_felm$N)
#  expect_equal(boot_felm$N, boot_fixest_fe$N)
#  expect_equal(boot_fixest_fe$N, boot_felm_fe$N)  
#  
#  # B
#  expect_equal(boot_lm$B, boot_fixest$B)
#  expect_equal(boot_fixest$B, boot_felm$B)
#  expect_equal(boot_felm$B, boot_fixest_fe$B)
#  expect_equal(boot_fixest_fe$B, boot_felm_fe$B)   
#  
#  # N_G
#  expect_equal(as.numeric(boot_lm$N_G), as.numeric(boot_fixest$N_G))
#  expect_equal(as.numeric(boot_fixest$N_G), as.numeric(boot_felm$N_G))
#  expect_equal(as.numeric(boot_felm$N_G), as.numeric(boot_fixest_fe$N_G))
#  expect_equal(as.numeric(boot_fixest_fe$N_G), as.numeric(boot_felm_fe$N_G))
#  
#  # clustid
#  expect_equal(boot_lm$clustid, boot_fixest$clustid)
#  expect_equal(boot_fixest$clustid, boot_felm$clustid)
#  expect_equal(boot_felm$clustid, boot_fixest_fe$clustid)
#  expect_equal(boot_fixest_fe$clustid, boot_felm_fe$clustid)  
# })
# # 
# # 
# 
# # test_that("test handling of missing values in cluster variables",{
# # #   # Step 1: create data
# #  voters[1, group_id1 := NA]
# #  voters[1, group_id2 := NA]
# #  
# #  # -------------------------------------------------------------------------------------------------------------------------- # 
# #  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)
# #  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, data = voters)
# #  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, data = voters)
# #  
# #  expect_warning(boottest(object = lm_fit, clustid = "group_id1", B = 1000, seed = 911, param = "treatment", conf_int = FALSE))
# #  expect_warning(boottest(object = feols_fit, clustid = c("group_id1"), B = 1000, seed = 911, param = "treatment", conf_int = FALSE, beta = 0))
# #  expect_warning(boottest(object = felm_fit, clustid =  "group_id1", B = 1000, seed = 911, param = "treatment", conf_int = FALSE))
# #  
# #  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | group_id1, data = voters)
# #  expect_warning(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 1000, seed = 911, param = "treatment", conf_int = FALSE))
# #  
# #  # it is not allowed to delete a cluster variable specified in felm from boottest()
# #  expect_error(boottest(object = felm_fit, clustid =  c("group_id2"), B = 1000, seed = 911, param = "treatment", conf_int = FALSE))
# # 
# # })
# # 
# # 
# # 
# # test_that("check if argument checks via dreamerr work properly", {
# #  
# #   voters[1, group_id1 := NA]
# #   voters[1, group_id2 := NA]
# #   
# #   # -------------------------------------------------------------------------------------------------------------------------- # 
# #   
# #   
# #   
# # })
# # 
# # 
# # 
# test_that("Different seeds lead to similar results", {
#    
#  # B needs to be large  
#  # -------------------------------------------------------------------------------------------------------------------------- # 
#  # Test 1: oneway clustering
#  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  
#  boot_lm1 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 1, param = "treatment", conf_int = FALSE)
#  boot_lm2 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 2, param = "treatment", conf_int = FALSE)
#  boot_lm3 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 3, param = "treatment", conf_int = FALSE)
#  
#  boot_fixest1 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 4, param = "treatment", conf_int = FALSE, beta = 0)
#  boot_fixest2 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 5, param = "treatment", conf_int = FALSE, beta = 0)
#  boot_fixest3 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 6, param = "treatment", conf_int = FALSE, beta = 0)
#  
#  boot_felm1 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 7, param = "treatment", conf_int = FALSE)
#  boot_felm2 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 8, param = "treatment", conf_int = FALSE)
#  boot_felm3 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 9, param = "treatment", conf_int = FALSE)
#  
#  expect_equal(boot_lm1$p_val, boot_lm2$p_val, tol = 1e-2 / 2)
#  expect_equal(boot_lm2$p_val, boot_lm3$p_val, tol = 1e-2/ 2)
#  
#  expect_equal(boot_fixest1$p_val, boot_fixest2$p_val, tol = 1e-2/ 2)
#  expect_equal(boot_fixest2$p_val, boot_fixest3$p_val, tol = 1e-2/ 2)
#  
#  expect_equal(boot_felm1$p_val, boot_felm2$p_val, tol = 1e-2/ 2)
#  expect_equal(boot_felm2$p_val, boot_felm3$p_val, tol = 1e-2/ 2)
#  
#  # ------------------------------------------------------------------------------------------ 
#  # test 2: twoway clustering 
# 
#  boot_lm1 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 1, param = "treatment", conf_int = FALSE)
#  boot_lm2 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 2, param = "treatment", conf_int = FALSE)
#  boot_lm3 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 3, param = "treatment", conf_int = FALSE)
#  
#  boot_fixest1 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 4, param = "treatment", conf_int = FALSE, beta = 0)
#  boot_fixest2 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 5, param = "treatment", conf_int = FALSE, beta = 0)
#  boot_fixest3 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 6, param = "treatment", conf_int = FALSE, beta = 0)
#  
#  boot_felm1 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 7, param = "treatment", conf_int = FALSE)
#  boot_felm2 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 8, param = "treatment", conf_int = FALSE)
#  boot_felm3 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 9, param = "treatment", conf_int = FALSE)
#  
#  expect_equal(boot_lm1$p_val, boot_lm2$p_val, tol = 1e-2 / 2)
#  expect_equal(boot_lm2$p_val, boot_lm3$p_val, tol = 1e-2/ 2)
#  
#  expect_equal(boot_fixest1$p_val, boot_fixest2$p_val, tol = 1e-2/ 2)
#  expect_equal(boot_fixest2$p_val, boot_fixest3$p_val, tol = 1e-2/ 2)
#  
#  expect_equal(boot_felm1$p_val, boot_felm2$p_val, tol = 1e-2/ 2)
#  expect_equal(boot_felm2$p_val, boot_felm3$p_val, tol = 1e-2/ 2)
#  
# })
#  
# testthat::test_that("test preprocess oneclust", {
#  
#  
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", data = voters)
#  
#  preprocess1 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = NULL, 
#                                                    seed = 1))
#  
#  preprocess2 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = "Q1_immigration", 
#                                                    seed = 1))
#  
#  expect_equal(preprocess1$data, preprocess2$data)
#  expect_equal(preprocess1$clustid, preprocess2$clustid)
#  expect_equal(preprocess1$clustid_dims, preprocess2$clustid_dims)
#  expect_equal(preprocess1$N, preprocess2$N)
#  expect_equal(preprocess1$k, preprocess2$k + 10)
#  #expect_equal(preprocess1$Y, preprocess2$Y)
#  #expect_equal(preprocess1$X, preprocess2$X)
#  expect_equal(preprocess1$N_G, preprocess2$N_G)
#  expect_equal(10, preprocess2$n_fe)
#  expect_equal(preprocess1$seed, preprocess2$seed)
#  
#  
#  # lm, feols, felm without fe
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = NULL, 
#                                                          seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                      param = "treatment",
#                                                      clustid = c("group_id1"),
#                                                      beta0 = 0,
#                                                      alpha = 0.05, 
#                                                      fe = NULL, 
#                                                      seed = 1))
#  preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
#                                                  param = "treatment",
#                                                  clustid = c("group_id1"),
#                                                  beta0 = 0,
#                                                  alpha = 0.05, 
#                                                  seed = 1))
#  
#  expect_equal(preprocess_lm$fixed_effect, preprocess_felm$fixed_effect)
#  expect_equal(preprocess_lm$fixed_effect, preprocess_fixest$fixed_effect)
#  
#  # data is not used anywhere - delete later
#  #expect_equal(preprocess_lm$data, preprocess_felm$data)
#  #expect_equal(preprocess_lm$data, preprocess_fixest$data)
#  
#  expect_equal(preprocess_lm$clustid, preprocess_felm$clustid)
#  expect_equal(preprocess_lm$clustid, preprocess_fixest$clustid)
#  
#  expect_equal(preprocess_lm$clustid_dims, preprocess_felm$clustid_dims)
#  expect_equal(preprocess_lm$clustid_dims, preprocess_fixest$clustid_dims)
#  
#  expect_equal(preprocess_lm$N, preprocess_felm$N)
#  expect_equal(preprocess_lm$N, preprocess_fixest$N)
# 
#  expect_equal(preprocess_lm$k, preprocess_felm$k)
#  expect_equal(preprocess_lm$k, preprocess_fixest$k)
#  
#  expect_equal(preprocess_lm$N_G, preprocess_felm$N_G)
#  expect_equal(preprocess_lm$N_G, preprocess_fixest$N_G)
#  
#  expect_equal(preprocess_lm$n_fe, preprocess_felm$n_fe)
#  expect_equal(preprocess_lm$n_fe, preprocess_fixest$n_fe)
#  
#  expect_equal(preprocess_lm$W, preprocess_felm$W)
#  expect_equal(preprocess_lm$W, preprocess_fixest$W)
#  
#  expect_equal(preprocess_lm$seed, preprocess_felm$seed)
#  expect_equal(preprocess_lm$seed, preprocess_fixest$seed)
#  
#  expect_equal(preprocess_lm$X, preprocess_felm$X)
#  expect_equal(preprocess_lm$X, preprocess_fixest$X)
#  
#  expect_equal(preprocess_lm$Y, preprocess_felm$Y)
#  expect_equal(preprocess_lm$Y, preprocess_fixest$Y)
#  
#  
#  # felm with fe on = lfe with fe on in estimation and boottest 
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income| Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0, data = voters)
#  
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = "Q1_immigration", 
#                                                          seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                      param = "treatment",
#                                                      clustid = c("group_id1"),
#                                                      beta0 = 0,
#                                                      alpha = 0.05, 
#                                                      fe = "Q1_immigration", 
#                                                      seed = 1))
#  names(preprocess_fixest$fixed_effect) <- "fe"
#  names(preprocess_felm$fixed_effect) <- "fe"
#    
#  expect_equal(preprocess_felm$fixed_effect, preprocess_fixest$fixed_effect)
#  #expect_equal(preprocess_felm$data, preprocess_fixest$data)
#  expect_equal(preprocess_felm$param, preprocess_fixest$param)
#  expect_equal(preprocess_felm$clustid, preprocess_fixest$clustid)
#  expect_equal(preprocess_felm$clustid_dims, preprocess_fixest$clustid_dims)
#  expect_equal(preprocess_felm$N, preprocess_fixest$N)
#  expect_equal(preprocess_felm$k, preprocess_fixest$k)
#  expect_equal(preprocess_felm$Y, preprocess_fixest$Y)
#  expect_equal(preprocess_felm$X, preprocess_fixest$X)
#  expect_equal(preprocess_felm$beta0, preprocess_fixest$beta0)
#  expect_equal(preprocess_felm$R0, preprocess_fixest$R0)
#  expect_equal(preprocess_felm$N_G, preprocess_fixest$N_G)
#  expect_equal(preprocess_felm$alpha, preprocess_fixest$alpha)
#  expect_equal(preprocess_felm$n_fe, preprocess_fixest$n_fe) # error here!
#  expect_equal(preprocess_felm$W, preprocess_fixest$W)
#  expect_equal(preprocess_felm$seed, preprocess_fixest$seed)
#  expect_equal(sort(names(preprocess_felm)), sort(names(preprocess_fixest)))
#  
#  expect_is(preprocess_felm, "oneclust")
#  expect_is(preprocess_fixest, "oneclust")
#  
# })
#  
# testthat::test_that("test preprocess multclust", {
#  
# 
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", data = voters)
# 
#  preprocess1 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                            param = "treatment",
#                                            clustid = c("group_id1", "group_id2"),
#                                            beta0 = 0,
#                                            alpha = 0.05, 
#                                            fe = NULL, 
#                                            seed = 1))
#  
#  preprocess2 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                             param = "treatment",
#                                             clustid = c("group_id1", "group_id2"),
#                                             beta0 = 0,
#                                             alpha = 0.05, 
#                                             fe = "Q1_immigration", 
#                                             seed = 1))
# 
#  expect_equal(preprocess1$data, preprocess2$data)
#  expect_equal(preprocess1$clustid, preprocess2$clustid)
#  expect_equal(preprocess1$clustid_dims, preprocess2$clustid_dims)
#  expect_equal(preprocess1$N, preprocess2$N)
#  expect_equal(preprocess1$k, preprocess2$k + 10)
#  #expect_equal(preprocess1$Y, preprocess2$Y)
#  #expect_equal(preprocess1$X, preprocess2$X)
#  expect_equal(preprocess1$N_G, preprocess2$N_G)
#  expect_equal(10, preprocess2$n_fe)
#  expect_equal(preprocess1$seed, preprocess2$seed)
#  
#  
#  # lm, feols, felm without fe
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1", "group_id2"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = NULL, 
#                                                    seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1", "group_id2"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = NULL, 
#                                                          seed = 1))
#  preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1", "group_id2"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          seed = 1))
#  
#  expect_equal(preprocess_lm$fixed_effect, preprocess_felm$fixed_effect)
#  expect_equal(preprocess_lm$fixed_effect, preprocess_fixest$fixed_effect)
#  
#  # data is not used anywhere - delete later
#  #expect_equal(preprocess_lm$data, preprocess_felm$data)
#  #expect_equal(preprocess_lm$data, preprocess_fixest$data)
#  
#  expect_equal(preprocess_lm$clustid, preprocess_felm$clustid)
#  expect_equal(preprocess_lm$clustid, preprocess_fixest$clustid)
#  
#  expect_equal(preprocess_lm$clustid_dims, preprocess_felm$clustid_dims)
#  expect_equal(preprocess_lm$clustid_dims, preprocess_fixest$clustid_dims)
#  
#  expect_equal(preprocess_lm$N, preprocess_felm$N)
#  expect_equal(preprocess_lm$N, preprocess_fixest$N)
#  
#  expect_equal(preprocess_lm$k, preprocess_felm$k)
#  expect_equal(preprocess_lm$k, preprocess_fixest$k)
#  
#  expect_equal(preprocess_lm$N_G, preprocess_felm$N_G)
#  expect_equal(preprocess_lm$N_G, preprocess_fixest$N_G)
#  
#  expect_equal(preprocess_lm$n_fe, preprocess_felm$n_fe)
#  expect_equal(preprocess_lm$n_fe, preprocess_fixest$n_fe)
#  
#  expect_equal(preprocess_lm$W, preprocess_felm$W)
#  expect_equal(preprocess_lm$W, preprocess_fixest$W)
#  
#  expect_equal(preprocess_lm$seed, preprocess_felm$seed)
#  expect_equal(preprocess_lm$seed, preprocess_fixest$seed)
#  
#  expect_equal(preprocess_lm$X, preprocess_felm$X)
#  expect_equal(preprocess_lm$X, preprocess_fixest$X)
#  
#  expect_equal(preprocess_lm$Y, preprocess_felm$Y)
#  expect_equal(preprocess_lm$Y, preprocess_fixest$Y)
#  
#  # --------------------------------------------------------------------------------------------- # 
#  # felm with fe on = lfe with fe on in estimation and boottest 
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income| Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0, data = voters)
#  
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1", "group_id2"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = "Q1_immigration", 
#                                                          seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                      param = "treatment",
#                                                      clustid = c("group_id1", "group_id2"),
#                                                      beta0 = 0,
#                                                      alpha = 0.05, 
#                                                      fe = "Q1_immigration", 
#                                                      seed = 1))
#  
#  names(preprocess_felm$fixed_effect) <- "fe"
#  names(preprocess_fixest$fixed_effect) <- "fe"
#  
#  expect_equal((preprocess_felm$fixed_effect), (preprocess_fixest$fixed_effect))
#  #expect_equal(preprocess_felm$data, preprocess_fixest$data)
#  expect_equal(preprocess_felm$param, preprocess_fixest$param)
#  expect_equal(preprocess_felm$clustid, preprocess_fixest$clustid)
#  expect_equal(preprocess_felm$clustid_dims, preprocess_fixest$clustid_dims)
#  expect_equal(preprocess_felm$N, preprocess_fixest$N)
#  expect_equal(preprocess_felm$k, preprocess_fixest$k)
#  expect_equal(preprocess_felm$Y, preprocess_fixest$Y)
#  expect_equal(preprocess_felm$X, preprocess_fixest$X)
#  expect_equal(preprocess_felm$beta0, preprocess_fixest$beta0)
#  expect_equal(preprocess_felm$R0, preprocess_fixest$R0)
#  expect_equal(preprocess_felm$N_G, preprocess_fixest$N_G)
#  expect_equal(preprocess_felm$alpha, preprocess_fixest$alpha)
#  expect_equal(preprocess_felm$n_fe, preprocess_fixest$n_fe) # error here!
#  expect_equal(preprocess_felm$W, preprocess_fixest$W)
#  expect_equal(preprocess_felm$seed, preprocess_fixest$seed)
#  expect_equal(sort(names(preprocess_felm)), sort(names(preprocess_fixest)))
#  
#  expect_is(preprocess_felm, "multclust")
#  expect_is(preprocess_fixest, "multclust")
#    
# })
# 
# 
# test_that("output of boot_algo, oneclust", {
#  
#  # 1) compare feols with fe and without fe
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", data = voters)
#  preprocess1 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = NULL, 
#                                                    seed = 1))
#  preprocess2 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = "Q1_immigration", 
#                                                    seed = 1))
#  res1 <- suppressWarnings(boot_algo.multclust(preprocess1, B = 1000))
#  res2 <- suppressWarnings(boot_algo.multclust(preprocess2, B = 1000))
#  expect_equal(res1$p_val, res2$p_val)
#  expect_equal(res1$v, res2$v)
#  expect_equal(res1$B, res2$B)
#  expect_equal(res1$clustid, res2$clustid)
#  
#  # 2) compare felm and lm and feols without fe 
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)
#  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = NULL, 
#                                                          seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                      param = "treatment",
#                                                      clustid = c("group_id1"),
#                                                      beta0 = 0,
#                                                      alpha = 0.05, 
#                                                      fe = NULL, 
#                                                      seed = 1))
#  preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
#                                                  param = "treatment",
#                                                  clustid = c("group_id1"),
#                                                  beta0 = 0,
#                                                  alpha = 0.05, 
#                                                  seed = 1))
#  res_fixest <- suppressWarnings(boot_algo.multclust(preprocess_fixest, B = 1000))
#  res_felm <- suppressWarnings(boot_algo.multclust(preprocess_felm, B = 1000))
#  res_lm <- suppressWarnings(boot_algo.multclust(preprocess_lm, B = 1000))
#  
#  expect_equal(res_fixest$p_val, res_felm$p_val)
#  expect_equal(res_fixest$p_val, res_lm$p_val)
#  
#  expect_equal(res_fixest$t_stat, res_felm$t_stat)
#  expect_equal(res_fixest$t_stat, res_lm$t_stat)
#  
#  expect_equal(res_fixest$t_boot, res_felm$t_boot)
#  expect_equal(res_fixest$t_boot, res_lm$t_boot)
#  
#  expect_equal(res_fixest$X, res_felm$X)
#  expect_equal(res_fixest$X, res_lm$X)
#  
#  expect_equal(res_fixest$Y, res_felm$Y)
#  expect_equal(res_fixest$Y, res_lm$Y)
#  
#  expect_equal(res_fixest$v, res_felm$v)
#  expect_equal(res_fixest$v, res_lm$v)
#  
#  expect_equal(res_fixest$invalid_t, res_felm$invalid_t)
#  expect_equal(res_fixest$invalid_t, res_lm$invalid_t)
#  
#  expect_equal(res_fixest$clustid, res_felm$clustid)
#  expect_equal(res_fixest$clustid, res_lm$clustid)
#  
#  # 2b) compare feols / felm with fe - boottest without fe with lm (same as 2, but feols and felm estimated with fe)
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0 , data = voters)
#  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = NULL, 
#                                                          seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                      param = "treatment",
#                                                      clustid = c("group_id1"),
#                                                      beta0 = 0,
#                                                      alpha = 0.05, 
#                                                      fe = NULL, 
#                                                      seed = 1))
#  preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
#                                                  param = "treatment",
#                                                  clustid = c("group_id1"),
#                                                  beta0 = 0,
#                                                  alpha = 0.05, 
#                                                  seed = 1))
#  res_fixest <- suppressWarnings(boot_algo.multclust(preprocess_fixest, B = 1000))
#  res_felm <- suppressWarnings(boot_algo.multclust(preprocess_felm, B = 1000))
#  res_lm <- suppressWarnings(boot_algo.multclust(preprocess_lm, B = 1000))
#  
#  # smth wrong with felm
#  expect_equal(res_fixest$p_val, res_felm$p_val)
#  expect_equal(res_fixest$p_val, res_lm$p_val)
#  
#  expect_equal(res_fixest$t_stat, res_felm$t_stat)
#  expect_equal(res_fixest$t_stat, res_lm$t_stat)
#  
#  expect_equal(res_fixest$t_boot, res_felm$t_boot)
#  expect_equal(res_fixest$t_boot, res_lm$t_boot)
#  
#  # error is here - dummy is missing in felm
#  expect_equal(res_fixest$X, res_felm$X)
#  expect_equal(res_fixest$X, res_lm$X)
#  
#  expect_equal(res_fixest$Y, res_felm$Y)
#  expect_equal(res_fixest$Y, res_lm$Y)
#  
#  expect_equal(res_fixest$v, res_felm$v)
#  expect_equal(res_fixest$v, res_lm$v)
#  
#  expect_equal(res_fixest$invalid_t, res_felm$invalid_t)
#  expect_equal(res_fixest$invalid_t, res_lm$invalid_t)
#  
#  expect_equal(res_fixest$clustid, res_felm$clustid)
#  expect_equal(res_fixest$clustid, res_lm$clustid)
#  
#  # 3) compare felm with fe and feols with fe
#  
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0 , data = voters)
#  
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = "Q1_immigration", 
#                                                          seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                      param = "treatment",
#                                                      clustid = c("group_id1"),
#                                                      beta0 = 0,
#                                                      alpha = 0.05, 
#                                                      fe = "Q1_immigration", 
#                                                      seed = 1))
#  
#  
#  res_fixest <- suppressWarnings(boot_algo.multclust(preprocess_fixest, B = 1000))
#  res_felm <- suppressWarnings(boot_algo.multclust(preprocess_felm, B = 1000))
#  
#  expect_equal(res_fixest$p_val, res_felm$p_val, tol = 1e-3)
#  expect_equal(res_fixest$v, res_felm$v)
#  expect_equal(res_fixest$B, res_felm$B)
#  expect_equal(res_fixest$clustid, res_felm$clustid)
#  expect_equal(res_fixest$X, res_felm$X)
#  expect_equal(res_fixest$Y, res_felm$Y)
#  expect_equal(res_fixest$Xr, res_felm$Xr)
#  expect_equal(res_fixest$invXX, res_felm$invXX)
#  expect_equal(res_fixest$XinvXXr, res_felm$XinvXXr)
#  
#  
#  
# })
# 
# 
# test_that("output of boot_algo, multclust", {
# 
#  # 1) compare feols with fe and without fe
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", data = voters)
#  preprocess1 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1", "group_id2"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = NULL, 
#                                                    seed = 1))
#  preprocess2 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1", "group_id2"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = "Q1_immigration", 
#                                                    seed = 1))
#  res1 <- suppressWarnings(boot_algo.multclust(preprocess1, B = 1000))
#  res2 <- suppressWarnings(boot_algo.multclust(preprocess2, B = 1000))
#  expect_equal(res1$p_val, res2$p_val, tol = 1e-2 / 2)
#  expect_equal(res1$v, res2$v)
#  expect_equal(res1$B, res2$B)
#  expect_equal(res1$clustid, res2$clustid)
#  
#  # 2) compare felm and lm and feols without fe 
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)
#  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1", "group_id2"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = NULL, 
#                                                          seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                      param = "treatment",
#                                                      clustid = c("group_id1", "group_id2"),
#                                                      beta0 = 0,
#                                                      alpha = 0.05, 
#                                                      fe = NULL, 
#                                                      seed = 1))
#  preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1", "group_id2"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          seed = 1))
#  res_fixest <- suppressWarnings(boot_algo.multclust(preprocess_fixest, B = 1000))
#  res_felm <- suppressWarnings(boot_algo.multclust(preprocess_felm, B = 1000))
#  res_lm <- suppressWarnings(boot_algo.multclust(preprocess_lm, B = 1000))
#  
#  expect_equal(res_fixest$p_val, res_felm$p_val)
#  expect_equal(res_fixest$p_val, res_lm$p_val)
#  
#  expect_equal(res_fixest$t_stat, res_felm$t_stat)
#  expect_equal(res_fixest$t_stat, res_lm$t_stat)
#  
#  expect_equal(res_fixest$t_boot, res_felm$t_boot)
#  expect_equal(res_fixest$t_boot, res_lm$t_boot)
#  
#  expect_equal(res_fixest$X, res_felm$X)
#  expect_equal(res_fixest$X, res_lm$X)
#  
#  expect_equal(res_fixest$Y, res_felm$Y)
#  expect_equal(res_fixest$Y, res_lm$Y)
#  
#  expect_equal(res_fixest$v, res_felm$v)
#  expect_equal(res_fixest$v, res_lm$v)
#  
#  expect_equal(res_fixest$invalid_t, res_felm$invalid_t)
#  expect_equal(res_fixest$invalid_t, res_lm$invalid_t)
#  
#  expect_equal(res_fixest$clustid, res_felm$clustid)
#  expect_equal(res_fixest$clustid, res_lm$clustid)
#  
#  # 2b) compare feols / felm with fe - boottest without fe with lm (same as 2, but feols and felm estimated with fe)
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0 , data = voters)
#  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                          param = "treatment",
#                                                          clustid = c("group_id1", "group_id2"),
#                                                          beta0 = 0,
#                                                          alpha = 0.05, 
#                                                          fe = NULL, 
#                                                          seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                      param = "treatment",
#                                                      clustid = c("group_id1", "group_id2"),
#                                                      beta0 = 0,
#                                                      alpha = 0.05, 
#                                                      fe = NULL, 
#                                                      seed = 1))
#  preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
#                                                  param = "treatment",
#                                                  clustid = c("group_id1", "group_id2"),
#                                                  beta0 = 0,
#                                                  alpha = 0.05, 
#                                                  seed = 1))
#  res_fixest <- suppressWarnings(boot_algo.multclust(preprocess_fixest, B = 1000))
#  res_felm <- suppressWarnings(boot_algo.multclust(preprocess_felm, B = 1000))
#  res_lm <- suppressWarnings(boot_algo.multclust(preprocess_lm, B = 1000))
#  
#  # smth wrong with felm
#  expect_equal(res_fixest$p_val, res_felm$p_val)
#  expect_equal(res_fixest$p_val, res_lm$p_val)
#  
#  expect_equal(res_fixest$t_stat, res_felm$t_stat)
#  expect_equal(res_fixest$t_stat, res_lm$t_stat)
#  
#  expect_equal(res_fixest$t_boot, res_felm$t_boot)
#  expect_equal(res_fixest$t_boot, res_lm$t_boot)
#  
#  # error is here - dummy is missing in felm
#  expect_equal(res_fixest$X, res_felm$X)
#  expect_equal(res_fixest$X, res_lm$X)
#  
#  expect_equal(res_fixest$Y, res_felm$Y)
#  expect_equal(res_fixest$Y, res_lm$Y)
#  
#  expect_equal(res_fixest$v, res_felm$v)
#  expect_equal(res_fixest$v, res_lm$v)
#  
#  expect_equal(res_fixest$invalid_t, res_felm$invalid_t)
#  expect_equal(res_fixest$invalid_t, res_lm$invalid_t)
#  
#  expect_equal(res_fixest$clustid, res_felm$clustid)
#  expect_equal(res_fixest$clustid, res_lm$clustid)
#  
#  # 3) compare felm with fe and feols with fe
#  
#  feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", data = voters)
#  felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0 , data = voters)
#  
#  preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1", "group_id2"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = "Q1_immigration", 
#                                                    seed = 1))
#  preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                    param = "treatment",
#                                                    clustid = c("group_id1", "group_id2"),
#                                                    beta0 = 0,
#                                                    alpha = 0.05, 
#                                                    fe = "Q1_immigration", 
#                                                    seed = 1))
#  
#  
#  res_fixest <- suppressWarnings(boot_algo.multclust(preprocess_fixest, B = 1000))
#  res_felm <- suppressWarnings(boot_algo.multclust(preprocess_felm, B = 1000))
#  
#  names(preprocess_felm$fixed_effect) <- "fe"
#  names(preprocess_fixest$fixed_effect) <- "fe"
#  
#  expect_equal(preprocess_felm$fixed_effect, preprocess_fixest$fixed_effect)
#  
#  expect_equal(res_fixest$p_val, res_felm$p_val, tol = 1e-3)
#  expect_equal(res_fixest$v, res_felm$v)
#  expect_equal(res_fixest$B, res_felm$B)
#  expect_equal(res_fixest$clustid, res_felm$clustid)
#  expect_equal(res_fixest$X, res_felm$X)
#  expect_equal(res_fixest$Y, res_felm$Y)
#  expect_equal(res_fixest$Xr, res_felm$Xr)
#  expect_equal(res_fixest$invXX, res_felm$invXX)
#  expect_equal(res_fixest$XinvXXr, res_felm$XinvXXr)
# 
# })
# 
# 
# # test_that("compare boot_algo and boot_algo2, multclust", {
# #  
# #    # 1) 
# #    voters <- fwildclusterboot::create_data_2(N = 100000, N_G1 = 10, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345)
# #    feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
# #    felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 , data = voters)
# #    lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
# #    
# #    preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
# #                                                            param = "treatment",
# #                                                            clustid = c("group_id1", "group_id2"),
# #                                                            beta0 = 0,
# #                                                            alpha = 0.05, 
# #                                                            fe = NULL, 
# #                                                            seed = 1))
# #    preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
# #                                                        param = "treatment",
# #                                                        clustid = c("group_id1", "group_id2"),
# #                                                        beta0 = 0,
# #                                                        alpha = 0.05, 
# #                                                        fe = NULL, 
# #                                                        seed = 1))
# #    preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
# #                                                    param = "treatment",
# #                                                    clustid = c("group_id1", "group_id2"),
# #                                                    beta0 = 0,
# #                                                    alpha = 0.05, 
# #                                                    seed = 1))
# #  
# #    res_fixest <- boot_algo.multclust(preprocess_fixest, B = 1000)
# #    res_felm <- boot_algo.multclust(preprocess_felm, B = 1000)
# #    res_lm <- boot_algo.multclust(preprocess_lm, B = 1000)
# #    
# #    res_fixest2 <- boot_algo2.multclust(preprocess_fixest, boot_iter = 100000)
# #    res_felm2 <- boot_algo2.multclust(preprocess_felm, boot_iter = 100000)
# #    res_lm2 <- boot_algo2.multclust(preprocess_lm, boot_iter = 100000)
# #    
# #    # check that output contains the same objects - _2 contains pre-computed "ABCD"
# #    expect_equal(sort(names(res_fixest)), sort(names(res_fixest2)[-which(names(res_fixest2) == "ABCD")]))
# #    expect_equal(sort(names(res_felm)), sort(names(res_felm2)[-which(names(res_fixest2) == "ABCD")]))
# #    expect_equal(sort(names(res_lm)), sort(names(res_lm)[-which(names(res_fixest2) == "ABCD")]))
# #  
# #    # 
# #    #lapply(names(res_felm), function(x) expect_equal(res_fixest[[x]], res_fixest2[[x]]))
# #    expect_equal(res_fixest[["p_val"]], res_fixest2[["p_val"]])
# #    expect_equal(res_felm[["p_val"]], res_felm2[["p_val"]])
# #    expect_equal(res_fixest[["res_lm"]], res_fixest2[["res_lm2"]])
# #    
# #    expect_equal(res_fixest[["t_stat"]], res_fixest2[["t_stat"]])
# #    expect_equal(res_felm[["t_stat"]], res_felm2[["t_stat"]])
# #    expect_equal(res_fixest[["t_stat"]], res_fixest2[["t_stat"]])
# #    
# #    # ... test all outputs
# #    
# #    
# #  })
#  
#  
#  
#  
# # test_that("compare invert_p_val & invert_p_val2, multclust", {
# #    
# #    voters <- fwildclusterboot::create_data_2(N = 10000, N_G1 = 10, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345)
# #    feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
# #    felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 , data = voters)
# #    lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, data = voters)
# #    
# #    preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
# #                                                            param = "treatment",
# #                                                            clustid = c("group_id1", "group_id2"),
# #                                                            beta0 = 0,
# #                                                            alpha = 0.05, 
# #                                                            fe = NULL, 
# #                                                            seed = 1))
# #    preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
# #                                                        param = "treatment",
# #                                                        clustid = c("group_id1", "group_id2"),
# #                                                        beta0 = 0,
# #                                                        alpha = 0.05, 
# #                                                        fe = NULL, 
# #                                                        seed = 1))
# #    preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
# #                                                    param = "treatment",
# #                                                    clustid = c("group_id1", "group_id2"),
# #                                                    beta0 = 0,
# #                                                    alpha = 0.05, 
# #                                                    seed = 1))
# #    
# #    B <- 10000
# #    res_fixest <- boot_algo.multclust(preprocess_fixest, B = B)
# #    res_felm <- boot_algo.multclust(preprocess_felm, B = B)
# #    res_lm <- boot_algo.multclust(preprocess_lm, B = B)
# #    
# #    res_fixest2 <- boot_algo2.multclust(preprocess_fixest, boot_iter = B)
# #    res_felm2 <- boot_algo2.multclust(preprocess_felm, boot_iter = B)
# #    res_lm2 <- boot_algo2.multclust(preprocess_lm, boot_iter = B)
# #    
# #    # -> now test if the same confidence sets can be generated
# #    point_estimate <- feols_fit$coefficients["treatment"]
# #    se_guess <- feols_fit$se["treatment"]
# #    
# #    res_p_val_fixest <- invert_p_val.algo_multclust(object = res_fixest,
# #                              point_estimate = point_estimate,
# #                              se_guess = se_guess, 
# #                              clustid = preprocess_fixest$clustid,
# #                              fixed_effect = preprocess_fixest$fixed_effect, 
# #                              X = preprocess_fixest$X,
# #                              Y = preprocess_fixest$Y,
# #                              N = preprocess_fixest$N,
# #                              k = preprocess_fixest$k,
# #                              v = res_fixest$v,
# #                              param = "treatment",
# #                              R0 = preprocess_fixest$R0,
# #                              B = B,
# #                              beta0 = preprocess_fixest$beta0,
# #                              alpha = preprocess_fixest$alpha, 
# #                              W = preprocess_fixest$W, 
# #                              n_fe = preprocess_fixest$n_fe, 
# #                              N_G = preprocess_fixest$N_G)
# #    
# #    #rm(list= ls()[!(ls() %in% c('res_fixest2','point_estimate', "se_guess", "preprocess_fixest", "B"))])
# #    res_p_val_fixest2 <- invert_p_val2.algo_multclust(object = res_fixest2,
# #                                                      point_estimate = point_estimate,
# #                                                      se_guess = se_guess, 
# #                                                      clustid = preprocess_fixest$clustid, 
# #                                                      alpha = preprocess_fixest$alpha, 
# #                                                      B = B)
# #    
# #    point_estimate <- felm_fit$coefficients["treatment", ]
# #   se_guess <- felm_fit$se["treatment"]
# #    
# #    res_p_val_felm <- invert_p_val.algo_multclust(object = res_felm,
# #                                     point_estimate = point_estimate,
# #                                     se_guess = se_guess, 
# #                                     clustid = preprocess_felm$clustid,
# #                                     fixed_effect = preprocess_felm$fixed_effect, 
# #                                     X = preprocess_felm$X,
# #                                     Y = preprocess_felm$Y,
# #                                     N = preprocess_felm$N,
# #                                     k = preprocess_felm$k,
# #                                     v = res_felm$v,
# #                                     param = "treatment",
# #                                     R0 = preprocess_felm$R0,
# #                                   B = B,
# #                                     beta0 = preprocess_felm$beta0,
# #                                     alpha = preprocess_felm$alpha, 
# #                                     W = preprocess_felm$W, 
# #                                     n_fe = preprocess_felm$n_fe, 
# #                                     N_G = preprocess_felm$N_G)
# #    res_p_val_felm2 <- invert_p_val2.algo_multclust(object = res_felm2,
# #                                                      point_estimate = point_estimate,
# #                                                      se_guess = se_guess, 
# #                                                      clustid = preprocess_fixest$clustid, 
# #                                                      alpha = preprocess_fixest$alpha, 
# #                                                      B = B)
# #    
# #    point_estimate <- lm_fit$coefficients["treatment"]
# #    clustid_fml <- as.formula(paste("~", paste(c("group_id1", "group_id2"), collapse = "+")))
# #    
# #    vcov <- suppressWarnings(sandwich::vcovCL(lm_fit, cluster =  clustid_fml))
# #    coefs <- suppressWarnings(lmtest::coeftest(lm_fit, vcov))
# #    se_guess <- coefs["treatment", "Std. Error"]
# #    res_p_val_lm <- invert_p_val.algo_multclust(object = res_lm,
# #                                     point_estimate = point_estimate,
# #                                     se_guess = se_guess, 
# #                                     clustid = preprocess_lm$clustid,
# #                                     fixed_effect = preprocess_lm$fixed_effect, 
# #                                     X = preprocess_lm$X,
# #                                     Y = preprocess_lm$Y,
# #                                     N = preprocess_lm$N,
# #                                     k = preprocess_lm$k,
# #                                     v = res_lm$v,
# #                                     param = param,
# #                                     R0 = preprocess_lm$R0,
# #                                     B = B,
# #                                     beta0 = preprocess_lm$beta0,
# #                                     alpha = preprocess_lm$alpha, 
# #                                     W = preprocess_lm$W, 
# #                                     n_fe = preprocess_lm$n_fe, 
# #                                     N_G = preprocess_lm$N_G)
# #    res_p_val_lm2 <- invert_p_val2.algo_multclust(object = res_lm2,
# #                                                    point_estimate = point_estimate,
# #                                                    se_guess = se_guess, 
# #                                                    clustid = preprocess_fixest$clustid, 
# #                                                    alpha = preprocess_fixest$alpha, 
# #                                                    B = B)
# #    
# #    expect_equal(res_p_val_fixest, res_p_val_fixest2)
# #    expect_equal(res_p_val_felm, res_p_val_felm2)
# #    expect_equal(res_p_val_lm, res_p_val_lm2)
# #    
# #    
# #  })
