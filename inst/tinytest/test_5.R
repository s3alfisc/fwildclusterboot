# Test 5: Different seeds lead to similar results"
  
# B needs to be large  
# -------------------------------------------------------------------------------------------------------------------------- # 
# Test 1: oneway clustering
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm1 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 1, param = "treatment", conf_int = FALSE)
boot_lm2 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 2, param = "treatment", conf_int = FALSE)
boot_lm3 <-  boottest(object = lm_fit, clustid = "group_id1", B = 100000, seed = 3, param = "treatment", conf_int = FALSE)

boot_fixest1 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 4, param = "treatment", conf_int = FALSE, beta = 0)
boot_fixest2 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 5, param = "treatment", conf_int = FALSE, beta = 0)
boot_fixest3 <- boottest(object = feols_fit, clustid = c("group_id1"), B = 100000, seed = 6, param = "treatment", conf_int = FALSE, beta = 0)

# boot_felm1 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 7, param = "treatment", conf_int = FALSE)
# boot_felm2 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 8, param = "treatment", conf_int = FALSE)
# boot_felm3 <- boottest(object = felm_fit, clustid =  "group_id1", B = 100000, seed = 9, param = "treatment", conf_int = FALSE)

expect_equal(boot_lm1$p_val, boot_lm2$p_val, tol = 1e-2 / 2)
expect_equal(boot_lm2$p_val, boot_lm3$p_val, tol = 1e-2/ 2)

expect_equal(boot_fixest1$p_val, boot_fixest2$p_val, tol = 1e-2/ 2)
expect_equal(boot_fixest2$p_val, boot_fixest3$p_val, tol = 1e-2/ 2)

# expect_equal(boot_felm1$p_val, boot_felm2$p_val, tol = 1e-2/ 2)
# expect_equal(boot_felm2$p_val, boot_felm3$p_val, tol = 1e-2/ 2)

# ------------------------------------------------------------------------------------------ 
# test 2: twoway clustering 
# 
boot_lm1 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 1, param = "treatment", conf_int = FALSE)
boot_lm2 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 2, param = "treatment", conf_int = FALSE)
boot_lm3 <-  boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 3, param = "treatment", conf_int = FALSE)

boot_fixest1 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 4, param = "treatment", conf_int = FALSE, beta = 0)
boot_fixest2 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 5, param = "treatment", conf_int = FALSE, beta = 0)
boot_fixest3 <- boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 100000, seed = 6, param = "treatment", conf_int = FALSE, beta = 0)

# boot_felm1 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 7, param = "treatment", conf_int = FALSE)
# boot_felm2 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 8, param = "treatment", conf_int = FALSE)
# boot_felm3 <- boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 100000, seed = 9, param = "treatment", conf_int = FALSE)

expect_equal(boot_lm1$p_val, boot_lm2$p_val, tol = 1e-2 / 2)
expect_equal(boot_lm2$p_val, boot_lm3$p_val, tol = 1e-2/ 2)

expect_equal(boot_fixest1$p_val, boot_fixest2$p_val, tol = 1e-2/ 2)
expect_equal(boot_fixest2$p_val, boot_fixest3$p_val, tol = 1e-2/ 2)

# expect_equal(boot_felm1$p_val, boot_felm2$p_val, tol = 1e-2/ 2)
# expect_equal(boot_felm2$p_val, boot_felm3$p_val, tol = 1e-2/ 2)

# })

# testthat::test_that("test preprocess oneclust", {


feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

preprocess1 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
                                                  param = "treatment",
                                                  clustid = c("group_id1"),
                                                  beta0 = 0,
                                                  alpha = 0.05, 
                                                  fe = NULL, 
                                                  seed = 1))

preprocess2 <- suppressWarnings(preprocess.fixest(object = feols_fit, 
                                                  param = "treatment",
                                                  clustid = c("group_id1"),
                                                  beta0 = 0,
                                                  alpha = 0.05, 
                                                  fe = "Q1_immigration", 
                                                  seed = 1))

expect_equal(preprocess1$data, preprocess2$data)
expect_equal(preprocess1$clustid, preprocess2$clustid)
expect_equal(preprocess1$clustid_dims, preprocess2$clustid_dims)
expect_equal(preprocess1$N, preprocess2$N)
expect_equal(preprocess1$k, preprocess2$k + 10)
#expect_equal(preprocess1$Y, preprocess2$Y)
#expect_equal(preprocess1$X, preprocess2$X)
expect_equal(preprocess1$N_G, preprocess2$N_G)
expect_equal(10, preprocess2$n_fe)
expect_equal(preprocess1$seed, preprocess2$seed)


# lm, feols, felm without fe
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, 
                   data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
                                                        param = "treatment",
                                                        clustid = c("group_id1"),
                                                        beta0 = 0,
                                                        alpha = 0.05, 
                                                        fe = NULL, 
                                                        seed = 1))
# preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                     param = "treatment",
#                                                     clustid = c("group_id1"),
#                                                     beta0 = 0,
#                                                     alpha = 0.05, 
#                                                     fe = NULL, 
#                                                     seed = 1))
preprocess_lm <- suppressWarnings(preprocess.lm(object = lm_fit, 
                                                param = "treatment",
                                                clustid = c("group_id1"),
                                                beta0 = 0,
                                                alpha = 0.05, 
                                                seed = 1))

# expect_equal(preprocess_lm$fixed_effect, preprocess_felm$fixed_effect)
expect_equal(preprocess_lm$fixed_effect, preprocess_fixest$fixed_effect)

# data is not used anywhere - delete later
#expect_equal(preprocess_lm$data, preprocess_felm$data)
#expect_equal(preprocess_lm$data, preprocess_fixest$data)

# expect_equal(preprocess_lm$clustid, preprocess_felm$clustid)
expect_equal(preprocess_lm$clustid, preprocess_fixest$clustid)

# expect_equal(preprocess_lm$clustid_dims, preprocess_felm$clustid_dims)
expect_equal(preprocess_lm$clustid_dims, preprocess_fixest$clustid_dims)

# expect_equal(preprocess_lm$N, preprocess_felm$N)
expect_equal(preprocess_lm$N, preprocess_fixest$N)
# 
# expect_equal(preprocess_lm$k, preprocess_felm$k)
expect_equal(preprocess_lm$k, preprocess_fixest$k)

# expect_equal(preprocess_lm$N_G, preprocess_felm$N_G)
expect_equal(preprocess_lm$N_G, preprocess_fixest$N_G)

# expect_equal(preprocess_lm$n_fe, preprocess_felm$n_fe)
expect_equal(preprocess_lm$n_fe, preprocess_fixest$n_fe)

# expect_equal(preprocess_lm$W, preprocess_felm$W)
expect_equal(preprocess_lm$W, preprocess_fixest$W)

# expect_equal(preprocess_lm$seed, preprocess_felm$seed)
expect_equal(preprocess_lm$seed, preprocess_fixest$seed)

# expect_equal(preprocess_lm$X, preprocess_felm$X)
expect_equal(preprocess_lm$X, preprocess_fixest$X)

# expect_equal(preprocess_lm$Y, preprocess_felm$Y)
expect_equal(preprocess_lm$Y, preprocess_fixest$Y)


# felm with fe on = lfe with fe on in estimation and boottest 
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income| Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

preprocess_fixest <- suppressWarnings(preprocess.fixest(object = feols_fit, 
                                                        param = "treatment",
                                                        clustid = c("group_id1"),
                                                        beta0 = 0,
                                                        alpha = 0.05, 
                                                        fe = "Q1_immigration", 
                                                        seed = 1))
# preprocess_felm <- suppressWarnings(preprocess.felm(object = felm_fit, 
#                                                     param = "treatment",
#                                                     clustid = c("group_id1"),
#                                                     beta0 = 0,
#                                                     alpha = 0.05, 
#                                                     fe = "Q1_immigration", 
#                                                     seed = 1))
names(preprocess_fixest$fixed_effect) <- "fe"
# names(preprocess_felm$fixed_effect) <- "fe"
  
# expect_equal(preprocess_felm$fixed_effect, preprocess_fixest$fixed_effect)
# #expect_equal(preprocess_felm$data, preprocess_fixest$data)
# expect_equal(preprocess_felm$param, preprocess_fixest$param)
# expect_equal(preprocess_felm$clustid, preprocess_fixest$clustid)
# expect_equal(preprocess_felm$clustid_dims, preprocess_fixest$clustid_dims)
# expect_equal(preprocess_felm$N, preprocess_fixest$N)
# expect_equal(preprocess_felm$k, preprocess_fixest$k)
# expect_equal(preprocess_felm$Y, preprocess_fixest$Y)
# expect_equal(preprocess_felm$X, preprocess_fixest$X)
# expect_equal(preprocess_felm$beta0, preprocess_fixest$beta0)
# expect_equal(preprocess_felm$R0, preprocess_fixest$R0)
# expect_equal(preprocess_felm$N_G, preprocess_fixest$N_G)
# expect_equal(preprocess_felm$alpha, preprocess_fixest$alpha)
# expect_equal(preprocess_felm$n_fe, preprocess_fixest$n_fe) # error here!
# expect_equal(preprocess_felm$W, preprocess_fixest$W)
# expect_equal(preprocess_felm$seed, preprocess_fixest$seed)
# expect_equal(sort(names(preprocess_felm)), sort(names(preprocess_fixest)))
# 
# expect_true(class(preprocess_felm) == "oneclust")
# expect_true(class(preprocess_fixest) == "oneclust")

# })

