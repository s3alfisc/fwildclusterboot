# Test 8: output of boot_algo, multclust

# 1) compare feols with fe and without fe
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
preprocess1 <- suppressWarnings(fwildclusterboot::preprocess.fixest(object = feols_fit, 
                                                  param = "treatment",
                                                  clustid = c("group_id1", "group_id2"),
                                                  beta0 = 0,
                                                  alpha = 0.05, 
                                                  fe = NULL, 
                                                  seed = 1))
preprocess2 <- suppressWarnings(fwildclusterboot::preprocess.fixest(object = feols_fit, 
                                                  param = "treatment",
                                                  clustid = c("group_id1", "group_id2"),
                                                  beta0 = 0,
                                                  alpha = 0.05, 
                                                  fe = "Q1_immigration", 
                                                  seed = 1))
res1 <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess1, B = 1000,
                                                               wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
res2 <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess2, B = 1000,
                                                               wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
expect_equal(res1$p_val, res2$p_val, tol = 1e-2 / 2)
expect_equal(res1$v, res2$v)
expect_equal(res1$B, res2$B)
expect_equal(res1$clustid, res2$clustid)

# 2) compare felm and lm and feols without fe 
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
#felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
preprocess_fixest <- suppressWarnings(fwildclusterboot::preprocess.fixest(object = feols_fit, 
                                                        param = "treatment",
                                                        clustid = c("group_id1", "group_id2"),
                                                        beta0 = 0,
                                                        alpha = 0.05, 
                                                        fe = NULL, 
                                                        seed = 1))
# preprocess_felm <- suppressWarnings(fwildclusterboot::preprocess.felm(object = felm_fit, 
#                                                     param = "treatment",
#                                                     clustid = c("group_id1", "group_id2"),
#                                                     beta0 = 0,
#                                                     alpha = 0.05, 
#                                                     fe = NULL, 
#                                                     seed = 1))
preprocess_lm <- suppressWarnings(fwildclusterboot::preprocess.lm(object = lm_fit, 
                                                        param = "treatment",
                                                        clustid = c("group_id1", "group_id2"),
                                                        beta0 = 0,
                                                        alpha = 0.05, 
                                                        seed = 1))
res_fixest <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess_fixest, B = 1000,
                                                                     wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
# res_felm <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess_felm, B = 1000,
#                                                                    wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
res_lm <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess_lm, B = 1000,
                                                                 wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))

#expect_equal(res_fixest$p_val, res_felm$p_val)
expect_equal(res_fixest$p_val, res_lm$p_val)

#expect_equal(res_fixest$t_stat, res_felm$t_stat)
expect_equal(res_fixest$t_stat, res_lm$t_stat)

#expect_equal(res_fixest$t_boot, res_felm$t_boot)
expect_equal(res_fixest$t_boot, res_lm$t_boot)

#expect_equal(res_fixest$X, res_felm$X)
expect_equal(res_fixest$X, res_lm$X)

#expect_equal(res_fixest$Y, res_felm$Y)
expect_equal(res_fixest$Y, res_lm$Y)

#expect_equal(res_fixest$v, res_felm$v)
expect_equal(res_fixest$v, res_lm$v)

#expect_equal(res_fixest$invalid_t, res_felm$invalid_t)
expect_equal(res_fixest$invalid_t, res_lm$invalid_t)

#expect_equal(res_fixest$clustid, res_felm$clustid)
expect_equal(res_fixest$clustid, res_lm$clustid)

#2b) compare feols / felm with fe - boottest without fe with lm (same as 2, but feols and felm estimated with fe)
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
#felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0 , weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
preprocess_fixest <- suppressWarnings(fwildclusterboot::preprocess.fixest(object = feols_fit, 
                                                        param = "treatment",
                                                        clustid = c("group_id1", "group_id2"),
                                                        beta0 = 0,
                                                        alpha = 0.05, 
                                                        fe = NULL, 
                                                        seed = 1))
#preprocess_felm <- suppressWarnings(fwildclusterboot::preprocess.felm(object = felm_fit, 
                                                   # param = "treatment",
                                                   # clustid = c("group_id1", "group_id2"),
                                                   # beta0 = 0,
                                                   # alpha = 0.05, 
                                                   # fe = NULL, 
                                                   # seed = 1))
preprocess_lm <- suppressWarnings(fwildclusterboot::preprocess.lm(object = lm_fit, 
                                                param = "treatment",
                                                clustid = c("group_id1", "group_id2"),
                                                beta0 = 0,
                                                alpha = 0.05, 
                                                seed = 1))
res_fixest <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess_fixest, B = 1000,
                                                                     wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
#res_felm <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess_felm, B = 1000,
#wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
res_lm <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess_lm, B = 1000,
                                                                 wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))

# smth wrong with felm
#expect_equal(res_fixest$p_val, res_felm$p_val)
expect_equal(res_fixest$p_val, res_lm$p_val)

#expect_equal(res_fixest$t_stat, res_felm$t_stat)
expect_equal(res_fixest$t_stat, res_lm$t_stat)

#expect_equal(res_fixest$t_boot, res_felm$t_boot)
expect_equal(res_fixest$t_boot, res_lm$t_boot)

# error is here - dummy is missing in felm
#expect_equal(res_fixest$X, res_felm$X)
expect_equal(res_fixest$X, res_lm$X)

#expect_equal(res_fixest$Y, res_felm$Y)
expect_equal(res_fixest$Y, res_lm$Y)

#expect_equal(res_fixest$v, res_felm$v)
expect_equal(res_fixest$v, res_lm$v)

#expect_equal(res_fixest$invalid_t, res_felm$invalid_t)
expect_equal(res_fixest$invalid_t, res_lm$invalid_t)

#expect_equal(res_fixest$clustid, res_felm$clustid)
expect_equal(res_fixest$clustid, res_lm$clustid)

# 3) compare felm with fe and feols with fe

# feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration", weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# #felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | 0 , weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# 
# preprocess_fixest <- suppressWarnings(fwildclusterboot::preprocess.fixest(object = feols_fit, 
#                                                   param = "treatment",
#                                                   clustid = c("group_id1", "group_id2"),
#                                                   beta0 = 0,
#                                                   alpha = 0.05, 
#                                                   fe = "Q1_immigration", 
#                                                   seed = 1))
# #preprocess_felm <- suppressWarnings(fwildclusterboot::preprocess.felm(object = felm_fit, 
#                                                  param = "treatment",
#                                                  clustid = c("group_id1", "group_id2"),
#                                                  beta0 = 0,
#                                                  alpha = 0.05, 
#                                                  fe = "Q1_immigration", 
#                                                  seed = 1))
# 
# 
# res_fixest <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess_fixest, B = 1000))
# #res_felm <- suppressWarnings(fwildclusterboot::boot_algo.multclust(preprocess_felm, B = 1000))
# 
# #names(preprocess_felm$fixed_effect) <- "fe"
# #names(preprocess_fixest$fixed_effect) <- "fe"#
# 
# #expect_equal(preprocess_felm$fixed_effect, preprocess_fixest$fixed_effect)
# 
# expect_equal(res_fixest$p_val, res_felm$p_val, tol = 1e-3)
# expect_equal(res_fixest$v, res_felm$v)
# expect_equal(res_fixest$B, res_felm$B)
# expect_equal(res_fixest$clustid, res_felm$clustid)
# expect_equal(res_fixest$X, res_felm$X)
# expect_equal(res_fixest$Y, res_felm$Y)
# expect_equal(res_fixest$Xr, res_felm$Xr)
# expect_equal(res_fixest$invXX, res_felm$invXX)
# expect_equal(res_fixest$XinvXXr, res_felm$XinvXXr)
# 
