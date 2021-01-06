# # Test 1x: compare invert_p_val & invert_p_val2, oneclust
# 
# 
# 
# feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 10, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345))
# #felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 , weights = NULL, data = create_data_2(N = 1000, N_G1 = 10, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345))
# lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 10, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345))
# 
# preprocess_fixest <- suppressWarnings(fwildclusterboot::preprocess.fixest(object = feols_fit, 
#                                                                           param = "treatment",
#                                                                           clustid = c("group_id1"),
#                                                                           beta0 = 0,
#                                                                           alpha = 0.05, 
#                                                                           fe = NULL, 
#                                                                           seed = 1))
# # preprocess_felm <- suppressWarnings(fwildclusterboot::preprocess.felm(object = felm_fit, 
# #                                                     param = "treatment",
# #                                                     clustid = c("group_id1"),
# #                                                     beta0 = 0,
# #                                                     alpha = 0.05, 
# #                                                     fe = NULL, 
# #                                                     seed = 1))
# preprocess_lm <- suppressWarnings(fwildclusterboot::preprocess.lm(object = lm_fit, 
#                                                                   param = "treatment",
#                                                                   clustid = c("group_id1"),
#                                                                   beta0 = 0,
#                                                                   alpha = 0.05, 
#                                                                   seed = 1))
# 
# B <- 1000
# res_fixest <- suppressWarnings(fwildclusterboot::boot_algo.oneclust(preprocess_fixest, B = B,
#                                                                      wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
# #res_felm <- fwildclusterboot::boot_algo.oneclust(preprocess_felm, B = B)
# res_lm <- suppressWarnings(fwildclusterboot::boot_algo.oneclust(preprocess_lm, B = B, 
#                                                                  wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
# 
# res_fixest2 <- suppressWarnings(fwildclusterboot::boot_algo2.oneclust(preprocess_fixest, boot_iter = B,
#                                                                        wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
# #res_felm2 <- fwildclusterboot::boot_algo2.oneclust(preprocess_felm, boot_iter = B)
# res_lm2 <- suppressWarnings(fwildclusterboot::boot_algo2.oneclust(preprocess_lm, boot_iter = B,
#                                                                    wild_draw_fun = function(n){sample(c(-1,1), n, replace = TRUE)}))
# 
# # -> now test if the same confidence sets can be generated
# point_estimate <- feols_fit$coefficients["treatment"]
# se_guess <- feols_fit$se["treatment"]
# 
# res_p_val_fixest <- fwildclusterboot::invert_p_val.algo_oneclust(object = res_fixest,
#                                                                   point_estimate = point_estimate,
#                                                                   se_guess = se_guess, 
#                                                                   clustid = preprocess_fixest$clustid,
#                                                                   fixed_effect = preprocess_fixest$fixed_effect, 
#                                                                   X = preprocess_fixest$X,
#                                                                   Y = preprocess_fixest$Y,
#                                                                   N = preprocess_fixest$N,
#                                                                   k = preprocess_fixest$k,
#                                                                   v = res_fixest$v,
#                                                                   param = "treatment",
#                                                                   R0 = preprocess_fixest$R0,
#                                                                   B = B,
#                                                                   beta0 = preprocess_fixest$beta0,
#                                                                   alpha = preprocess_fixest$alpha, 
#                                                                   W = preprocess_fixest$W, 
#                                                                   n_fe = preprocess_fixest$n_fe, 
#                                                                   N_G = preprocess_fixest$N_G)
# 
# #rm(list= ls()[!(ls() %in% c('res_fixest2','point_estimate', "se_guess", "preprocess_fixest", "B"))])
# res_p_val_fixest2 <- fwildclusterboot::invert_p_val2.algo_oneclust(object = res_fixest2,
#                                                                     point_estimate = point_estimate,
#                                                                     se_guess = se_guess, 
#                                                                     clustid = preprocess_fixest$clustid, 
#                                                                     alpha = preprocess_fixest$alpha, 
#                                                                     B = B)
# 
# #point_estimate <- felm_fit$coefficients["treatment", ]
# # #   se_guess <- felm_fit$se["treatment"]
# 
# # res_p_val_felm <- fwildclusterboot::invert_p_val.algo_oneclust(object = res_felm,
# #                                  point_estimate = point_estimate,
# #                                  se_guess = se_guess, 
# #                                  clustid = preprocess_felm$clustid,
# #                                  fixed_effect = preprocess_felm$fixed_effect, 
# #                                  X = preprocess_felm$X,
# #                                  Y = preprocess_felm$Y,
# #                                  N = preprocess_felm$N,
# #                                  k = preprocess_felm$k,
# #                                  v = res_felm$v,
# #                                  param = "treatment",
# #                                  R0 = preprocess_felm$R0,
# #                                B = B,
# #                                  beta0 = preprocess_felm$beta0,
# #                                  alpha = preprocess_felm$alpha, 
# #                                  W = preprocess_felm$W, 
# #                                  n_fe = preprocess_felm$n_fe, 
# #                                  N_G = preprocess_felm$N_G)
# # res_p_val_felm2 <- fwildclusterboot::invert_p_val2.algo_oneclust(object = res_felm2,
# #                                                   point_estimate = point_estimate,
# #                                                   se_guess = se_guess, 
# #                                                   clustid = preprocess_fixest$clustid, 
# #                                                   alpha = preprocess_fixest$alpha, 
# #                                                   B = B)
# 
# point_estimate <- lm_fit$coefficients["treatment"]
# clustid_fml <- as.formula(paste("~", paste(c("group_id1", "group_id2"), collapse = "+")))
# 
# vcov <- suppressWarnings(sandwich::vcovCL(lm_fit, cluster =  clustid_fml))
# coefs <- suppressWarnings(lmtest::coeftest(lm_fit, vcov))
# se_guess <- coefs["treatment", "Std. Error"]
# res_p_val_lm <- fwildclusterboot::invert_p_val.algo_oneclust(object = res_lm,
#                                                               point_estimate = point_estimate,
#                                                               se_guess = se_guess, 
#                                                               clustid = preprocess_lm$clustid,
#                                                               fixed_effect = preprocess_lm$fixed_effect, 
#                                                               X = preprocess_lm$X,
#                                                               Y = preprocess_lm$Y,
#                                                               N = preprocess_lm$N,
#                                                               k = preprocess_lm$k,
#                                                               v = res_lm$v,
#                                                               param = param,
#                                                               R0 = preprocess_lm$R0,
#                                                               B = B,
#                                                               beta0 = preprocess_lm$beta0,
#                                                               alpha = preprocess_lm$alpha, 
#                                                               W = preprocess_lm$W, 
#                                                               n_fe = preprocess_lm$n_fe, 
#                                                               N_G = preprocess_lm$N_G)
# res_p_val_lm2 <- fwildclusterboot::invert_p_val2.algo_oneclust(object = res_lm2,
#                                                                 point_estimate = point_estimate,
#                                                                 se_guess = se_guess, 
#                                                                 clustid = preprocess_fixest$clustid, 
#                                                                 alpha = preprocess_fixest$alpha, 
#                                                                 B = B)
# 
# expect_equal(res_p_val_fixest$conf_int, res_p_val_fixest2$conf_int, tol = 1e-2 / 2)
# # expect_equal(res_p_val_felm, res_p_val_felm2)
# expect_equal(res_p_val_lm$conf_int, res_p_val_lm2$conf_int, tol = 1e-2 / 2)
