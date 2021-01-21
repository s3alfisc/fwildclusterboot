# # test preprocessing 
# 
# feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  c("Q1_immigration", "Q2_defense"), 
#                            data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# 
# felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense, 
#                            data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# 
# lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense, 
#                 data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# 
# prep_feols <- 
# preprocess.fixest(object = feols_fit, 
#                   param = "treatment",
#                   clustid = "group_id1",
#                   beta0 = 0,
#                   alpha = 0.05, 
#                   seed = 1, 
#                   bootcluster = "min", 
#                   fe = "Q1_immigration")
# prep_felm <- 
# preprocess.felm(object = felm_fit, 
#                 param = "treatment",
#                 clustid = "group_id1",
#                 beta0 = 0,
#                 alpha = 0.05, 
#                 seed = 1, 
#                 bootcluster = "min", 
#                 fe = "Q1_immigration")
# prep_lm <- 
# preprocess.lm(object = lm_fit, 
#               param = "treatment",
#               clustid = "group_id1",
#               beta0 = 0,
#               alpha = 0.05, 
#               seed = 1, 
#               bootcluster = "min")
# 
# names(prep_feols)
# 
# expect_equal(prep_feols$param, prep_felm$param)
# expect_equal(prep_feols$clustid, prep_felm$clustid)
# expect_equal(prep_feols$clustid_dims, prep_felm$clustid_dims)
# expect_equal(prep_feols$N, prep_felm$N)
# expect_equal(prep_feols$k, prep_felm$k)
# expect_equal(prep_feols$beta0, prep_felm$beta0)
# expect_equal(prep_feols$N_G, prep_felm$N_G)
# expect_equal(prep_feols$alpha, prep_felm$alpha)
# expect_equal(prep_feols$seed, prep_felm$seed)
# expect_equal(prep_feols$bootcluster, prep_felm$bootcluster)
# expect_equal(prep_feols$vcov_sign, prep_felm$vcov_sign)
# 
# expect_equal(prep_feols$fixed_effect, prep_felm$fixed_effect)
# expect_equal(prep_feols$Y, prep_felm$Y)
# expect_equal(prep_feols$X, prep_felm$X)
# expect_equal(prep_feols$R0, prep_felm$R0)
# expect_equal(prep_feols$n_fe, prep_felm$n_fe)
# expect_equal(prep_feols$W, prep_felm$W)

