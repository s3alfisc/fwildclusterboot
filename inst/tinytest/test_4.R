# Test 4: test with one fixed effects in regression objects, oneway clustering

# -------------------------------------------------------------------------------------------------------------------------- # 
# Test 1: no fixed effects
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, 
             data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, 
                           data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

#felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, 
#                      data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))


boot_lm <-  boottest(object = lm_fit, clustid = "group_id1", B = 1000, seed = 911, param = "treatment", conf_int = FALSE)

# 
boot_fixest <- boottest(object = feols_fit, clustid = c("group_id1"), B = 1000, seed = 911, param = "treatment", conf_int = FALSE, beta = 0)
 
# 
#boot_felm <- boottest(object = felm_fit, clustid =  "group_id1", B = 1000, seed = 911, param = "treatment", conf_int = FALSE)
#
# 
boot_fixest_fe <- boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 1000, seed = 911, param = "treatment", conf_int = FALSE, beta = 0)
# 
# 
#boot_felm_fe <- boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration",B = 1000, seed = 911, param = "treatment", conf_int = FALSE)
#

expect_true(class(boot_lm) =="boottest")  
expect_true(class(boot_fixest) == "boottest")  
#expect_true(class(boot_felm) =="boottest")  
expect_true(class(boot_fixest_fe) == "boottest")  
#expect_true(class(boot_felm_fe) == "boottest")  

# p-val
expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2 / 2)
#expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2 / 2)
#expect_equal(boot_felm$p_val, boot_fixest_fe$p_val, tolerance = 1e-2 / 2)
#expect_equal(boot_fixest_fe$p_val, boot_felm_fe$p_val, tolerance = 1e-2 / 2)

# equality of confidence sets 
expect_equal(boot_lm$conf_int, boot_fixest$conf_int, tolerance = 1e-2 / 2)
#expect_equal(boot_fixest$conf_int, boot_felm$conf_int, tolerance = 1e-2 / 2)
#expect_equal(boot_felm$conf_int, boot_fixest_fe$conf_int, tolerance = 1e-2 / 2)
#expect_equal(boot_fixest_fe$conf_int, boot_felm_fe$conf_int, tolerance = 1e-2 / 2)

# equality of point estimate
expect_equal(boot_lm$point_estimate, boot_fixest$point_estimate)
#expect_equal(as.numeric(boot_fixest$point_estimate), as.numeric(boot_felm$point_estimate))
#expect_equal(as.numeric(boot_felm$point_estimate), as.numeric(boot_fixest_fe$point_estimate))
#expect_equal(as.numeric(boot_fixest_fe$point_estimate), as.numeric(boot_felm_fe$point_estimate))

# t-stats 
expect_equal(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 1e-2 )
#expect_equal(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 1e-2)
#expect_equal(boot_felm$t_stat, boot_fixest_fe$t_stat, tolerance = 1e-2)
#expect_equal(boot_fixest_fe$t_stat, boot_felm_fe$t_stat, tolerance = 1e-2)  

# N, B, N_G, clustid
expect_equal(boot_lm$N, boot_fixest$N)
# expect_equal(boot_fixest$N, boot_felm$N)
# expect_equal(boot_felm$N, boot_fixest_fe$N)
# expect_equal(boot_fixest_fe$N, boot_felm_fe$N)  

# B
expect_equal(boot_lm$B, boot_fixest$B)
# expect_equal(boot_fixest$B, boot_felm$B)
# expect_equal(boot_felm$B, boot_fixest_fe$B)
# expect_equal(boot_fixest_fe$B, boot_felm_fe$B)   

# N_G
expect_equal(as.numeric(boot_lm$N_G), as.numeric(boot_fixest$N_G))
# expect_equal(as.numeric(boot_fixest$N_G), as.numeric(boot_felm$N_G))
# expect_equal(as.numeric(boot_felm$N_G), as.numeric(boot_fixest_fe$N_G))
# expect_equal(as.numeric(boot_fixest_fe$N_G), as.numeric(boot_felm_fe$N_G))

# clustid
expect_equal(boot_lm$clustid, boot_fixest$clustid)
# expect_equal(boot_fixest$clustid, boot_felm$clustid)
# expect_equal(boot_felm$clustid, boot_fixest_fe$clustid)
# expect_equal(boot_fixest_fe$clustid, boot_felm_fe$clustid)  
# })
# # 
# # 