library(fwildclusterboot)
library(fixest)
#library(lfe)
library(data.table)
library(dreamerr)

# test output of boottest() without any fixed effects in regression objects, twoway clustering"

create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911)
 
# Test 1: no fixed effects
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, 
             data = create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911))
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, 
                           data = create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911))
#felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, 
#                      data = create_data_2(N = 10000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 911))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  c("group_id1", "group_id2"), B = 1000, seed = 911, param = "treatment", conf_int = TRUE))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 1000, seed = 911, param = "treatment", conf_int = TRUE, beta = 0))

#boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 1000, seed = 911, param = "treatment", conf_int = TRUE))
# 
expect_true(class(boot_lm) == "boottest")  
expect_true(class(boot_fixest) == "boottest")  
#expect_true(class(boot_felm) == "boottest")  
# 
# equality of p-vals 
expect_equal(boot_lm$p_val, boot_fixest$p_val, tolerance = 1e-2 / 5)
#expect_equal(boot_fixest$p_val, boot_felm$p_val, tolerance = 1e-2 / 5)

# equality of confidence sets 
expect_equal(boot_lm$conf_int, boot_fixest$conf_int, tolerance = 1e-2 / 5)
#expect_equal(boot_fixest$conf_int, boot_felm$conf_int, tolerance = 1e-2 / 5)

# equality of point estimate
expect_equal(boot_lm$point_estimate, boot_fixest$point_estimate)
#expect_equal(as.numeric(boot_fixest$point_estimate), as.numeric(boot_felm$point_estimate))

# t-stats 
expect_equal(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 1e-2 )
#expect_equal(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 1e-2)

# N, B, N_G, clustid
expect_identical(boot_lm$N, boot_fixest$N)
#expect_identical(boot_fixest$N, boot_felm$N)

# B
expect_identical(boot_lm$B, boot_fixest$B)
#expect_identical(boot_fixest$B, boot_felm$B)

# N_G
expect_identical(boot_lm$N_G, boot_fixest$N_G)
#expect_identical(boot_fixest$N_G, boot_felm$N_G)

# clustid
expect_identical(boot_lm$clustid, boot_fixest$clustid)
#expect_identical(boot_fixest$clustid, boot_felm$clustid)

 