# ------------------------------------------------------------------ # 
# test 1: default behavior with weights
# - Tests .1 - one fixed effect, 
# - Tests .2 - two fixed effect
# - Part A1: no fixed effect
#            - cluster vars in boottest
#            - cluster vars in feols and fixest
#            - in total: 5 boottest objects created
# - Part B1: one fixed effect
#            - cluster vars in boottest
#            - cluster vars in feols and fixest
#            - switch fe on and off
#            - in total: 10 boottest objects created
# - Part C1: two fixed effects
#            - cluster vars in boottest
#            - cluster vars in feols and fixest
#            - switch fe on and off
#            - in total: 10 boottest objects created
# ------------------------------------------------------------------ # 

tol <- 1e-04

# library(fwildclusterboot)
# ---------------------------------------------------------------------------------------------- # 
# Part 1: one cluster variable
# ---------------------------------------------------------------------------------------------- # 

# ---------------------------------------------------------------------------------------------- # 
# Part A1: no fixed effect in model

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             weights = 1:500/ 500, 
             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                           weights = 1:500/ 500, 
                           data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,  
                      weights = 1:500/ 500, 
                      data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                             weights = 1:500/ 500, 
                             cluster = "group_id1",
                             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 | group_id1,
                        weights = 1:500/ 500, 
                        data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = tol)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = tol)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = tol)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = tol)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = tol)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = tol)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = tol)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = tol)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = tol)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = tol)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = tol)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = tol)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = tol)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = tol)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = tol)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = tol)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = tol)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = tol)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = tol)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = tol)





# ---------------------------------------------------------------------------------------------- # 
# Part B1: one fixed effect in model

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,  weights = 1:500/ 500, 
             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,  weights = 1:500/ 500, 
                           data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,  weights = 1:500/ 500, 
                      data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,  weights = 1:500/ 500, 
                             cluster = "group_id1",
                             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration  | 0 | group_id1, weights = 1:500/ 500, 
                        data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))

# boot_fixest_fe <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_felm_fe <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_fixest_c_fe <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_felm_c_fe <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = tol)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = tol)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = tol)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = tol)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = tol)

# expect_equivalent(boot_lm$point_estimate, boot_fixest_fe$point_estimate, tol = tol)
# expect_equivalent(boot_fixest_fe$point_estimate, boot_felm_fe$point_estimate, tol = tol)
# expect_equivalent(boot_felm_fe$point_estimate, boot_fixest_c_fe$point_estimate, tol = tol)
# expect_equivalent(boot_fixest_c_fe$point_estimate, boot_felm_c_fe$point_estimate, tol = tol)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = tol)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = tol)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = tol)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = tol)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = tol)

# expect_equivalent(boot_lm$p_val, boot_fixest_fe$p_val, tol = tol)
# expect_equivalent(boot_fixest_fe$p_val, boot_felm_fe$p_val, tol = tol)
# expect_equivalent(boot_felm_fe$p_val, boot_fixest_c_fe$p_val, tol = tol)
# expect_equivalent(boot_fixest_c_fe$p_val, boot_felm_c_fe$p_val, tol = tol)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = tol)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = tol)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = tol)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = tol)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = tol)

# expect_equivalent(boot_lm$t_stat, boot_fixest_fe$t_stat, tol = tol)
# expect_equivalent(boot_fixest_fe$t_stat, boot_felm_fe$t_stat, tol = tol)
# expect_equivalent(boot_felm_fe$t_stat, boot_fixest_c_fe$t_stat, tol = tol)
# expect_equivalent(boot_fixest_c_fe$t_stat, boot_felm_c_fe$t_stat, tol = tol)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = tol)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = tol)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = tol)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = tol)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = tol)

# expect_equivalent(boot_lm$conf_int, boot_fixest_fe$conf_int, tol = tol)
# expect_equivalent(boot_fixest_fe$conf_int, boot_felm_fe$conf_int, tol = tol)
# expect_equivalent(boot_felm_fe$conf_int, boot_fixest_c_fe$conf_int, tol = tol)
# expect_equivalent(boot_fixest_c_fe$conf_int, boot_felm_c_fe$conf_int, tol = tol)



# ---------------------------------------------------------------------------------------------- # 
# Part C1: two fixed effects in model

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense, weights = 1:500/ 500, 
             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,  weights = 1:500/ 500, 
                           data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,  weights = 1:500/ 500, 
                      data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,  weights = 1:500/ 500, 
                             cluster = "group_id1",
                             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense | 0 | group_id1, weights = 1:500/ 500, 
                        data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))

# boot_lm_fe <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_fixest_fe <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_felm_fe <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_fixest_c_fe <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_felm_c_fe <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))


# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = tol)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = tol)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = tol)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = tol)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = tol)

# expect_equivalent(boot_lm$point_estimate, boot_fixest_fe$point_estimate, tol = tol)
# expect_equivalent(boot_fixest_fe$point_estimate, boot_felm_fe$point_estimate, tol = tol)
# expect_equivalent(boot_felm_fe$point_estimate, boot_fixest_c_fe$point_estimate, tol = tol)
# expect_equivalent(boot_fixest_c_fe$point_estimate, boot_felm_c_fe$point_estimate, tol = tol)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = tol)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = tol)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = tol)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = tol)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = tol)

# expect_equivalent(boot_lm$p_val, boot_fixest_fe$p_val, tol = tol)
# expect_equivalent(boot_fixest_fe$p_val, boot_felm_fe$p_val, tol = tol)
# expect_equivalent(boot_felm_fe$p_val, boot_fixest_c_fe$p_val, tol = tol)
# expect_equivalent(boot_fixest_c_fe$p_val, boot_felm_c_fe$p_val, tol = tol)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = tol)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = tol)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = tol)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = tol)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = tol)

# expect_equivalent(boot_lm$t_stat, boot_fixest_fe$t_stat, tol = tol)
# expect_equivalent(boot_fixest_fe$t_stat, boot_felm_fe$t_stat, tol = tol)
# expect_equivalent(boot_felm_fe$t_stat, boot_fixest_c_fe$t_stat, tol = tol)
# expect_equivalent(boot_fixest_c_fe$t_stat, boot_felm_c_fe$t_stat, tol = tol)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = tol)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = tol)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = tol)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = tol)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = tol)

# expect_equivalent(boot_lm$conf_int, boot_fixest_fe$conf_int, tol = tol)
# expect_equivalent(boot_fixest_fe$conf_int, boot_felm_fe$conf_int, tol = tol)
# expect_equivalent(boot_felm_fe$conf_int, boot_fixest_c_fe$conf_int, tol = tol)
# expect_equivalent(boot_fixest_c_fe$conf_int, boot_felm_c_fe$conf_int, tol = tol)


# ---------------------------------------------------------------------------------------------- # 
# Part 2: two cluster variables
# ---------------------------------------------------------------------------------------------- # 

# ---------------------------------------------------------------------------------------------- # 
# Part A2: no fixed effect in model

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,  weights = 1:500/ 500, 
             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,  weights = 1:500/ 500, 
                           data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,  weights = 1:500/ 500, 
                      data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,  weights = 1:500/ 500, 
                             cluster = "group_id1",
                             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 | group_id1, weights = 1:500/ 500, 
                        data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = tol)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = tol)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = tol)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = tol)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = tol)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = tol)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = tol)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = tol)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = tol)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = tol)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = tol)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = tol)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = tol)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = tol)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = tol)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = tol)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = tol)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = tol)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = tol)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = tol)




# ---------------------------------------------------------------------------------------------- # 
# Part B2: one fixed effect in model

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,  weights = 1:500/ 500, 
             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,  weights = 1:500/ 500, 
                           data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,  weights = 1:500/ 500, 
                      data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration,  weights = 1:500/ 500, 
                             cluster = "group_id1",
                             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration  | 0 | group_id1, weights = 1:500/ 500, 
                        data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))

# boot_lm_fe <-  suppressWarnings(boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_fixest_fe <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_felm_fe <- suppressWarnings(boottest(object = felm_fit, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_fixest_c_fe <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_felm_c_fe <- suppressWarnings(boottest(object = felm_fit_c, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = tol)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = tol)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = tol)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = tol)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = tol)

# expect_equivalent(boot_lm$point_estimate, boot_fixest_fe$point_estimate, tol = tol)
# expect_equivalent(boot_fixest_fe$point_estimate, boot_felm_fe$point_estimate, tol = tol)
# expect_equivalent(boot_felm_fe$point_estimate, boot_fixest_c_fe$point_estimate, tol = tol)
# expect_equivalent(boot_fixest_c_fe$point_estimate, boot_felm_c_fe$point_estimate, tol = tol)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = tol)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = tol)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = tol)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = tol)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = tol)

# expect_equivalent(boot_lm$p_val, boot_fixest_fe$p_val, tol = tol)
# expect_equivalent(boot_fixest_fe$p_val, boot_felm_fe$p_val, tol = tol)
# expect_equivalent(boot_felm_fe$p_val, boot_fixest_c_fe$p_val, tol = tol)
# expect_equivalent(boot_fixest_c_fe$p_val, boot_felm_c_fe$p_val, tol = tol)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = tol)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = tol)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = tol)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = tol)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = tol)

# expect_equivalent(boot_lm$t_stat, boot_fixest_fe$t_stat, tol = tol)
# expect_equivalent(boot_fixest_fe$t_stat, boot_felm_fe$t_stat, tol = tol)
# expect_equivalent(boot_felm_fe$t_stat, boot_fixest_c_fe$t_stat, tol = tol)
# expect_equivalent(boot_fixest_c_fe$t_stat, boot_felm_c_fe$t_stat, tol = tol)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = tol)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = tol)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = tol)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = tol)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = tol)

# expect_equivalent(boot_lm$conf_int, boot_fixest_fe$conf_int, tol = tol)
# expect_equivalent(boot_fixest_fe$conf_int, boot_felm_fe$conf_int, tol = tol)
# expect_equivalent(boot_felm_fe$conf_int, boot_fixest_c_fe$conf_int, tol = tol)
# expect_equivalent(boot_fixest_c_fe$conf_int, boot_felm_c_fe$conf_int, tol = tol)



# ---------------------------------------------------------------------------------------------- # 
# Part C2: two fixed effects in model

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,  weights = 1:500/ 500, 
             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,  weights = 1:500/ 500, 
                           data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,  weights = 1:500/ 500, 
                      data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,  weights = 1:500/ 500, 
                             cluster = "group_id1",
                             data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense | 0 | group_id1, weights = 1:500/ 500, 
                        data = fwildclusterboot:::create_data(N = 500, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))

# boot_lm_fe <-  suppressWarnings(boottest(object = lm_fit, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_fixest_fe <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_felm_fe <- suppressWarnings(boottest(object = felm_fit, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_fixest_c_fe <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))
# boot_felm_c_fe <- suppressWarnings(boottest(object = felm_fit_c, clustid = c("group_id1", "group_id2"), fe = "Q1_immigration", B = 999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE)))


# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = tol)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = tol)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = tol)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = tol)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = tol)

# expect_equivalent(boot_lm$point_estimate, boot_fixest_fe$point_estimate, tol = tol)
# expect_equivalent(boot_fixest_fe$point_estimate, boot_felm_fe$point_estimate, tol = tol)
# expect_equivalent(boot_felm_fe$point_estimate, boot_fixest_c_fe$point_estimate, tol = tol)
# expect_equivalent(boot_fixest_c_fe$point_estimate, boot_felm_c_fe$point_estimate, tol = tol)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = tol)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = tol)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = tol)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = tol)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = tol)

# expect_equivalent(boot_lm$p_val, boot_fixest_fe$p_val, tol = tol)
# expect_equivalent(boot_fixest_fe$p_val, boot_felm_fe$p_val, tol = tol)
# expect_equivalent(boot_felm_fe$p_val, boot_fixest_c_fe$p_val, tol = tol)
# expect_equivalent(boot_fixest_c_fe$p_val, boot_felm_c_fe$p_val, tol = tol)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = tol)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = tol)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = tol)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = tol)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = tol)

# expect_equivalent(boot_lm$t_stat, boot_fixest_fe$t_stat, tol = tol)
# expect_equivalent(boot_fixest_fe$t_stat, boot_felm_fe$t_stat, tol = tol)
# expect_equivalent(boot_felm_fe$t_stat, boot_fixest_c_fe$t_stat, tol = tol)
# expect_equivalent(boot_fixest_c_fe$t_stat, boot_felm_c_fe$t_stat, tol = tol)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = tol)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = tol)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = tol)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = tol)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = tol)

# expect_equivalent(boot_lm$conf_int, boot_fixest_fe$conf_int, tol = tol)
# expect_equivalent(boot_fixest_fe$conf_int, boot_felm_fe$conf_int, tol = tol)
# expect_equivalent(boot_felm_fe$conf_int, boot_fixest_c_fe$conf_int, tol = tol)
# expect_equivalent(boot_fixest_c_fe$conf_int, boot_felm_c_fe$conf_int, tol = tol)

