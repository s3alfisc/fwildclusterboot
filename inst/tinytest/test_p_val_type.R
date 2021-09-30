# test if different boottest methods produce equivalent results for different
# p-val types


lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.1, N_G2 = 10, icc2 = 0.1, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                   data = fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.1, N_G2 = 10, icc2 = 0.1, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                 data = fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.1, N_G2 = 10, icc2 = 0.1, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                     cluster = "group_id1",
                     data = fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.1, N_G2 = 10, icc2 = 0.1, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 | group_id1,
                   data = fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.1, N_G2 = 10, icc2 = 0.1, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))


# equal-tailed

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = "equal-tailed"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = "equal-tailed"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = "equal-tailed"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = "equal-tailed"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = "equal-tailed"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = 1e-04)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = 1e-04)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = 1e-04)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = 1e-04)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = 1e-04)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = 1e-04)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = 1e-04)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = 1e-04)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = 1e-04)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = 1e-04)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = 1e-04)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = 1e-04)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = 1e-04)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = 1e-04)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = 1e-04)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = 1e-04)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = 1e-04)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = 1e-04)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = 1e-04)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = 1e-04)

# ">"

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = ">"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = ">"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = ">"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = ">"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, p_val_type = ">"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = 1e-04)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = 1e-04)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = 1e-04)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = 1e-04)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = 1e-04)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = 1e-04)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = 1e-04)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = 1e-04)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = 1e-04)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = 1e-04)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = 1e-04)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = 1e-04)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = 1e-04)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = 1e-04)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = 1e-04)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = 1e-04)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = 1e-04)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = 1e-04)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = 1e-04)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = 1e-04)







# ---------------------------------------------------------------------------------------------- # 
# Part 2: two cluster variables
# ---------------------------------------------------------------------------------------------- # 

#  "<"


boot_lm <-  suppressWarnings(boottest(object = lm_fit, 
                                      clustid =  "group_id1", 
                                      B = 9999,
                                      seed = 91, 
                                      param = "treatment", 
                                      conf_int = TRUE, 
                                      p_val_type = "<"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit,
                                         clustid = c("group_id1"), 
                                         B = 9999,
                                         seed = 91, 
                                         param = "treatment",
                                         conf_int = TRUE,
                                         p_val_type = "<"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, 
                                       clustid =  "group_id1",
                                       B = 9999, 
                                       seed = 91,
                                       param = "treatment",
                                       conf_int = TRUE,
                                       p_val_type = "<"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, 
                                           clustid = "group_id1",
                                           B = 9999, 
                                           seed = 91, 
                                           param = "treatment",
                                           conf_int = TRUE, 
                                           p_val_type = "<"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c,
                                         clustid =  "group_id1", 
                                         B = 9999, 
                                         seed = 91,
                                         param = "treatment", 
                                         conf_int = TRUE,
                                         p_val_type = "<"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tol = 1e-04)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tol = 1e-04)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tol = 1e-04)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tol = 1e-04)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tol = 1e-04)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tol = 1e-04)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tol = 1e-04)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tol = 1e-04)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tol = 1e-04)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tol = 1e-04)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tol = 1e-04)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tol = 1e-04)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tol = 1e-04)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tol = 1e-04)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tol = 1e-04)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int, tol = 1e-04)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int, tol = 1e-04)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tol = 1e-04)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tol = 1e-04)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol = 1e-04)

