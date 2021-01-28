# ------------------------------------------------------------------ # 
# test 1: bootcluster 
# Tests A1 and A2
# note: all tests without fixed effects should be identical, 
# with fixed effects: because point estimate are slightly different, 
# everything else might be slightly different (p-values, confidence intervals etc)
# point estimates are different because feols / felm probably use different
# convergence criteria
# so without fe: expect_equivalent else _equivalent
# ------------------------------------------------------------------ # 

library(fwildclusterboot)
library(lfe)
library(fixest)

# ---------------------------------------------------------------------------------------------- # 
# Part 1: one cluster variable
# ---------------------------------------------------------------------------------------------- # 

# ---------------------------------------------------------------------------------------------- # 
# Part A1: no fixed effect in model: rademacher

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                           data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                      data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                             cluster = "group_id1",
                             data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 | group_id1,
                        data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 1e-2)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 1e-2)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 1e-2)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tolerance = 1e-2)



# ---------------------------------------------------------------------------------------------- # 
# Part 2: two cluster variables
# ---------------------------------------------------------------------------------------------- # 

# ---------------------------------------------------------------------------------------------- # 
# Part A2: no fixed effect in model


boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "rademacher"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 1e-2)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 1e-2)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 1e-2)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tolerance = 1e-2)



# ---------------------------------------------------------------------------------------------- # 
# Part A1: no fixed effect in model: webb


boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 1e-2)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 1e-2)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 1e-2)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tolerance = 1e-2)


# ---------------------------------------------------------------------------------------------- # 
# Part 2: two cluster variables
# ---------------------------------------------------------------------------------------------- # 

# ---------------------------------------------------------------------------------------------- # 
# Part A2: no fixed effect in model

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "webb"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 1e-2)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 1e-2)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 1e-2)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tolerance = 1e-2)





# ---------------------------------------------------------------------------------------------- # 
# Part A1: no fixed effect in model: norm

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 1e-2)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 1e-2)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 1e-2)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tolerance = 1e-2)



# ---------------------------------------------------------------------------------------------- # 
# Part 2: two cluster variables
# ---------------------------------------------------------------------------------------------- # 

# ---------------------------------------------------------------------------------------------- # 
# Part A2: no fixed effect in model

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "norm"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 1e-2)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 1e-2)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 1e-2)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tolerance = 1e-2)




# ---------------------------------------------------------------------------------------------- # 
# Part A1: no fixed effect in model: mammen
# 

boot_lm <-  suppressWarnings(boottest(object = lm_fit, 
                                      clustid =  "group_id1", 
                                      B = 9999,
                                      seed = 91, 
                                      param = "treatment", 
                                      conf_int = TRUE, 
                                      type = "mammen"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit,
                                         clustid = c("group_id1"), 
                                         B = 9999,
                                         seed = 91, 
                                         param = "treatment",
                                         conf_int = TRUE,
                                         type = "mammen"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, 
                                       clustid =  "group_id1",
                                       B = 9999, 
                                       seed = 91,
                                       param = "treatment",
                                       conf_int = TRUE,
                                       type = "mammen"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, 
                                           clustid = "group_id1",
                                           B = 9999, 
                                           seed = 91, 
                                           param = "treatment",
                                           conf_int = TRUE, 
                                           type = "mammen"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c,
                                         clustid =  "group_id1", 
                                         B = 9999, 
                                         seed = 91,
                                         param = "treatment", 
                                         conf_int = TRUE,
                                         type = "mammen"))

# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 1e-2)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 1e-2)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 1e-2)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tolerance = 1e-2)

# all outputs need to be equal 
#lapply(1:length(boot_felm_c),function(i) expect_equivalent(boot_felm_c[[i]], boot_fixest_c[[i]]))



# ---------------------------------------------------------------------------------------------- # 
# Part 2: two cluster variables
# ---------------------------------------------------------------------------------------------- # 

# ---------------------------------------------------------------------------------------------- # 
# Part A2: no fixed effect in model

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "mammen"))
boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "mammen"))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "mammen"))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "mammen"))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  c("group_id1", "group_id2"), B = 999, seed = 91, param = "treatment", conf_int = TRUE, type = "mammen"))


# point estimates
expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate)
expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 1e-2)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 1e-2)

# p-vals
expect_equivalent(boot_lm$p_val, boot_fixest$p_val)
expect_equivalent(boot_fixest$p_val, boot_felm$p_val)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 1e-2)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 1e-2)

# t_stats
expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat)
expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 1e-2)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 1e-2)

# confidence intervals
expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int, tolerance = 1e-2)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tolerance = 1e-2)


