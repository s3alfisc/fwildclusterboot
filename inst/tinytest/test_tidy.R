# test tidy method

library(fwildclusterboot)
library(lfe)
library(fixest)

base::options(warn = 1)

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

boot_lm <-  tidy(boottest(object = lm_fit, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min"))
boot_fixest <- tidy(boottest(object = feols_fit, clustid = c("group_id1"), B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min"))
boot_felm <- tidy(boottest(object = felm_fit, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min"))
boot_fixest_c <- tidy(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min"))
boot_felm_c <- tidy(boottest(object = felm_fit_c, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min"))

expect_equivalent(boot_lm, boot_fixest, tolerance = 0.1)
expect_equivalent(boot_fixest, boot_felm, tolerance = 0.1)
expect_equivalent(boot_felm, boot_fixest_c, tolerance = 0.1)
expect_equivalent(boot_fixest_c, boot_felm_c, tolerance = 0.1)
expect_equivalent(boot_felm_c, boot_lm, tolerance = 0.1)

