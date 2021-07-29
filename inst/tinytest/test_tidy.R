# test method for boottest objects
# 
# library(fwildclusterboot)

base::options(warn = 1)

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
            data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                          data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                     data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                            cluster = "group_id1",
                            data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 | group_id1,
                       data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  boottest(object = lm_fit, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min")
boot_fixest <- boottest(object = feols_fit, clustid = c("group_id1"), B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min")
boot_felm <- boottest(object = felm_fit, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min")
boot_fixest_c <- boottest(object = feols_fit_c, clustid = c("group_id1"), B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min")
boot_felm_c <- boottest(object = felm_fit_c, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min")

# test summary method
expect_equivalent(summary(boot_lm), summary(boot_fixest), tolerance = 0.1)
expect_equivalent(summary(boot_fixest), summary(boot_felm), tolerance = 0.1)
expect_equivalent(summary(boot_felm), summary(boot_fixest_c), tolerance = 0.1)
expect_equivalent(summary(boot_fixest_c), summary(boot_felm_c), tolerance = 0.1)
expect_equivalent(summary(boot_felm_c), summary(boot_lm), tolerance = 0.1)

# test tidy method
expect_equivalent(fwildclusterboot:::tidy.boottest(boot_lm), fwildclusterboot:::tidy.boottest(boot_fixest), tolerance = 0.1)
expect_equivalent(fwildclusterboot:::tidy.boottest(boot_fixest), fwildclusterboot:::tidy.boottest(boot_felm), tolerance = 0.1)
expect_equivalent(fwildclusterboot:::tidy.boottest(boot_felm), fwildclusterboot:::tidy.boottest(boot_fixest_c), tolerance = 0.1)
expect_equivalent(fwildclusterboot:::tidy.boottest(boot_fixest_c), fwildclusterboot:::tidy.boottest(boot_felm_c), tolerance = 0.1)
expect_equivalent(fwildclusterboot:::tidy.boottest(boot_felm_c), fwildclusterboot:::tidy.boottest(boot_lm), tolerance = 0.1)

# test glance method

# test plot method
expect_equivalent(fwildclusterboot:::plot.boottest(boot_lm), fwildclusterboot:::plot.boottest(boot_fixest), tolerance = 0.1)
expect_equivalent(fwildclusterboot:::plot.boottest(boot_fixest), fwildclusterboot:::plot.boottest(boot_felm), tolerance = 0.1)
expect_equivalent(fwildclusterboot:::plot.boottest(boot_felm), fwildclusterboot:::plot.boottest(boot_fixest_c), tolerance = 0.1)
expect_equivalent(fwildclusterboot:::plot.boottest(boot_fixest_c), fwildclusterboot:::plot.boottest(boot_felm_c), tolerance = 0.1)
expect_equivalent(fwildclusterboot:::plot.boottest(boot_felm_c), fwildclusterboot:::plot.boottest(boot_lm), tolerance = 0.1)

