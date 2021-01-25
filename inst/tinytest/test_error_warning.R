# ------------------------------------------------------------------ # 
# test for warnings and errors
# ------------------------------------------------------------------ # 

# test boottest function arguments for errors 
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                           data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                      data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

# sign_level
expect_error(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                      sign_level = 1.1))
expect_error(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                      sign_level = 1.1))
expect_error(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                      sign_level = 1.1))
expect_error(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                      sign_level = -1.1))
expect_error(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                      sign_level = -1.1))
expect_error(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                      sign_level = -1.1))


# B < 100
expect_error(boottest(object = lm_fit, clustid =  "group_id1", B = 99, seed = 911, param = "treatment", conf_int = TRUE))
expect_error(boottest(object = feols_fit, clustid = c("group_id1"), B = 99, seed = 911, param = "treatment", conf_int = TRUE))
expect_error(boottest(object = felm_fit, clustid =  "group_id1", B = 99, seed = 911, param = "treatment", conf_int = TRUE))

# param not in data.frame
expect_error(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment1", conf_int = TRUE))
expect_error(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment1", conf_int = TRUE))
expect_error(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment1", conf_int = TRUE))

# rademacher draws
expect_warning(boottest(object = lm_fit, clustid =  "group_id1", B = 9999, seed = 911, param = "treatment", conf_int = TRUE))
expect_warning(boottest(object = feols_fit, clustid = c("group_id1"), B = 9999, seed = 911, param = "treatment", conf_int = TRUE))
expect_warning(boottest(object = felm_fit, clustid =  "group_id1", B = 9999, seed = 911, param = "treatment", conf_int = TRUE))

# test for banned function arguments and syntax for fixest
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + i(log_income, Q1_immigration), 
                           data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
expect_error(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment1", conf_int = TRUE))

# joint fe != NULL and weights = on
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                           weights = 1:10000/ 10000, 
                           data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,  
                      weights = 1:10000/ 10000, 
                      data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
expect_error(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, fe = "Q1_immigration"))
expect_error(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, fe = "Q1_immigration"))

