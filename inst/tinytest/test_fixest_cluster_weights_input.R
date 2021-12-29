# see this issue: https://github.com/s3alfisc/fwildclusterboot/issues/26

data1 <<- fwildclusterboot:::create_data(N = 500, N_G1 = 6, icc1 = 0.01, N_G2 = 4, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 9865)

feols1 <-feols(proposition_vote ~ treatment + ideology1 + log_income + group_id2,
                data = data1, 
                weights = ~weights)
feols2 <-feols(proposition_vote ~ treatment + ideology1 + log_income + group_id2,
               data = data1, 
               weights = data1$weights)

feols3 <-feols(proposition_vote ~ treatment + ideology1 + log_income + group_id2,
                         data = data1, 
                         cluster = ~ group_id1)
feols4 <- feols(proposition_vote ~ treatment + ideology1 + log_income + group_id2,
                data = data1, 
                cluster = data1$group_id1) 

boot1 <- boottest(feols1, param = "treatment", B = 999, clustid = "group_id1", seed = 12342)
boot2 <- boottest(feols2, param = "treatment", B = 999, clustid = "group_id1", seed = 12342)
boot3 <- boottest(feols3, param = "treatment", B = 999, clustid = "group_id1", seed = 12342)
boot4 <- boottest(feols4, param = "treatment", B = 999, clustid = "group_id1", seed = 12342)

expect_equal(generics::tidy(boot1), generics::tidy(boot2))
expect_equal(generics::tidy(boot3), generics::tidy(boot4))



