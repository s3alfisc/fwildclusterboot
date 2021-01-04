# Test 14: Test for warnings in preprocessing of fixest and felm with regards to missing 
#          values and on-the-fly adjustments of standard errors



voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345)
voters[1:2, proposition_vote:=NA]
voters[3, group_id1 := NA]      #
voters[4, group_id2 := NA]
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration",weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income |Q1_immigration | 0 | 0, data = voters)
# summary(feols_fit, se = "twoway", cluster = c("group_id1", "group_id2"))
# feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, cluster =  c("group_id1", "group_id2"),  fixef =  "Q1_immigration",weights = NULL, data = voters)
# summary(feols_fit, se = "cluster", cluster = c("group_id1"))


# check 1: no variables specified in feols() but in boottest()
res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id1"))
expect_equal(res$N + sum(is.na(voters$group_id1)), 10000 - 2)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id1"))
expect_equal(res$N + sum(is.na(voters$group_id1)), 10000 - 2)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id2"))
expect_equal(res$N + sum(is.na(voters$group_id2)), 10000 - 2)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id2"))
expect_equal(res$N + sum(is.na(voters$group_id2)), 10000 - 2)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id1", "group_id2"))
expect_equal(res$N + sum(is.na(voters$group_id1)) + sum(is.na(voters$group_id2)), 10000 - 2)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id1", "group_id2"))
expect_equal(res$N + sum(is.na(voters$group_id1)) + sum(is.na(voters$group_id2)), 10000 - 2)

# check 2: same variables specified in feols() as in boottest()
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, cluster = "group_id1",fixef =  "Q1_immigration",weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 | group_id1, data = voters)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id1"))
expect_equal(res$N , feols_fit$nobs)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id1"))
expect_equal(res$N , felm_fit$N)

feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, cluster = c("group_id1", "group_id2"),fixef =  "Q1_immigration",weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 |group_id1 + group_id2, data = voters)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id1", "group_id2"))
expect_equal(res$N , feols_fit$nobs)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id1", "group_id2"))
expect_equal(res$N , felm_fit$N)

# check 3: more variables specified in feols() than in boottest()
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, cluster = c("group_id1", "group_id2"),fixef =  "Q1_immigration",weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 |group_id1 + group_id2, data = voters)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id1"))
expect_equal(res$N , feols_fit$nobs)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id1"))
expect_equal(res$N , felm_fit$N)

# check 4: more variables specified in boottest() than in feols()
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, cluster = c("group_id1"),fixef =  "Q1_immigration",weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 |group_id1 , data = voters)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id1", "group_id2"))
expect_equal(res$N + 1 , feols_fit$nobs)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id1", "group_id2"))
expect_equal(res$N + 1 , felm_fit$N)

# check 5: 
voters[5, Q1_immigration := NA]
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, cluster = c("group_id1"),fixef =  "Q1_immigration",weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 |group_id1 , data = voters)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id1", "Q1_immigration"))
expect_equal(res$N , feols_fit$nobs)

res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id1", "Q1_immigration"))
expect_equal(res$N , felm_fit$N)

# check7: drop clustid group_id1 and add clustid group_id2 not in covs
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, cluster = "group_id1", fixef =  "Q1_immigration",weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 |group_id1 , data = voters)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id2"))
expect_equal(res$N , feols_fit$nobs - 1)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id2"))
expect_equal(res$N , felm_fit$N - 1)

# check 6: without NAs
voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345)
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, fixef =  "Q1_immigration",weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, data = voters)

res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id1"))
expect_equal(res$N, feols_fit$nobs)
res <- boottest(feols_fit, param = "treatment", B = 1000, clustid = c("group_id2"))
expect_equal(res$N, feols_fit$nobs)

res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id1"))
expect_equal(res$N, felm_fit$N)
res <- boottest(felm_fit, param = "treatment", B = 1000, clustid = c("group_id2"))
expect_equal(res$N, felm_fit$N)

