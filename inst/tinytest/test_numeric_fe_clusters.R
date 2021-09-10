# test issue https://github.com/s3alfisc/fwildclusterboot/issues/14
# raised by Timoth?e
# Test 1: one cluster variable is numeric
# Test 2: one fixed effect is numeric
# Test 3: both fixed effect and cluster variables are numeric

data(voters)

to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
sapply(voters[, to_char], class)


# Test 1: one cluster variable is numeric
voters_1 <- voters
voters_1$Q2_defense <- as.numeric(voters_1$Q2_defense)
sapply(voters_1[, to_char], class)

feols_fit <- feols(proposition_vote ~ treatment  + log_income | Q2_defense , data = voters)
feols_fit_2 <- feols(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters_1)
lfe_fit <- lfe::felm(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters_1)

# bootstrap inference via boottest()
# expect_no_error
boottest(feols_fit,
                       clustid = c("Q1_immigration","Q2_defense"),
                       B = 9999,
                       param = "treatment",
                       bootcluster='min')

expect_error(boottest(feols_fit_2,
                         clustid = c("Q1_immigration","Q2_defense"),
                         B = 9999,
                         param = "treatment",
                         bootcluster='min'))

expect_error(boottest(lfe_fit,
                         clustid = c("Q1_immigration","Q2_defense"),
                         B = 9999,
                         param = "treatment",
                         bootcluster='min'))



# are results the same no matter the type of the clustering variable?

run <- FALSE
if(run){
  voters_2 <- voters
  voters_2$group_id1 <- as.numeric(voters_2$group_id1)
  
  feols_fit <- feols(proposition_vote ~ treatment  + log_income  , data = voters)
  feols_fit_2 <- feols(proposition_vote ~ treatment  + log_income , data = voters_2)
  lfe_fit <- lfe::felm(proposition_vote ~ treatment  + log_income , data = voters)
  lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment  + log_income , data = voters_2)
  
  feols_boot <- 
    boottest(feols_fit,
             clustid = c("Q1_immigration","Q2_defense"),
             B = 9999,
             param = "treatment",
             bootcluster='min')
  feols_boot_2 <- 
    boottest(feols_fit_2,
             clustid = c("Q1_immigration","Q2_defense"),
             B = 9999,
             param = "treatment",
             bootcluster='min')
  lfe_boot <- 
    boottest(lfe_fit,
             clustid = c("Q1_immigration","Q2_defense"),
             B = 9999,
             param = "treatment",
             bootcluster='min')
  lfe_boot_2 <- 
    boottest(lfe_fit_2,
             clustid = c("Q1_immigration","Q2_defense"),
             B = 9999,
             param = "treatment",
             bootcluster='min')
  expect_equal(feols_boot$p_val, feols_boot_2$p_val)
  expect_equal(feols_boot$p_val, lfe_boot$p_val)
  expect_equal(lfe_boot$p_val, lfe_boot_2$p_val)
  
  
}
