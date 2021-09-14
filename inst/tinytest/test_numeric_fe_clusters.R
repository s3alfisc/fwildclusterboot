# test invariance of boottest() results to type of fixed effect variable
# (numeric vs factor vs character)
# test issue https://github.com/s3alfisc/fwildclusterboot/issues/14
# raised by Timothée


# Test 1: one cluster variable is numeric vs no cluster variable is numeric

data(voters)

to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
sapply(voters[, to_char], class)

voters_1 <<- voters
voters_1$Q1_immigration <- as.numeric(voters_1$Q1_immigration)
sapply(voters_1[, to_char], class)

feols_fit <- feols(proposition_vote ~ treatment  + log_income | Q2_defense , data = voters)
feols_fit_2 <- feols(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters_1)
lfe_fit <- lfe::felm(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters)
lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters_1)

boot1 <- 
boottest(feols_fit,
         clustid = c("Q1_immigration","Q2_defense"),
         B = 9999,
         param = "treatment",
         bootcluster='min')
boot2 <- 
  boottest(feols_fit_2,
           clustid = c("Q1_immigration","Q2_defense"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot3 <- 
  boottest(feols_fit,
           clustid = c("Q1_immigration","Q2_defense"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot4 <- 
  boottest(feols_fit,
           clustid = c("Q1_immigration","Q2_defense"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
expect_equal(boot1$p_val, boot2$p_val)
expect_equal(boot2$p_val, boot3$p_val)
expect_equal(boot3$p_val, boot4$p_val)
expect_equal(boot4$p_val, boot1$p_val)


# Test 2: one fixed effect is numeric vs no fixed effect is numeric

data(voters)

to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
sapply(voters[, to_char], class)

voters_1 <<- voters
voters_1$Q2_defense <- as.numeric(voters_1$Q2_defense)
sapply(voters_1[, to_char], class)

feols_fit <- feols(proposition_vote ~ treatment  + log_income | Q2_defense , data = voters)
feols_fit_2 <- feols(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters_1)
lfe_fit <- lfe::felm(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters)
lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters_1)

boot1 <- 
  boottest(feols_fit,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot2 <- 
  boottest(feols_fit_2,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot3 <- 
  boottest(lfe_fit,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot4 <- 
  boottest(lfe_fit_2,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
expect_equal(boot1$p_val, boot2$p_val)
expect_equal(boot2$p_val, boot3$p_val)
expect_equal(boot3$p_val, boot4$p_val)
expect_equal(boot4$p_val, boot1$p_val)


# Test 3: all fixed effects and cluster variables are numeric vs factors

data(voters)

to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
voters$group_id1 <- as.factor(voters$group_id1)
sapply(voters[, to_char], class)

voters_1 <<- voters
voters_1$Q1_immigration <- as.numeric(voters_1$Q1_immigration)
voters_1$Q2_defense <- as.numeric(voters_1$Q2_defense)

sapply(voters_1[, to_char], class)

feols_fit <- feols(proposition_vote ~ treatment  + log_income | Q2_defense , data = voters)
feols_fit_2 <- feols(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters_1)
lfe_fit <- lfe::felm(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters)
lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment  + log_income | Q2_defense, data = voters_1)

boot1 <- 
  boottest(feols_fit,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot2 <- 
  boottest(feols_fit_2,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot3 <- 
  boottest(lfe_fit,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot4 <- 
  boottest(lfe_fit_2,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
expect_equal(boot1$p_val, boot2$p_val)
expect_equal(boot2$p_val, boot3$p_val)
expect_equal(boot3$p_val, boot4$p_val)
expect_equal(boot4$p_val, boot1$p_val)


# Test 4: Test 3, but now with two fixed effects


data(voters)

to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
voters$group_id1 <- as.factor(voters$group_id1)
sapply(voters[, to_char], class)

voters_1 <<- voters
voters_1$Q1_immigration <- as.numeric(voters_1$Q1_immigration)
voters_1$Q2_defense <- as.numeric(voters_1$Q2_defense)

sapply(voters_1[, to_char], class)

feols_fit <- feols(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense , data = voters)
feols_fit_2 <- feols(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense, data = voters_1)
lfe_fit <- lfe::felm(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense, data = voters)
lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense, data = voters_1)

boot1 <- 
  boottest(feols_fit,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot2 <- 
  boottest(feols_fit_2,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot3 <- 
  boottest(lfe_fit,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot4 <- 
  boottest(lfe_fit_2,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')

expect_equal(boot1$p_val, boot2$p_val)
expect_equal(boot2$p_val, boot3$p_val)
expect_equal(boot3$p_val, boot4$p_val)
expect_equal(boot4$p_val, boot1$p_val)



# What if a fixed effect is a character? 


data(voters)

to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
sapply(voters[, to_char], class)

voters_1 <<- voters
voters_1$Q1_immigration <- as.character(voters_1$Q1_immigration)
voters_1$Q2_defense <- as.character(voters_1$Q2_defense)

sapply(voters_1[, to_char], class)

feols_fit <- feols(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense , data = voters)
feols_fit_2 <- feols(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense, data = voters_1)
lfe_fit <- lfe::felm(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense, data = voters)
lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense, data = voters_1)

boot1 <- 
  boottest(feols_fit,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot2 <- 
  boottest(feols_fit_2,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot3 <- 
  boottest(lfe_fit,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')
boot4 <- 
  boottest(lfe_fit_2,
           clustid = c("Q1_immigration"),
           B = 9999,
           param = "treatment",
           bootcluster='min')

expect_equal(boot1$p_val, boot2$p_val)
expect_equal(boot2$p_val, boot3$p_val)
expect_equal(boot3$p_val, boot4$p_val)
expect_equal(boot4$p_val, boot1$p_val)
