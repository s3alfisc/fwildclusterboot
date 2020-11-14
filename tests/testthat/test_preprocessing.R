
B <- 1000
seed <- 2180019
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 50, icc = 0.01)
voters[, iv:=rnorm(nrow(voters))]
head(voters)

felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration, weights = NULL, data = voters)
felm_fit1 <- felm(proposition_vote ~ treatment + ideology + log_income  + Q1_immigration, weights = NULL, data = voters)

feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
feols_fit1 <- feols(proposition_vote ~ treatment + ideology + log_income + Q1_immigration, weights = NULL, data = voters)
#feols_fit2 <- feols(proposition_vote ~ 0 + treatment + ideology + log_income + Q1_immigration, weights = NULL, data = voters)

feols_fit_w_intercept <- feols(proposition_vote ~ treatment + ideology + log_income, weights = NULL, data = voters)
feols_fit_wo_intercept <- feols(proposition_vote ~ 0 + treatment + ideology + log_income, weights = NULL, data = voters)


# ------------------------------------------------------ #
# 1) check preprocessing of felm()

test_that("preprocess_felm() argument alpha",{
  
  # test alpha
  expect_error(preprocess_felm(object = felm_fit, 
                               param = "treatment", 
                               clustid = voters$group_id, 
                               beta0 = 0, 
                               alpha = 2))
  expect_error(preprocess_felm(object = felm_fit, 
                               param = "treatment", 
                               clustid = voters$group_id, 
                               beta0 = 0, 
                               alpha = -1))
  expect_error(preprocess_felm(object = felm_fit, 
                               param = "treatment", 
                               clustid = voters$group_id, 
                               beta0 = 0, 
                               alpha = "a"))
  expect_error(preprocess_felm(object = felm_fit, 
                               param = "treatment", 
                               clustid = voters$group_id, 
                               beta0 = 0, 
                               alpha = c(0.1, 0.2)))
})


test_that("preprocess_felm() argument param",{
  
  # expect error when param not in formula
  expect_error(preprocess_felm(object = felm_fit, 
                               param = "treat", 
                               clustid = voters$group_id, 
                               beta0 = 0, 
                               alpha = 0.05))
})

test_that("preprocess_felm() returns the same objects for two equivalent formulas",{
  
  model_formulation_1 <- 
    preprocess_felm(object = felm_fit, 
                    param = "treatment", 
                    clustid = voters$group_id, 
                    beta0 = 0, 
                    alpha = 0.05)
  expect_equal(ncol(model_formulation_1$fixed_effects), 1)
  expect_equal(ncol(model_formulation_1$data), model_formulation_1$k )
  
  model_formulation_2 <- 
    preprocess_felm(object = felm_fit1, 
                    param = "treatment", 
                    clustid = voters$group_id, 
                    beta0 = 0, 
                    alpha = 0.05)
  expect_equal(ncol(model_formulation_2$fixed_effects), NULL)
  expect_equal(ncol(model_formulation_1$data), model_formulation_1$k )
  
})



# ------------------------------------------------------ #
# 1) check preprocessing of fixest()

test_that("preprocess.fixest() argument alpha",{
  
  # test alpha
  expect_error(preprocess.fixest(object = feols_fit, 
                                 param = "treatment", 
                                 clustid = voters$group_id, 
                                 beta0 = 0, 
                                 alpha = 2))
  expect_error(preprocess.fixest(object = feols_fit, 
                                 param = "treatment", 
                                 clustid = voters$group_id, 
                                 beta0 = 0, 
                                 alpha = -1))
  expect_error(preprocess.fixest(object = feols_fit, 
                                 param = "treatment", 
                                 clustid = voters$group_id, 
                                 beta0 = 0, 
                                 alpha = "a"))
  expect_error(preprocess.fixest(object = feols_fit, 
                                 param = "treatment", 
                                 clustid = voters$group_id, 
                                 beta0 = 0, 
                                 alpha = c(0.1, 0.2)))
})

test_that("preprocess.fixest() argument param",{
  
  # expect error when param not in formula
  expect_error(preprocess.fixest(object = feols_fit, 
                                 param = "treat", 
                                 clustid = voters$group_id, 
                                 beta0 = 0, 
                                 alpha = 0.05))
})


test_that("preprocess_fixest() returns the same objects for two equivalent formulas",{
  
  model_formulation_1 <- 
    preprocess.fixest(object = feols_fit, 
                      param = "treatment", 
                      clustid = voters$group_id, 
                      beta0 = 0, 
                      alpha = 0.05)
  expect_equal(ncol(model_formulation_1$fixed_effects), 1)
  expect_equal(ncol(model_formulation_1$data), model_formulation_1$k )
  
  model_formulation_2 <- 
    preprocess.fixest(object = feols_fit1, 
                      param = "treatment", 
                      clustid = voters$group_id, 
                      beta0 = 0, 
                      alpha = 0.05)
  expect_equal(ncol(model_formulation_2$fixed_effects), NULL)
  expect_equal(ncol(model_formulation_1$data), model_formulation_1$k )
  
})

test_that("feols intercept handling",{
  
  w_intercept <- 
  preprocess.fixest(object = feols_fit_w_intercept, 
                    param = "treatment", 
                    clustid = voters$group_id, 
                    beta0 = 0, 
                    alpha = 0.05)
  wo_intercept <- 
    preprocess.fixest(object = feols_fit_wo_intercept, 
                      param = "treatment", 
                      clustid = voters$group_id, 
                      beta0 = 0, 
                      alpha = 0.05)
  
  expect_equal(c("(Intercept)", colnames(wo_intercept$X)), 
               colnames(w_intercept$X))

})



# 3) Check preprocessing of lm()


