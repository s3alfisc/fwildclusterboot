library(testthat)
library(fwildclusterboot)







test_check("fwildclusterboot")

test_that("test get_model_frame() and get_fixed_effects() for objects of class fixest"){
  
  B <- 10000
  seed <- 42
  set.seed(seed)
  voters <- create_data_1(N = 2000, N_G = 20, icc = 0.5)
  voters <- fabricatr::fabricate(
    N = 2000,
    group_id = rep(1:100, 20),
    ideology = fabricatr::draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = 0.01),
    ideological_label = fabricatr::draw_ordered(
      x = ideology,
      break_labels = c(
        "Very Conservative", "Conservative",
        "Liberal", "Very Liberal"
      )
    ),
    income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
    Q1_immigration = fabricatr::draw_likert(x = ideology, type = 7),
    Q2_defence = fabricatr::draw_likert(x = ideology + 0.5, type = 7),
    treatment = fabricatr::draw_binary(0.5, N = N),
    proposition_vote = fabricatr::draw_binary(latent = ideology + 0.01 * treatment, link = "probit")
  )
  
  data.table::setDT(voters)
  voters[, log_income := log(income)]
  
  feols_fit_2 <- feols(proposition_vote ~ treatment + ideology + log_income |  Q1_immigration + Q2_defence, weights = NULL, data = voters)
  
  voters[, Q1_immigration := as.factor(Q1_immigration) ]
  voters[, Q2_defence := as.factor(Q2_defence)]
  
  # estimate regressions
  feols_fit_1 <- feols(proposition_vote ~ treatment + ideology + log_income, fixef = c("Q1_immigration", "Q2_defence"), weights = NULL, data = voters)
  feols_fit_3 <- feols(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, weights = NULL, data = voters)
  
  # test get_model_frame
  model_frame_fit_1 <- cbind(get_model_frame(feols_fit_1), get_model_fe(feols_fit_1))
  model_frame_fit_2 <- cbind(get_model_frame(feols_fit_2), get_model_fe(feols_fit_2))
  model_frame_fit_3 <- get_model_frame(feols_fit_3)
  
  expect_identical(model_frame_fit_1, model_frame_fit_3)
  expect_identical(model_frame_fit_2, model_frame_fit_3)

  
}

test_that("test preprocessing for boottest.fixest"){
  
  B <- 10000
  seed <- 42
  set.seed(seed)
  voters <- create_data_1(N = 2000, N_G = 20, icc = 0.5)
  voters <- fabricatr::fabricate(
    N = 2000,
    group_id = rep(1:100, 20),
    ideology = fabricatr::draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = 0.01),
    ideological_label = fabricatr::draw_ordered(
      x = ideology,
      break_labels = c(
        "Very Conservative", "Conservative",
        "Liberal", "Very Liberal"
      )
    ),
    income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
    Q1_immigration = fabricatr::draw_likert(x = ideology, type = 7),
    Q2_defence = fabricatr::draw_likert(x = ideology + 0.5, type = 7),
    treatment = fabricatr::draw_binary(0.5, N = N),
    proposition_vote = fabricatr::draw_binary(latent = ideology + 0.01 * treatment, link = "probit")
  )
  
  data.table::setDT(voters)
  voters[, log_income := log(income)]
  voters[, Q1_immigration := as.factor(Q1_immigration) ]
  voters[, Q2_defence := as.factor(Q2_defence)]
  
  # estimate regressions
  feols_fit_1 <- feols(proposition_vote ~ treatment + ideology + log_income, fixef = c("Q1_immigration", "Q2_defence"), weights = NULL, data = voters)
  feols_fit_2 <- feols(proposition_vote ~ treatment + ideology + log_income |  Q1_immigration + Q2_defence, weights = NULL, data = voters)
  feols_fit_3 <- feols(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, weights = NULL, data = voters)
  
  # 
  preprocess_1 <- preprocess.fixest(object = feols_fit_1, param = "treatment", clustid = clustid, beta0 = 0)
  preprocess_2 <- preprocess.fixest(object = feols_fit_2, param = "treatment", clustid = clustid, beta0 = 0)
  preprocess_3 <- preprocess.fixest(object = feols_fit_3, param = "treatment", clustid = clustid, beta0 = 0)
  
  expect_equal(preprocess_1$N, preprocess_2$N, preprocess_3$N)
  expect_equal(preprocess_1$k, preprocess_3$k)
  expect_equal(preprocess_1$R0, preprocess_3$R0)
  expect_equal(preprocess_1$N_G, preprocess_3$N_G)
  
  
  
}


test_that("test that boottest.fixest runs properly, independent of how input looks"){
  
  B <- 10000
  seed <- 42
  set.seed(seed)
  voters <- create_data_1(N = 2000, N_G = 20, icc = 0.5)
  voters <- fabricatr::fabricate(
    N = 2000,
    group_id = rep(1:100, 20),
     ideology = fabricatr::draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = 0.01),
     ideological_label = fabricatr::draw_ordered(
       x = ideology,
       break_labels = c(
         "Very Conservative", "Conservative",
         "Liberal", "Very Liberal"
       )
     ),
     income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
     Q1_immigration = fabricatr::draw_likert(x = ideology, type = 7),
     Q2_defence = fabricatr::draw_likert(x = ideology + 0.5, type = 7),
     treatment = fabricatr::draw_binary(0.5, N = N),
     proposition_vote = fabricatr::draw_binary(latent = ideology + 0.01 * treatment, link = "probit")
   )
   
   data.table::setDT(voters)
   voters[, log_income := log(income)]
   voters[, Q1_immigration := as.factor(Q1_immigration) ]
   voters[, Q2_defence := as.factor(Q2_defence)]
  
   # estimate regressions
   feols_fit_1 <- feols(proposition_vote ~ treatment + ideology + log_income, fixef = c("Q1_immigration", "Q2_defence"), weights = NULL, data = voters)
   feols_fit_2 <- feols(proposition_vote ~ treatment + ideology + log_income |  Q2_defence, weights = NULL, data = voters)
   feols_fit_3 <- feols(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, weights = NULL, data = voters)
   
   # calculate p values and confidence sets
   boot_feols_fit_1 <- boottest(feols_fit_1, B = B, param = "treatment", clustid = voters$group_id)
   boot_feols_fit_2 <- boottest(feols_fit_2, B = B, param = "treatment", clustid = voters$group_id)
   boot_feols_fit_3 <- boottest(feols_fit_3, B = B, param = "treatment", clustid = voters$group_id)
  
   # test p-values for equivalence 
   expect_equivalent(boot_feols_fit_1$p_val, boot_feols_fit_3$p_val, tolerance = 0.01)
   expect_equivalent(boot_feols_fit_2$p_val, boot_feols_fit_3$p_val, tolerance = 0.01)
   
   # test confidence sets for equivalence
   expect_equivalent(boot_feols_fit_1$conf_int, boot_feols_fit_2$conf_int, tolerance = 0.01)
   expect_equivalent(boot_feols_fit_2$conf_int, boot_feols_fit_3$conf_int, tolerance = 0.01)
   
   # test t-stats for equivalence
   expect_equivalent(boot_feols_fit_1$t_stat, boot_feols_fit_2$t_stat, tolerance = 0.01)
   expect_equivalent(boot_feols_fit_2$t_stat, boot_feols_fit_3$t_stat, tolerance = 0.01)
   
   
}

# test_that("felm fe pre-processing and demeaning works properly", {
#   
#   B <- 10000
#   seed <- 42
#   set.seed(seed)
#   voters <- create_data_1(N = 10000, N_G = 20, icc = 0.5)
# 
#   felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration, weights = NULL, data = voters)
#   felm_fit1 <- felm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration, weights = NULL, data = voters)
#   
#   fml <- Formula::Formula(eval(felm_fit$call$formula, envir =  attr(felm_fit$terms, ".Environment"))), lhs = 0, rhs = 2)
#   expect_true(formula( != "~0")
#   expect_false(formula(Formula::Formula(eval(felm_fit1$call$formula, envir =  attr(felm_fit1$terms, ".Environment"))), lhs = 0, rhs = 1) != "~0")
#   
#   
#   if(formula(Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment"))), lhs = 0, rhs = 2) != "~0"){
#     fixed_effects <- get_model_fe(object)
#     demean_data <- fixest::demean(data, fixed_effects)
#     data <- as.data.frame(demean_data)
#     R0 <- as.numeric(param == c("(Intercept)", rownames(object$coefficients)))
#   } else{
#     R0 <- as.numeric(param == c(names(object$coefficients)))
#   }
#   
# })
# 
# 
# 
# 
# # test_that("test dimensions mat_mean_by_cluster", {
# #   
# #   N <- 1000
# #   k <- 5
# #   X <- matrix(rnorm(N * k), N, k)
# #   clustid <- sample(1:3, N, replace = TRUE)
# #   unique_clusters <- length(unique(clustid))
# #   
# #   expect_equal(dim(mat_mean_by_cluster(prod = X, clustid = clustid)), c(unique_clusters,k))
# #   
# # })
# 
# 
# test_that("test if results of boottest are similar to each results by sandwich se", {
#   
#   seed <- sample(1:1000, 1)
#   set.seed(seed)
#   
#   B <- 100000
# 
#   voters <- fabricatr::fabricate(
#     N = 10000,
#     group_id = rep(1:100, 100),
#     ideology = draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = 0.01),
#     ideological_label = draw_ordered(
#       x = ideology,
#       break_labels = c(
#         "Very Conservative", "Conservative",
#         "Liberal", "Very Liberal"
#       )
#     ),
#     income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
#     Q1_immigration = draw_likert(x = ideology, type = 7),
#     Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
#     treatment = draw_binary(0.5, N = N),
#     proposition_vote = draw_binary(latent = ideology + 0.01 * treatment, link = "probit")
#   )
#   
#   data.table::setDT(voters)
#   voters[, log_income := log(income)]
#   voters[, Q1_immigration := as.factor(Q1_immigration) ]
#   voters[, Q2_defence := as.factor(Q2_defence)]
#   
#   # estimate regressions 
#   
#   lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income +Q1_immigration + Q2_defence, weights = NULL, data = voters)
#   lm_robust_fit <- lm_robust(proposition_vote ~ treatment + ideology + log_income, fixed_effects = ~ Q1_immigration + Q2_defence, weights = NULL, data = voters)
#   lm_robust_fit1 <- lm_robust(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, weights = NULL, data = voters )
#   feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income, fixef = c("Q1_immigration", "Q2_defence"), weights = NULL, data = voters)
#   felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration + Q2_defence, weights = NULL, data = voters)
#   
#   # estimate benchmark regression
#   lm_robust_sandwich <-   lm_robust_fit1 <- lm_robust(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, weights = NULL, data = voters, clusters = voters$group_id )
#   p_val <-  lm_robust_sandwich$p.value["treatment"]
#   
#   # calculate p-values with fast bootstrap 
#   lm = boottest.lm(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
#   estimatr_fe = boottest.lm_robust(lm_robust_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
#   estimatr = boottest.lm_robust(lm_robust_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
#   felm = boottest.felm(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
#   fixest = boottest.fixest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
#   
#   # test
#   expect_equivalent(lm$p_val, p_val, tolerance = 0.01)
#   expect_equivalent(estimatr$p_val, p_val, tolerance = 0.01)
#   expect_equivalent(estimatr_fe$p_val, p_val, tolerance = 0.01)
#   expect_equivalent(felm$p_val, p_val, tolerance = 0.01)
#   expect_equivalent(fixest$p_val, p_val, tolerance = 0.01)
#   
# })
