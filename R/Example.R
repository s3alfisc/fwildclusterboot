# library(devtools)
# library(usethis)
# 
# 
# 
# 
# # # ==================================================================== #
# # # Example: 
# # # - Estimate a model without any clusters and demonstrates that the 
# # #   wild cluster bootstrap implemented replicates the expected p-values
# # #   based on sandwich covariance matrices + New Test for truly clustered
# # #   data
# # # - note: the function now works for arbitrary N and number of params k
# # # - also note: the larger the numbers of clusters, the slower the 
# # #   function is. Real speed gains will be achieved if the number of 
# # #   clusters is small
# # # - there is still smth wrong with the p-value for the intercept in 
# # #   the second example
# # # - also effectively I broke the first example
# # # ==================================================================== #
# # 
# # 
# library(data.table)
# library(estimatr)
# library(magrittr)
# library(mvtnorm)
# library(multiwayvcov)
# library(lmtest)
# library(lfe)
# library(pracma)
# library(fabricatr)
# library(rbenchmark)
# library(fixest)
# 
# 
# # 
# N <- 4000
# # 
# x1 <- rnorm(N)
# x2 <- rnorm(N)
# x3 <- rnorm(N)
# x4 <- rnorm(N)
# error <- rnorm(N)
# # 
# y <- 1 + 0.05*x1 - 0.02*x2 + 0.5*x3 + x4 + error
# # 
# data <- data.table(y = y, 
#                  x1 = x1, 
#                  x2 = x2, 
#                  x3 = x3, 
#                  x4 = x4, 
#                  cluster1 = 1:N, 
#                  cluster2 = rep(1:(N/10), 10))
# # 
# # # just heteroskedasticity robust
# lm_fit <- lm(y ~ x1 + x2 + x3 + x4, data = data)
# lm_robust(y ~ x1 + x2 + x3 + x4 , data = data) %>% 
# summary()
# 
# # note that this part is rather slow, as a speed gains mainly for few clusters
# boottest.lm(lm_fit, 1:2000, B = 1000, seed = 1, param = "(Intercept)")
# boottest.lm(lm_fit, 1:2000, B = 1000, seed = 1, param = "x1")
# boottest.lm(lm_fit, 1:2000, B = 1000, seed = 1, param = "x2")
# boottest.lm(lm_fit, 1:2000, B = 1000, seed = 1, param = "x3")
# boottest.lm(lm_fit, 1:2000, B = 1000, seed = 1, param = "x4")
# 
# 
# 
# 
# gen_cluster <- function(param = c(1, -0.005), n = 50000, n_cluster = 20, rho = 0.01) {
#   # source: https://yukiyanai.github.io/teaching/rm1/contents/R/clustered-data-analysis.html
#   # Function to generate clustered data
#   # Required package: mvtnorm
#   
#   # individual level
#   Sigma_i <- matrix(c(1, 0, 0, 1 - rho), ncol = 2)
#   values_i <- rmvnorm(n = n, sigma = Sigma_i)
#   
#   # cluster level
#   cluster_name <- rep(1:n_cluster, each = n / n_cluster)
#   Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
#   values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
#   
#   # predictor var consists of individual- and cluster-level components
#   x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
#   
#   # error consists of individual- and cluster-level components
#   error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
#   
#   # data generating process
#   y <- param[1] + param[2]*x + error
#   
#   df <- data.frame(x, y, cluster = cluster_name)
#   data.table::setDT(df)
#   return(df)
# }
# 
# # Now add an example with actual clustered errors
# 
# B <- 10000
# seed <- 1345671
# set.seed(seed)
# 
# 
# voters <- fabricate(
#   N = 20000,
#   group_id = rep(1:20, 1000),
#   ideology = draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = 0.1),
#   ideological_label = draw_ordered(
#     x = ideology,
#     break_labels = c(
#       "Very Conservative", "Conservative",
#       "Liberal", "Very Liberal"
#     )
#   ),
#   income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
#   Q1_immigration = draw_likert(x = ideology, type = 7),
#   Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
#   treatment = draw_binary(0.5, N = N),
#   proposition_vote = draw_binary(latent = ideology + 0.01 * treatment, link = "probit")
# )
# 
# head(voters)
# 
# #data <- gen_cluster()
# ##head(data)
# #data[, mean(y)]
# setDT(voters)
# voters[, log_income := log(income)]
# voters[, Q1_immigration := as.factor(Q1_immigration) ]
# voters[, Q2_defence := as.factor(Q2_defence)]
# # basic bootstrap, not parallel
# 
# 
# 
# 
# 
# # ------------------------------------------------------------------------------------ #
# # Invert p-value
# 
# lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, data = voters)
# summary(lm_fit)
# #y <- data$y
# #x <- data$x
# tic()
# lm_robust_fit <- lm_robust(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, data = voters, clusters = voters$group_id, se_type = "stata")
# toc()
# summary(lm_robust_fit)
# 
# system.time(
#   res1 <- boottest.lm(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
# )
# system.time(
#   res <- boottest.lm(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
# )
# 
# system.time(
#   boot_fit <- multiwayvcov::cluster.boot(lm_fit, 
#                                                    as.factor(voters$group_id), 
#                                                    R = B, 
#                                                    boot_type = "residual", 
#                                                    wild_type = "rademacher", 
#                                                    parallel = TRUE)
# )
# 
# # ---------------------------------------------------------------------------------------- # 
# # print results: 
# 
# res
# lmtest::coeftest(lm_fit, boot_fit)
# lm_robust_fit %>% 
#   summary()
# 
# 
# 
# # ---------------------------------------------------------------------------------------- #
# # fixed effects
# 
# #library(fixest)
# #tmp <- feols(proposition_vote ~ treatment + ideology + log_income  | Q1_immigration, 
# #             data = voters)
# #summary(tmp)
# #names(summary(tmp))
# 
# X <- as.matrix(voters[, .(treatment, ideology, log_income, proposition_vote)])
# fe <- list(voters[, .(Q1_immigration, Q2_defence)])
# 
# voters1 <- as.data.frame(fixest:::demean(X = X, fe = fe, weights = NULL))
# lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, data = voters)
# lm_fit1 <- lm(proposition_vote ~ treatment + ideology + log_income , data = voters1)
# summary(lm_fit)
# summary(lm_fit1)
# 
# 
# 
# 
# # ---------------------------------------------------------------------------------------------------- #
# # method for lm_robust
# 
# lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income +Q1_immigration + Q2_defence, weights = NULL, data = voters)
# lm_robust_fit <- lm_robust(proposition_vote ~ treatment + ideology + log_income, fixed_effects = ~ Q1_immigration + Q2_defence, weights = NULL, data = voters)
# lm_robust_fit1 <- lm_robust(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, weights = NULL, data = voters )
# feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income, fixef = c("Q1_immigration", "Q2_defence"), weights = NULL, data = voters)
# felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration + Q2_defence, weights = NULL, data = voters)
# 
# 
# 
# #get_model_frame(lm_robust_fit)
# #get_model_fe(lm_robust_fit)
# 
# lm = boottest.lm(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
# estimatr_fe = boottest.lm_robust(lm_robust_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
# estimatr = boottest.lm_robust(lm_robust_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
# felm = boottest.felm(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
# fixest = boottest.fixest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
# 
# tic()
# res_lm = boottest.lm(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
# toc()
# res_estimatr_fe = boottest.lm_robust(lm_robust_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
# res_estimatr = boottest.lm_robust(lm_robust_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
# res_felm = boottest.felm(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
# res_fixest = boottest.fixest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
# 
# summary(res_lm)
# summary(res_estimatr)
# summary(res_estimatr_fe)
# summary(res_felm)
# summary(res_fixest)
# 
# benchmark(
#   lm = boottest.lm(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE), 
#   estimatr_fe = boottest.lm_robust(lm_robust_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE), 
#   estimatr = boottest.lm_robust(lm_robust_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE), 
#   felm = boottest.felm(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE),
#   fixest = boottest.fixest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE),
#   replications = 10
# )
# 
# benchmark(
#   lm = boottest.lm(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE), 
#   estimatr_fe = boottest.lm_robust(lm_robust_fit,clustid = voters$group_id,  B = B, seed = seed, param = "treatment", conf_int = TRUE), 
#   estimatr = boottest.lm_robust(lm_robust_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE), 
#   felm = boottest.felm(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE),
#   fixest = boottest.fixest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE),
#   
#   replications = 10
# )
# 
# 
# 
# 
# lm$p_val
# estimatr$p_val
# estimatr_fe$p_val
# felm$p_val
# fixest$p_val
# 
# lm$conf_int
# estimatr$conf_int
# estimatr_fe$conf_int
# felm$conf_int
# fixest$conf_int












