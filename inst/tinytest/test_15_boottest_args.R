# # test boottest args alpha, type
# 
# lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
#              data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# 
# # increase alphas
# alphas <- c(0.01, 0.05, 0.1, 0.2, 0.5, 0.9)
# res <- 
# lapply(alphas, function(x){
#   tmp <- boottest(object = lm_fit, clustid =  "group_id1", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, alpha = x)
#   tidy(tmp)
# })
# 
# lower <- data.table::rbindlist(res)$`CI Lower`
# upper <- data.table::rbindlist(res)$`CI Upper`
# 
# expect_equal(sort(lower), lower)
# expect_equal(sort(upper, decreasing = TRUE), upper)
# 
# 
# # try out other distributions
# dists <- c("rademacher", "mammen", "norm","webb")
# res <- 
#   lapply(dists, function(x){
#     tmp <- boottest(object = lm_fit, clustid =  "group_id1", B = 99999, seed = 911, param = "treatment", conf_int = TRUE, type = x)
#     tidy(tmp)
#   })
# 
# p_vals <- (data.table::rbindlist(res)$`Pr(>|t|)`)
# 
# # expect_equal(p_vals[1], p_vals[2], tol = 1e-1)
# # expect_equal(p_vals[2], p_vals[3], tol = 1e-1)
# # expect_equal(p_vals[3], p_vals[4], tol = 1e-1)
# 
# 
# 
# # feols
# feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
#                            data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# # increase alphas
# alphas <- c(0.01, 0.05, 0.1, 0.2, 0.5, 0.9)
# res <- 
#   lapply(alphas, function(x){
#     tmp <- boottest(object = feols_fit, clustid =  "group_id1", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, alpha = x)
#     tidy(tmp)
#   })
# 
# lower <- data.table::rbindlist(res)$`CI Lower`
# upper <- data.table::rbindlist(res)$`CI Upper`
# 
# expect_equal(sort(lower), lower)
# expect_equal(sort(upper, decreasing = TRUE), upper)
# 
# 
# # try out other distributions
# dists <- c("rademacher", "mammen", "norm","webb")
# res <- 
#   lapply(dists, function(x){
#     tmp <- boottest(object = feols_fit, clustid =  "group_id1", B = 99999, seed = 911, param = "treatment", conf_int = TRUE, type = x)
#     tidy(tmp)
#   })
# 
# p_vals <- (data.table::rbindlist(res)$`Pr(>|t|)`)

# expect_equal(p_vals[1], p_vals[2], tol = 1e-1)
# expect_equal(p_vals[2], p_vals[3], tol = 1e-1)
# expect_equal(p_vals[3], p_vals[4], tol = 1e-1)
