# Test 11: p.value initiation fails because p-value = 0.05
# in consequence   crossings <-  (p < alpha) - (p > alpha) gives wrong results
# needs ot be crossings <-  (p <= alpha) - (p > alpha) gives wrong results


library(fwildclusterboot)
seed <- 1234
set.seed(seed)

N <- 1000
voters <- create_data_2(N = N, N_G1 = 40, icc1 = 0.01, N_G2 = 20, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)
lm_fit <- lm(proposition_vote ~ treatment  + log_income + Q1_immigration , weights = NULL, data = voters)

boottest(lm_fit, clustid = "group_id1", B = 1000, param = "treatment")

beta0 = 0
alpha = 0.05
B = 1000
weights = NULL
conf_int = NULL 
debug = FALSE
object = lm_fit
clustid = "group_id1"
param = "treatment"

preprocess <- fwildclusterboot:::preprocess.lm(object = object, 
                         param = param, 
                         clustid = clustid, 
                         beta0 = beta0,
                         alpha = alpha, 
                         seed = seed)

clustid_dims <- preprocess$clustid_dims
point_estimate <- object$coefficients[param]
clustid_fml <- as.formula(paste("~", paste(clustid, collapse = "+")))
res <- fwildclusterboot:::boot_algo2.oneclust(preprocess, boot_iter = B)
vcov <- suppressWarnings(sandwich::vcovCL(object, cluster =  clustid_fml))
coefs <- suppressWarnings(lmtest::coeftest(object, vcov))
se_guess <- coefs[param, "Std. Error"]

res_p_val <- fwildclusterboot:::invert_p_val2.algo_oneclust(object = res, B = B, point_estimate = point_estimate, se_guess = se_guess, clustid = preprocess$clustid, alpha = preprocess$alpha)
