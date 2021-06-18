# ------------------------------------------------------------------ # 
# test for warnings and errors
# ------------------------------------------------------------------ # 

library(fixest)
library(lfe)

# test boottest function arguments for errors 
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                           data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                      data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

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

# rademacher & mammen draws (full enumeration case)
expect_warning(boottest(object = lm_fit, clustid =  "group_id1", B = 9999, seed = 911, param = "treatment", conf_int = TRUE))
expect_warning(boottest(object = feols_fit, clustid = c("group_id1"), B = 9999, seed = 911, param = "treatment", conf_int = TRUE))
expect_warning(boottest(object = felm_fit, clustid =  "group_id1", B = 9999, seed = 911, param = "treatment", conf_int = TRUE))

expect_warning(boottest(object = lm_fit, clustid =  "group_id1", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, type = "mammen"))
expect_warning(boottest(object = feols_fit, clustid = c("group_id1"), B = 9999, seed = 911, param = "treatment", conf_int = TRUE, type = "mammen"))
expect_warning(boottest(object = felm_fit, clustid =  "group_id1", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, type = "mammen"))


# test for banned function arguments and syntax for fixest
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + i(log_income, Q1_immigration), 
                           data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
expect_error(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment1", conf_int = TRUE))

# joint fe != NULL and weights = on
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                           weights = 1:10000/ 10000, 
                           data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,  
                      weights = 1:10000/ 10000, 
                      data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
expect_error(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, fe = "Q1_immigration"))
expect_error(boottest(object = feols_fit, clustid = c("group_id1"), B = 999, seed = 911, param = "treatment", conf_int = TRUE, fe = "Q1_immigration"))

# nthreads < 1

expect_error(boottest(object = lm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      nthreads = -1))

# expect_warning(boottest(object = lm_fit,
#                       clustid =  "group_id1",
#                       B = 999, seed = 911, 
#                       param = "treatment",
#                       conf_int = TRUE, 
#                       nthreads = 20))
# Warning: In boottest.lm(object = lm_fit, clustid = "group_id1...:
# Asked for 20 threads while the maximum is 8. Set to 8 threads instead.
# will probably not run on cran, as max 2 cores

expect_error(boottest(object = feols_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      nthreads = -1))
# expect_warning(boottest(object = feols_fit,
#                       clustid =  "group_id1",
#                       B = 999, seed = 911, 
#                       param = "treatment",
#                       conf_int = TRUE, 
#                       nthreads = 20))

expect_error(boottest(object = felm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      nthreads = -1))

# expect_warning(boottest(object = felm_fit,
#                       clustid =  "group_id1",
#                       B = 999, seed = 911, 
#                       param = "treatment",
#                       conf_int = TRUE, 
#                       nthreads = 20))

# maxiter 
expect_error(boottest(object = lm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      maxiter = -1))
expect_error(boottest(object = lm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      maxiter = 0.1))

expect_error(boottest(object = feols_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      maxiter = -1))
expect_error(boottest(object = feols_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      maxiter = 0.1))

expect_error(boottest(object = felm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      maxiter = -1))
expect_error(boottest(object = felm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      maxiter = 0.1))


# tol
expect_error(boottest(object = lm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      tol = -1))

expect_error(boottest(object = feols_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      tol = -1))

expect_error(boottest(object = felm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      tol = -1))

# p-val type
expect_error(boottest(object = lm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      p_val_type = ">"))

expect_error(boottest(object = feols_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      p_val_type = ">"))

expect_error(boottest(object = felm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE, 
                      p_val_type = ">"))

# B = 1000
expect_message(boottest(object = lm_fit,
                      clustid =  "group_id1",
                      B = 1000, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE))

expect_message(boottest(object = feols_fit,
                      clustid =  "group_id1",
                      B = 1000, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE))

expect_message(boottest(object = felm_fit,
                      clustid =  "group_id1",
                      B = 1000, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE))

# banned function arguments 


# 1) felm 
felm_fit <- lfe::felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                      data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234), 
                      subset = sample(c(TRUE, FALSE), 1000, TRUE))
expect_error(boottest(object = felm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE))

# 2) lm 
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234), 
             subset = sample(c(TRUE, FALSE), 1000, TRUE))
expect_error(boottest(object = lm_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE))

# fixest
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + i(log_income,Q1_immigration), 
                           data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
expect_error(boottest(object = feols_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE))
feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1, 
                           data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234), 
                           subset = sample(c(TRUE, FALSE), 1000, TRUE))
expect_error(boottest(object = feols_fit,
                      clustid =  "group_id1",
                      B = 999, seed = 911, 
                      param = "treatment",
                      conf_int = TRUE))


# evalute dots ... in methods
# write sig_level instead of sign_level
expect_error(boottest(object = lm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                      sig_level = 0.1))
expect_error(boottest(object = feols_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                        sig_level = 0.1))
expect_error(boottest(object = felm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
                        sig_level = 0.1))

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

res <- boottest(lm_fit, clustid =  "group_id1", B = 999, seed = 911, param = "treatment", conf_int = TRUE, 
         sign_level = 0.1)

expect_error(summary(res, a = 1))
#expect_error(tidy(res, a = 1))
expect_error(plot(res, a = 1))




# if 2^(number of clusters) < B and rademacher or mammen weights are used, boottest() switches 
# to full enumeration. In consequence, only 2^(number of clusters - 1) unique t statistics can be computed (see Webb, "Reworking wild bootstrap based inference for clustered errors", 2013)
# This will cause trouble for the inversion of p-values, for two reasons: a) the p-value function will not 
# be sufficiently smooth b) no appropriate starting value for the root finding procedure will be found
# this set of tests checks if boottest() throws an error in the part of the code that is responsible for 
# calculating p-values

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 100, N_G1 = 4, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1))

# no confidence intervals calculated: expect warning
expect_warning(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # guarantees that full enumeration is employed
    B = 2^4 + 1, 
    seed = 1, 
    param = "treatment", 
    type = "rademacher",
    conf_int = FALSE)
)

# no confidence intervals calculated: expect warning
expect_warning(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # guarantees that full enumeration is employed
    B = 2^4 + 1, 
    seed = 1, 
    param = "treatment", 
    type = "mammen",
    conf_int = FALSE)
)

# with confidence intervals: expect_error because B < 100
expect_error(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # guarantees that full enumeration is employed
    B = 2^4 + 1, 
    seed = 1, 
    param = "treatment", 
    type = "rademacher",
    conf_int = TRUE)
)

# with confidence intervals: expect_error because B < 100
expect_error(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # guarantees that full enumeration is employed
    B = 2^z + 1, 
    seed = 1, 
    param = "treatment", 
    type = "mammen",
    conf_int = TRUE)
)




# ------------------------------------------------------------------------- #
# NA values in the cluster variables 
# ------------------------------------------------------------------------- #

data <-  fwildclusterboot:::create_data(N = 100, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1)
data[1, "group_id1"] <- NA
data2 <<- data

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = data2)

# expect error as na_omit = FALSE & missing variable in group_id1 (the cluster variable)
expect_error(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # guarantees that full enumeration is employed
    B = 999, 
    seed = 1, 
    param = "treatment", 
    type = "rademacher",
    conf_int = TRUE, 
    na_omit = FALSE)
)

expect_warning(
  res <- 
    boottest(
  object = lm_fit, 
  clustid =  "group_id1", 
  # guarantees that full enumeration is employed
  B = 999, 
  seed = 1, 
  param = "treatment", 
  type = "rademacher",
  conf_int = TRUE, 
  na_omit = TRUE)
)
expect_equal(res$N, 99)

data[2, "group_id1"] <- NA
data3 <<- data
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = data3)
expect_warning(
  res <- 
    boottest(
      object = lm_fit, 
      clustid =  "group_id1", 
      # guarantees that full enumeration is employed
      B = 999, 
      seed = 1, 
      param = "treatment", 
      type = "rademacher",
      conf_int = TRUE, 
      na_omit = TRUE)
)
expect_equal(res$N, 98)

