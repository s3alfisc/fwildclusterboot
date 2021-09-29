# test consistency of "multivariable" test implementation
# 1) test if boottest() produces equivalent results no matter the order of the "param" input vector
# 2) test if equivalent hypotheses produce the same results: e.g. var1 + var2 = 1
#    should be equivalent to 2*var1 + 2*var2 = 2.


# 1) order of param

# create the test data set and save it on disk
set.seed(1)
data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
             data = data1)


boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
    seed = 911,
    param = c("treatment", "ideology1"),
    R = c(0.21, 0.2),
    beta0 = 0.02,
    conf_int = FALSE,
    sign_level = 0.05
    )
)

# Test 1: one cluster variable
boot_lm2 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
    seed = 911,
    param = c("ideology1", "treatment"),
    R = c(0.2, 0.21),
    beta0 = 0.02,
    conf_int = FALSE,
    sign_level = 0.05
    )
)

expect_equivalent(boot_lm1$p_val, boot_lm2$p_val)
expect_equivalent(boot_lm1$conf_int, boot_lm2$conf_int)



# Test 2) check "equivalent" hypotheses

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
    seed = 911,
    param = c("treatment", "ideology1"),
    R = c(1, 1),
    beta0 = 0,
    conf_int = FALSE,
    sign_level = 0.05
  )
)

# Test 1: one cluster variable
boot_lm2 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
    seed = 911,
    param = c("ideology1", "treatment"),
    R = c(2, 2),
    beta0 = 0,
    conf_int = FALSE,
    sign_level = 0.05
  )
)

expect_equivalent(boot_lm1$p_val, boot_lm2$p_val)
expect_equivalent(boot_lm1$conf_int, boot_lm2$conf_int)



boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
    seed = 911,
    param = c("treatment", "ideology1"),
    R = c(1, 1),
    beta0 = 0.01,
    conf_int = FALSE,
    sign_level = 0.05
  )
)

# Test 1: one cluster variable
boot_lm2 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
    seed = 911,
    param = c("ideology1", "treatment"),
    R = c(2, 2),
    beta0 = 0.02,
    conf_int = FALSE,
    sign_level = 0.05
  )
)

expect_equivalent(boot_lm1$p_val, boot_lm2$p_val)
expect_equivalent(boot_lm1$conf_int, boot_lm2$conf_int)


boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
    seed = 911,
    param = c("treatment", "ideology1"),
    R = c(0.99, 1),
    beta0 = 0.01,
    conf_int = FALSE,
    sign_level = 0.05
  )
)

# Test 1: one cluster variable
boot_lm2 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
    seed = 911,
    param = c("ideology1", "treatment"),
    R = c(2, 2),
    beta0 = 0.02,
    conf_int = FALSE,
    sign_level = 0.05
  )
)

expect_false(boot_lm1$p_val - boot_lm2$p_val == 0)
