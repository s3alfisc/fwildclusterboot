# tests seed behavior: 
# Test 1: different seeds should lead to different inferences when calling boottest() multiple 
#         times on the same object
# Test 2: by default, if no seed is provided, boottest sets its internal seed to null - hence calling
#         boottest() multiple times on the same object will generate the same test statistics


# Test 1
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 100, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1))

# Case 1: no full enumeration - results will be different
boot_lm_s1 <-  suppressMessages(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # no full enumeration (N_G = 10, 2^10 = 1024)
    B = 999, 
    seed = 1, 
    param = "treatment", 
    type = "rademacher",
    conf_int = FALSE)
)

boot_lm_s2 <-  suppressMessages(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # no full enumeration (N_G = 10, 2^10 = 1024)
    B = 999, 
    seed = 2, 
    param = "treatment", 
    type = "rademacher",
    conf_int = FALSE)
)

expect_true(boot_lm_s1$p_val != boot_lm_s2$p_val)

# Case 2: full enumeration - results will be different
boot_lm_s1 <-  suppressMessages(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    #  full enumeration (N_G = 10, 2^10 = 1024)
    B = 1025, 
    seed = 1, 
    param = "treatment", 
    type = "rademacher",
    conf_int = FALSE)
)

boot_lm_s2 <-  suppressMessages(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # no full enumeration (N_G = 10, 2^10 = 1024)
    B = 1025, 
    seed = 2, 
    param = "treatment", 
    type = "rademacher",
    conf_int = FALSE)
)

expect_true(boot_lm_s1$p_val == boot_lm_s2$p_val)




# Test 2
# no seed provided - results should be the same
boot_lm_s1 <-  suppressMessages(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # no full enumeration (N_G = 10, 2^10 = 1024)
    B = 999, 
    param = "treatment", 
    type = "rademacher",
    conf_int = FALSE)
)

boot_lm_s2 <-  suppressMessages(
  boottest(
    object = lm_fit, 
    clustid =  "group_id1", 
    # no full enumeration (N_G = 10, 2^10 = 1024)
    B = 999, 
    param = "treatment", 
    type = "rademacher",
    conf_int = FALSE)
)

expect_true(boot_lm_s1$p_val != boot_lm_s2$p_val)

