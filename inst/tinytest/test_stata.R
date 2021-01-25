# test output of stata::boottest vs output of fwildclusterboot::boottest()

library(fwildclusterboot)
library(lfe)
library(fixest)

# run for stata: 
# voters <- fwildclusterboot:::create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
# data.table::fwrite(voters, "C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv")


# ---------------------------------------------------------------------------------------------- # 
# Part 1: one cluster variable - bootcluster = "min"
# ---------------------------------------------------------------------------------------------- # 

set.seed(1234)

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 999999,
    seed = 911,
    param = "treatment",
    conf_int = TRUE,
    sign_level = 0.05
  )
)

boot_lm2 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10
))

boot_lm3 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10, 
      beta0 = 0.05
    ))

boot_lm4 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "norm"
    ))

boot_lm5 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "mammen"
    ))
            
boot_lm6 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "webb"
    ))

boot_lm7 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "webb", 
      impose_null = FALSE
    ))
        


# stata code
# set seed 1
# 
# clear 
# import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
# quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
# 
# boottest treatment, reps(9999) nograph
# boottest treatment, reps(9999) nograph level(90)
# boottest treatment = 0.05, reps(9999) nograph level(90)
# boottest treatment, reps(9999) nograph level(90) weighttype("normal")
# boottest treatment, reps(9999) nograph level(90) weighttype("mammen")
# boottest treatment, reps(9999) nograph level(90) weighttype("webb")
# boottest treatment, reps(9999) nograph level(90) weighttype("webb") nonull


expect_equal(boot_lm1$p_val,  0.3680, tol = 1e-2)
expect_equal(boot_lm2$p_val,  0.3680, tol = 1e-2)
expect_equal(boot_lm3$p_val,  0.1549, tol = 1e-2)
expect_equal(boot_lm4$p_val, 0.3917, tol = 1e-2)
expect_equal(boot_lm5$p_val, 0.3950, tol = 1e-2)
expect_equal(boot_lm6$p_val, 0.3727, tol = 1e-2)
expect_equal(boot_lm7$p_val, 0.3661, tol = 1e-2)


# ---------------------------------------------------------------------------------- # 
# repeat the same but include regression weights

#weights <- sample(1:10, 1000, replace = TRUE) / 10
#all.equal(weights, voters$weights)
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234), 
             weights = weights)

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 999999,
    seed = 911,
    param = "treatment",
    conf_int = TRUE,
    sign_level = 0.05
  )
)

boot_lm2 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10
    ))

boot_lm3 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10, 
      beta0 = 0.05
    ))

boot_lm4 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "norm"
    ))

boot_lm5 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "mammen"
    ))

boot_lm6 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "webb"
    ))

boot_lm7 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 999999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "webb", 
      impose_null = FALSE
    ))



# stata code
# set seed 1
# 
# clear 
# import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
# quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
# 
# boottest treatment, reps(9999) nograph
# boottest treatment, reps(9999) nograph level(90)
# boottest treatment = 0.05, reps(9999) nograph level(90)
# boottest treatment, reps(9999) nograph level(90) weighttype("normal")
# boottest treatment, reps(9999) nograph level(90) weighttype("mammen")
# boottest treatment, reps(9999) nograph level(90) weighttype("webb")
# boottest treatment, reps(9999) nograph level(90) weighttype("webb") nonull

# 
# expect_equal(boot_lm1$p_val,  0.2752, tol = 1e-2)
# expect_equal(boot_lm2$p_val,  0.2757, tol = 1e-2)
# expect_equal(boot_lm3$p_val,  0.3338, tol = 1e-2)
# expect_equal(boot_lm4$p_val,  0.2892, tol = 1e-2)
# expect_equal(boot_lm5$p_val,  0.2915, tol = 1e-2)
# expect_equal(boot_lm6$p_val,  0.2764, tol = 1e-2)
# expect_equal(boot_lm7$p_val, 0.2737, tol = 1e-2)


