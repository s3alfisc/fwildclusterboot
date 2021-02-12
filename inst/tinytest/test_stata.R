## force tests to be executed if in dev release which we define as
## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
## code taken from https://stackoverflow.com/questions/36166288/skip-tests-on-cran-but-run-locally
## answer provided by user Dirk Eddelbuettel & edited by Anirban166
if (length(strsplit(packageDescription("fwildclusterboot")$Version, "\\.")[[1]]) > 3) { 
  #Sys.setenv("RunAllfwildclusterbootTests"="yes")
  runThisTest <- TRUE
} else {
  runThisTest <- FALSE
}

#.runThisTest <- Sys.getenv("RunAllfwildclusterbootTests") == "yes"

if (runThisTest) {
# skip data tests if not development version x.x.x.x

# test output of stata::boottest vs output of fwildclusterboot::boottest()

library(fwildclusterboot)
library(lfe)
library(fixest)

#run for stata: 
#voters <- fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
#data.table::fwrite(voters, "C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv")


# ---------------------------------------------------------------------------------------------- # 
# Part 1: one cluster variable - bootcluster = "min"
# ---------------------------------------------------------------------------------------------- # 

set.seed(1234)

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
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
      B = 99999,
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
      B = 99999,
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
      B = 99999,
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
      B = 99999,
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
      B = 99999,
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
      B = 99999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "webb", 
      impose_null = FALSE
    ))
        

expect_equal(boot_lm1$p_val,   0.7121, tol = 1e-2)
expect_equal(boot_lm2$p_val,    0.7111, tol = 1e-2)
expect_equal(boot_lm3$p_val,   0.0003, tol = 1e-2)
expect_equal(boot_lm4$p_val,   0.7376, tol = 1e-2)
expect_equal(boot_lm5$p_val,  0.7162, tol = 1e-2)
expect_equal(boot_lm6$p_val,  0.7125, tol = 1e-2)
expect_equal(boot_lm7$p_val,  0.7163, tol = 1e-2)

# no repeat, twoway

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid = c("group_id1", "group_id2"),
    B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10
    ))

boot_lm3 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid = c("group_id1", "group_id2"),
      B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "webb", 
      impose_null = FALSE
    ))


expect_equal(boot_lm1$p_val,    0.6799, tol = 1e-2)
expect_equal(boot_lm2$p_val,    0.6815, tol = 1e-2)
expect_equal(boot_lm3$p_val,   0.0087, tol = 1e-2)
expect_equal(boot_lm4$p_val,   0.6852, tol = 1e-2)
expect_equal(boot_lm5$p_val,   0.6856, tol = 1e-2)
expect_equal(boot_lm6$p_val, 0.6840, tol = 1e-2)
expect_equal(boot_lm7$p_val,  0.6797, tol = 1e-2)


# ---------------------------------------------------------------------------------- # 
# repeat the same but include regression weights

#weights <- sample(1:10, 1000, replace = TRUE) / 10
#all.equal(weights, voters$weights)
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234), 
             weights = weights)

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 99999,
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
      B = 99999,
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
      B = 99999,
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
      B = 99999,
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
      B = 99999,
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
      B = 99999,
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
      B = 99999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "webb", 
      impose_null = FALSE
    ))

expect_equal(boot_lm1$p_val,  0.3970, tol = 1e-2)
expect_equal(boot_lm2$p_val,   0.3965, tol = 1e-2)
expect_equal(boot_lm3$p_val,   0.0086, tol = 1e-2)
expect_equal(boot_lm4$p_val,  0.4249, tol = 1e-2)
expect_equal(boot_lm5$p_val,   0.4014, tol = 1e-2)
expect_equal(boot_lm6$p_val,    0.3997, tol = 1e-2)
expect_equal(boot_lm7$p_val,   0.4063, tol = 1e-2)


# weights, twoway


boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid = c("group_id1", "group_id2"),
    B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10
    ))

# increase B here to 299999, as results deviate
boot_lm3 <-
  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid = c("group_id1", "group_id2"),
      B = 299999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
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
      clustid = c("group_id1", "group_id2"),
      B = 99999,
      seed = 911,
      param = "treatment",
      conf_int = TRUE,
      sign_level = 0.10,
      type = "webb", 
      impose_null = FALSE
    ))


expect_equal(boot_lm1$p_val,   0.2656, tol = 1e-2)
expect_equal(boot_lm2$p_val,    0.2682, tol = 1e-2)
# decrease tol here
expect_equal(boot_lm3$p_val,    0.0138, tol = 2 * 1e-2)
expect_equal(boot_lm4$p_val,  0.2704, tol = 1e-2)
expect_equal(boot_lm5$p_val,    0.2674, tol = 1e-2)
expect_equal(boot_lm6$p_val,   0.2672, tol = 1e-2)
expect_equal(boot_lm7$p_val,   0.2648, tol = 1e-2)




# ---------------------------------------------------------------------------- # 
# Stata code 
# /* no weights, one and twoway */ 
#   
#   clear 
# import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
# 
# set seed 1 
# 
# quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
# 
# /* oneway */ 
#   boottest treatment, reps(99999) cluster(group_id1 ) nograph
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90)
# boottest treatment = 0.05, reps(99999) cluster(group_id1 ) nograph level(90)
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90) weighttype("normal")
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90) weighttype("mammen")
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90) weighttype("webb")
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90) weighttype("webb") nonull
# /* twoway */
#   boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90)
# boottest treatment = 0.05, reps(99999) cluster(group_id1 group_id2) nograph level(90)
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype("normal")
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype("mammen")
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype("webb")
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype("webb") nonull
# 
# 
# /* turn on weights */
#   quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
# 
# /* oneway */ 
#   boottest treatment, reps(99999) cluster(group_id1 ) nograph
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90)
# boottest treatment = 0.05, reps(99999) cluster(group_id1 ) nograph level(90)
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90) weighttype("normal")
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90) weighttype("mammen")
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90) weighttype("webb")
# boottest treatment, reps(99999) cluster(group_id1 ) nograph level(90) weighttype("webb") nonull
# /* twoway */
#   boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90)
# * increase B = 299.999 because results differ
# boottest treatment = 0.05, reps(299999) cluster(group_id1 group_id2) nograph level(90)
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype("normal")
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype("mammen")
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype("webb")
# boottest treatment, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype("webb") nonull

}

