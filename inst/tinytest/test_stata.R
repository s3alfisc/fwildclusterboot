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

# library(fwildclusterboot)

#run for stata:
#voters <- fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
#data.table::fwrite(voters, "C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv")


# ---------------------------------------------------------------------------------------------- #
# Part 1: one cluster variable - bootcluster = "min"
# ---------------------------------------------------------------------------------------------- #

set.seed(1234)

data1 <<- fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
#fwrite(data1, "c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv")

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
             data = data1)

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


expect_equal(boot_lm1$p_val,   0.8498, tol = 1e-2)
expect_equal(boot_lm2$p_val,     0.8506, tol = 1e-2)
expect_equal(boot_lm3$p_val,   0.0002, tol = 1e-2)
expect_equal(boot_lm4$p_val,    0.8616, tol = 1e-2)
expect_equal(boot_lm5$p_val,   0.8555, tol = 1e-2)
expect_equal(boot_lm6$p_val,  0.8541, tol = 1e-2)
expect_equal(boot_lm7$p_val,  0.8509, tol = 1e-2)

# no repeat, twoway

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid = c("group_id1", "group_id2"),
    B = 99999,
    seed = 911,
    param = "treatment",
    conf_int = TRUE,
    sign_level = 0.05,
    nthreads = 1
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
      sign_level = 0.10,
      nthreads = 1
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
      beta0 = 0.05,
      nthreads = 1
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
      type = "norm",
      nthreads = 1
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
      type = "mammen",
      nthreads = 1
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
      type = "webb",
      nthreads = 1
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
      impose_null = FALSE,
      nthreads = 1
    ))


expect_equal(boot_lm1$p_val,    0.8112, tol = 1e-2)
expect_equal(boot_lm2$p_val,     0.8079, tol = 1e-2)
expect_equal(boot_lm3$p_val,   0.0046, tol = 1e-2)
expect_equal(boot_lm4$p_val,   0.8125, tol = 1e-2)
expect_equal(boot_lm5$p_val,    0.8119, tol = 1e-2)
expect_equal(boot_lm6$p_val, 0.8091, tol = 1e-2)
expect_equal(boot_lm7$p_val,  0.8107, tol = 1e-2)


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

expect_equal(boot_lm1$p_val,     0.8272, tol = 1e-2)
expect_equal(boot_lm2$p_val,   0.8272, tol = 1e-2)
expect_equal(boot_lm3$p_val,   0.0007, tol = 1e-2)
expect_equal(boot_lm4$p_val,   0.8406, tol = 1e-2)
expect_equal(boot_lm5$p_val,    0.8332, tol = 1e-2)
expect_equal(boot_lm6$p_val,     0.8299, tol = 1e-2)
expect_equal(boot_lm7$p_val,    0.8302, tol = 1e-2)


# weights, twoway


boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid = c("group_id1", "group_id2"),
    B = 99999,
    seed = 911,
    param = "treatment",
    conf_int = TRUE,
    sign_level = 0.05,
    nthreads = 1
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
      sign_level = 0.10,
      nthreads = 1
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
      beta0 = 0.05,
      nthreads = 1
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
      type = "norm",
      nthreads = 1
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
      type = "mammen",
      nthreads = 1
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
      type = "webb",
      nthreads = 1
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
      impose_null = FALSE,
      nthreads = 1
    ))


expect_equal(boot_lm1$p_val,    0.7948, tol = 1e-2)
expect_equal(boot_lm2$p_val,     0.7947, tol = 1e-2)
# decrease tol here
expect_equal(boot_lm3$p_val,      0.0055, tol = 2 * 1e-2)
expect_equal(boot_lm4$p_val,   0.7982, tol = 1e-2)
expect_equal(boot_lm5$p_val,    0.7954, tol = 1e-2)
expect_equal(boot_lm6$p_val,   0.7976, tol = 1e-2)
expect_equal(boot_lm7$p_val,    0.7943, tol = 1e-2)


# ---------------------------------------------------------------------------- #
# Stata code to replicate these results
# /* no weights, one and twoway */
#
# clear
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


# --------------------------------------------------------------------------------------- #
# compare R with stata for full enumeration cases: results should be exactly the same
# (at least for p-values)
# --------------------------------------------------------------------------------------- #

# test 1:
#create a data set with N_G1 = 10 clusters -> full enumeration reached with 2^10 = 1024 bootstrap iterations
voters0 <<- fwildclusterboot:::create_data(N = 1000, N_G1 = 10, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 3)
#data.table::fwrite(voters, "C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv")

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
             data = voters0)

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 9999,
    seed = 911,
    param = "treatment",
    conf_int = TRUE,
    sign_level = 0.05
  )
)

# equal on fourth digit (stata::boottest gives out results with 4 digits after 0)
expect_equal(boot_lm1$p_val,  0.5117, 1e-4)


# test 2:
#create a data set with N_G1 = 10 clusters -> full enumeration reached with 2^10 = 1024 bootstrap iterations
voters1 <<- fwildclusterboot:::create_data(N = 1000, N_G1 = 8, icc1 = 0.2, N_G2 = 5, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 3)
#data.table::fwrite(voters1, "C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv")

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
             data = voters1)

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  "group_id1",
    B = 9999,
    seed = 911,
    param = "treatment",
    conf_int = TRUE,
    sign_level = 0.05
  )
)

# equal on fourth digit (stata::boottest gives out results with 4 digits after 0)
expect_equal(boot_lm1$p_val,  0.1484, 1e-3)

# test 3: twoway
#create a data set with N_G1 = 10 clusters -> full enumeration reached with 2^10 = 1024 bootstrap iterations
voters2 <<- fwildclusterboot:::create_data(N = 1000, N_G1 = 4, icc1 = 0.2, N_G2 = 4, icc2 = 0.41, numb_fe1 = 10, numb_fe2 = 10, seed = 3)
#data.table::fwrite(voters2, "C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv")

lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
             data = voters2)

boot_lm1 <-  suppressWarnings(
  boottest(
    object = lm_fit,
    clustid =  c("group_id1", "group_id2"),
    B = 99999,
    seed = 911,
    param = "treatment",
    conf_int = TRUE,
    sign_level = 0.05
  )
)

# equal on fourth digit (stata::boottest gives out results with 4 digits after 0)
expect_equal(boot_lm1$p_val,   0.8638, 1e-4)






# compare multiple fixed effects with stata
voters4 <<- fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.2, N_G2 = 4, icc2 = 0.41, numb_fe1 = 10, numb_fe2 = 10, seed = 3)
#data.table::fwrite(voters4, "C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv")

feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
             data = voters4)

boot_feols <-  suppressWarnings(
  boottest(
    object = feols_fit,
    clustid =  c("group_id1"),
    fe = "Q1_immigration",
    B = 99999,
    seed = 911,
    param = "treatment",
    conf_int = TRUE,
    sign_level = 0.05
  )
)

# stata p-value
# clear
# import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
# set seed 1
# quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration i.q2_defense
# boottest treatment, reps(99999) cluster(group_id1) nograph
expect_equal(boot_feols$p_val,  0.4727, 1e-2)


}

