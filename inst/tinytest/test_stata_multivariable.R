# This sequence of tests is equivalent to test_stata.R but focues on 
# multivariable hypotheses. Instead of testing treatment = 0, it tests 
# 0.8*treatment + 0.2*ideology1 = -0.01

# This sequence of tests compares output from fwildclusterboot::boottest 
# with the stata boottest package. 
# In order to run the code, both Stata and the RStata package need to be installed. 
# Therefore, these tests are only run locally and not on CRAN. 
# To get the RStata package to run, see https://github.com/lbraglia/RStata.

run_tests <- length(strsplit(packageDescription("fwildclusterboot")$Version, "\\.")[[1]]) > 3
#run_tests <- FALSE

# to execute all tests, 
# set run_tests <- TRUE & save the file & run 
# tinytest::run_test_file("C:/Users/alexa/Dropbox/fwildclusterboot/inst/tinytest/test_stata_multivariable.R")
# where you need to replace my local path with yours

if(run_tests){
  
  library(fwildclusterboot)
  library(data.table)
  library(tinytest)
  library(RStata)
  
  options("RStata.StataVersion" = 16)
  #chooseStataBin()
  options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataIC-64\"")
  
  # for uni-variable and multivariable hypotheses
  # Test Organization: 
  # 3 x 4 Sets of Tests: 
  # Test Set 1 tests equality of p-value of stata.boottest vs fwildclusterboot.boottest
  # Test Set 2 tests equality of confidence intervals of stata.boottest vs fwildclusterboot.boottest
  # Test Set 3 tests EXACT equality of confidence intervals - under full enum., p-values should be identical even for 
  # a small number of obs N and bootstrap iterations B as there is no sampling uncertainty
  # Test Subset A: oneway clustering
  # Test Subset B: twoway clustering
  # Test Subset C: weighted least squares & oneway clustering
  # Test Subset D: weighted least squares & twoway clustering
  # all tests are run with B = 99999 bootstrap iterations
  # the default relative tolerance of the tests is 0.005
  
  set.seed(1)
  
  nthreads <- 8
  tol <- 2 * 0.01 
  save_test_data_to <- "c:/Users/alexa/Dropbox/fwildclusterboot/"
  save_data <- paste0(save_test_data_to, "voters.csv")
  
  
  # -------------------------------------------------------- #
  # -------------------------------------------------------- #
  # Tests Set 1: test equality of p-values  
  # -------------------------------------------------------- #
  # -------------------------------------------------------- #
  
  
  # -------------------------------------------------------- #
  # Tests Set A:  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 15, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1)
  
  
  # Test 1: one cluster variable
  
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = FALSE,
      sign_level = 0.05, 
      nthreads = 4
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm1$p_val, stata_p_val, tol)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10, 
        nthreads = 4
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm2$p_val,  stata_p_val, tol)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 =  0.05, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = FALSE,
        sign_level = 0.10,
        beta0 = 0.05, 
        nthreads = 4
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm3$p_val,  stata_p_val, tol)
  
  # B 
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = 0.05, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = 0.05,
        conf_int = FALSE,
        sign_level = 0.10, 
        nthreads = 4
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm3$p_val,  stata_p_val, tol)
  
  # C
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01 , reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10, 
        nthreads = 4
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm3$p_val,  stata_p_val, tol)
  
  # Test 4
  
  boot_lm4 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "norm", 
        nthreads = 4
      ))
  
  test_4 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(normal)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_4, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm4$p_val,  stata_p_val, tol)
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 999999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "mammen", 
        nthreads = 4
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(999999) cluster(group_id1 ) nograph level(90) weighttype(mammen)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm5$p_val,  stata_p_val, tol)
  
  
  # Test 6
  
  boot_lm6 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "webb", 
        nthreads = 4
      ))
  
  test_6 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(webb)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_6, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm6$p_val,  stata_p_val, tol) # fails at tol
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 199999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "webb",
        impose_null = FALSE, 
        nthreads = 4
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(199999) cluster(group_id1 ) nograph level(90) weighttype(webb) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm7$p_val,  stata_p_val, tol) # fails at tol = 0.01
  
  
  # -------------------------------------------------------- #
  # Tests Set B (twoway clustering):  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 5000, N_G1 = 5, icc1 = 0.01, N_G2 = 5, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1)
  
  # Test 1: 
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid = c("group_id1", "group_id2"),
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = FALSE,
      sign_level = 0.05, 
      nthreads = 4
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm1$p_val, stata_p_val, tol) # fails at tol = 0.01
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10, 
        nthreads = 4
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm2$p_val,  stata_p_val, tol) # fails at tol = 0.01
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 group_id2) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = FALSE,
        sign_level = 0.10,
        beta0 = 0.05, 
        nthreads = 4
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm3$p_val,  stata_p_val, tol)
  
  
  # Test 4
  
  boot_lm4 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "norm", 
        nthreads = 4
      ))
  
  test_4 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(normal)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_4, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm4$p_val,  stata_p_val, tol)
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "mammen", 
        nthreads = 4
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(mammen)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm5$p_val,  stata_p_val, tol)  # fails at tol = 0.01
  
  
  # Test 6
  
  boot_lm6 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "webb", 
        nthreads = 4
      ))
  
  test_6 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(webb)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_6, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm6$p_val,  stata_p_val, tol = 2* tol)  # fails at tol = 0.01
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "webb",
        impose_null = FALSE, 
        nthreads = 4
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(webb) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm7$p_val,  stata_p_val, tol)  # fails at tol = 0.01
  
  
  # -------------------------------------------------------- #
  # Tests Set C (same as A, but with weights):  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk

  data1 <<- fwildclusterboot:::create_data(N = 5000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1, 
               weights = weights)
  
  
  # Test 1: one cluster variable
  
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = FALSE,
      sign_level = 0.05
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm1$p_val, stata_p_val, tol)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm2$p_val,  stata_p_val, tol)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = FALSE,
        sign_level = 0.10,
        beta0 = 0.05
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm3$p_val,  stata_p_val, tol)
  
  
  # Test 4
  
  boot_lm4 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "norm"
      ))
  
  test_4 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(normal)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_4, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm4$p_val,  stata_p_val, tol)
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 299999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "mammen"
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(299999) cluster(group_id1 ) nograph level(90) weighttype(mammen)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm5$p_val,  stata_p_val, tol)  # fails at tol = 0.01
  
  
  # Test 6
  
  boot_lm6 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "webb"
      ))
  
  test_6 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(webb)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_6, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm6$p_val,  stata_p_val, tol)
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 999999,
        seed = 921,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "webb",
        impose_null = FALSE
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(999999) cluster(group_id1 ) nograph level(90) weighttype(webb) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm7$p_val,  stata_p_val, tol)
  
  
  # -------------------------------------------------------- #
  # Tests Set D (twoway clustering, same as B but with weights):  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 5, icc1 = 0.01, N_G2 = 5, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1, 
               weights = weights)
  
  # Test 1: 
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid = c("group_id1", "group_id2"),
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = FALSE,
      sign_level = 0.05, 
      nthreads = 4
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm1$p_val, stata_p_val, tol)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10, 
        nthreads = 4
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration  [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm2$p_val,  stata_p_val, tol)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration  [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 group_id2) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = FALSE,
        sign_level = 0.10,
        beta0 = 0.05, 
        nthreads = 4
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm3$p_val,  stata_p_val, tol)
  
  
  # Test 4
  
  boot_lm4 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "norm", 
        nthreads = 4
      ))
  
  test_4 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(normal)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_4, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm4$p_val,  stata_p_val, tol = 2*tol)
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "mammen", 
        nthreads = 4
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(mammen)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm5$p_val,  stata_p_val, tol)
  
  
  # Test 6
  
  boot_lm6 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "webb", 
        nthreads = 4
      ))
  
  test_6 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(webb)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_6, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm6$p_val,  stata_p_val, tol = 3 * tol)  # fails at tol = 0.01
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "rademacher",
        impose_null = FALSE, 
        nthreads = 4
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(rademacher) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm7$p_val,  stata_p_val, tol)
  
  
  
  
  
  
  # -------------------------------------------------------- #
  # -------------------------------------------------------- #
  # Tests Set 2: test equality of confidence intervals
  # -------------------------------------------------------- #
  # -------------------------------------------------------- #
  
  
  # -------------------------------------------------------- #
  # Tests Set A:  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 15, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1)
  
  
  # Test 1: one cluster variable
  
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = TRUE,
      sign_level = 0.05
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm1$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm1$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm2$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm2$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 ) nograph level(90)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = TRUE,
        sign_level = 0.10,
        beta0 = 0.05
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm3$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm3$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 4
  
  boot_lm4 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "norm"
      ))
  
  test_4 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(normal)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_4, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm4$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm4$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "mammen"
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(mammen)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm5$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm5$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 6
  
  boot_lm6 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "webb"
      ))
  
  test_6 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(webb)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_6, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm6$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm6$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "webb",
        impose_null = FALSE
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(webb) nonull
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm7$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm7$conf_int[2], stata_conf_int_u, tol)
  
  
  # -------------------------------------------------------- #
  # Tests Set B (twoway clustering):  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 5, icc1 = 0.01, N_G2 = 5, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1)
  
  # Test 1: 
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid = c("group_id1", "group_id2"),
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = TRUE,
      sign_level = 0.05, 
      nthreads = 4
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm1$conf_int[1], stata_conf_int_l, tol)  # fails at tol = 0.01
  expect_equal(boot_lm1$conf_int[2], stata_conf_int_u, tol) # fails at tol = 0.01
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10, 
        nthreads = 4
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm2$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm2$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 group_id2) nograph level(90)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = TRUE,
        sign_level = 0.10,
        beta0 = 0.05, 
        nthreads = 4
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm3$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm3$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 4
  
  boot_lm4 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "norm", 
        nthreads = 4
      ))
  
  test_4 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(normal)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_4, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm4$conf_int[1], stata_conf_int_l, tol)  # fails at tol = 0.01
  expect_equal(boot_lm4$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "mammen", 
        nthreads = 4
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(mammen)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm5$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm5$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 6
  
  boot_lm6 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "webb", 
        nthreads = 4
      ))
  
  test_6 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(webb)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_6, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm6$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm6$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "webb",
        impose_null = FALSE, 
        nthreads = 4
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(webb) nonull
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm7$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm7$conf_int[2], stata_conf_int_u, tol)
  
  
  # -------------------------------------------------------- #
  # Tests Set C (same as A, but with weights):  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 15, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1, 
               weights = weights)
  
  
  # Test 1: one cluster variable
  
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = TRUE,
      sign_level = 0.05
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm1$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm1$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm2$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm2$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 ) nograph level(90)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = TRUE,
        sign_level = 0.10,
        beta0 = 0.05
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm3$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm3$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 4
  
  boot_lm4 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "norm"
      ))
  
  test_4 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(normal)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_4, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm4$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm4$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "mammen"
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(mammen)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm5$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm5$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 6
  
  boot_lm6 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "webb"
      ))
  
  test_6 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(webb)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_6, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm6$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm6$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "webb",
        impose_null = FALSE
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(webb) nonull
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm7$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm7$conf_int[2], stata_conf_int_u, tol)
  
  
  # -------------------------------------------------------- #
  # Tests Set D (twoway clustering, same as B but with weights):  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 5, icc1 = 0.01, N_G2 = 5, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1, 
               weights = weights)
  
  # Test 1: 
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid = c("group_id1", "group_id2"),
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = TRUE,
      sign_level = 0.05, 
      nthreads = 4
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm1$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm1$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10, 
        nthreads = 4
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration  [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm2$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm2$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration  [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 group_id2) nograph level(90)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = TRUE,
        sign_level = 0.10,
        beta0 = 0.05, 
        nthreads = 4
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm3$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm3$conf_int[2], stata_conf_int_u, tol)  # fails at tol = 0.01
  
  
  # Test 4
  
  boot_lm4 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "norm", 
        nthreads = 4
      ))
  
  test_4 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(normal)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_4, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm4$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm4$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "mammen", 
        nthreads = 4
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(mammen)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm5$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm5$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 6
  
  boot_lm6 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "webb", 
        nthreads = 4
      ))
  
  test_6 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(webb)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_6, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm6$conf_int[1], stata_conf_int_l, tol)  # fails at tol = 0.01
  expect_equal(boot_lm6$conf_int[2], stata_conf_int_u, tol)
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid = c("group_id1", "group_id2"),
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = TRUE,
        sign_level = 0.10,
        type = "webb",
        impose_null = FALSE, 
        nthreads = 4
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights]
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 group_id2) nograph level(90) weighttype(webb) nonull
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_conf_int_l <- unique(res$conf_int_l)
  stata_conf_int_u <- unique(res$conf_int_u)
  
  expect_equal(boot_lm7$conf_int[1], stata_conf_int_l, tol)
  expect_equal(boot_lm7$conf_int[2], stata_conf_int_u, tol)
  
  
  
  
  # -------------------------------------------------------- #
  # -------------------------------------------------------- #
  # Tests Set 3: test EXACT equality of p-values under full 
  # enumeration 
  # -------------------------------------------------------- #
  # -------------------------------------------------------- #
  
  # note: exact equality tests might cause errors on cran -> set
  # relative error tolerance to 1e-04
  tol <- 1e-04
  
  # -------------------------------------------------------- #
  # Tests Set A:  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 2000, N_G1 = 8, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1)
  
  
  # Test 1: one cluster variable
  
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = FALSE,
      sign_level = 0.05
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm1$p_val, stata_p_val)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm2$p_val,  stata_p_val)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = FALSE,
        sign_level = 0.10,
        beta0 = 0.05
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm3$p_val,  stata_p_val)
  
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "rademacher",
        impose_null = FALSE
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration, cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(rademacher) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm7$p_val,  stata_p_val)
  


  # -------------------------------------------------------- #
  # Tests Set C (same as A, but with weights):  
  # -------------------------------------------------------- #
  
  # create the test data set and save it on disk
  data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 8, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration ,
               data = data1, 
               weights = weights)
  
  
  # Test 1: one cluster variable
  
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      B = 99999,
      seed = 911,
      param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
      conf_int = FALSE,
      sign_level = 0.05
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm1$p_val, stata_p_val)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm2$p_val,  stata_p_val)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1  = 0.05, reps(99999) cluster(group_id1 ) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), 
        conf_int = FALSE,
        sign_level = 0.10,
        beta0 = 0.05
      ))
  
  res <- RStata::stata(test_3, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm3$p_val,  stata_p_val)
  
  
  # Test 4
  
  
  
  # Test 5
  
  boot_lm5 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "rademacher"
      ))
  
  test_5 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(rademacher)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_5, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm5$p_val,  stata_p_val)

  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        B = 99999,
        seed = 911,
        param = c("treatment", "ideology1"), R = c(0.8, 0.2), beta0 = -0.01,
        conf_int = FALSE,
        sign_level = 0.10,
        type = "rademacher",
        impose_null = FALSE
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration [pweight = weights], cluster(group_id1)
boottest 0.8*treatment + 0.2*ideology1 = -0.01, reps(99999) cluster(group_id1 ) nograph level(90) weighttype(rademacher) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_7, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm7$p_val,  stata_p_val)
  
  
  
  
}
