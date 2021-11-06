# compare fwildclusterboot with stata, different bootcluster variables
# not run at the moment - bug in boottest stata

run_tests <- length(strsplit(packageDescription("fwildclusterboot")$Version, "\\.")[[1]]) > 3
run_tests <- FALSE

if(run_tests){
  
  library(fwildclusterboot)
  library(data.table)
  library(tinytest)
  library(RStata)
  
  options("RStata.StataVersion" = 16)
  #chooseStataBin()
  options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataIC-64\"")
  
  
  tol <- 0.01 
  save_test_data_to <- "c:/Users/alexa/Dropbox/fwildclusterboot/"
  save_data <- paste0(save_test_data_to, "voters.csv")
  
  # create the test data set and save it on disk
  set.seed(2)
  data1 <<- fwildclusterboot:::create_data(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
               data = data1)
  
  
  
  # -------------------------------------------------------- #
  # Tests Set A:  
  # -------------------------------------------------------- #

  
  # Test 1: one cluster variable
  
  boot_lm1 <-  suppressWarnings(
    boottest(
      object = lm_fit,
      clustid =  "group_id1",
      bootcluster = c("group_id1", "Q1_immigration"),
      B = 99999,
      seed = 911,
      param = "treatment",
      conf_int = FALSE,
      sign_level = 0.05,        nthreads = 4
    )
  )
  
  test_1 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration i.q2_defense, cluster(group_id1)
boottest treatment, reps(99999) cluster(group_id1 ) bootcluster(group_id1 q1_immigration) nograph
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_1, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm1$p_val, stata_p_val, tol = tol)
  
  
  # Test 2: 
  
  boot_lm2 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        bootcluster = c("group_id1", "Q1_immigration"),
        B = 99999,
        seed = 911,
        param = "treatment",
        conf_int = FALSE,
        sign_level = 0.10
      ))
  
  test_2 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration i.q2_defense, cluster(group_id1)
boottest treatment, reps(99999) cluster(group_id1 ) bootcluster(group_id1 q1_immigration)  nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  res <- RStata::stata(test_2, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm2$p_val,  stata_p_val, tol = tol)
  
  
  # Test 3
  
  test_3 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration i.q2_defense, cluster(group_id2)
boottest treatment = 0.05, reps(99999) cluster(group_id1 ) bootcluster(group_id1 q1_immigration) nograph level(90)
gen p_val = r(p)
//gen conf_int = r(CI)
"
  boot_lm3 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        bootcluster = c("group_id1", "Q1_immigration"),
        B = 99999,
        seed = 911,
        param = "treatment",
        conf_int = FALSE,
        sign_level = 0.10,
        beta0 = 0.05
      ))
  
  
res <- RStata::stata(test_3, data.out = TRUE)
stata_p_val <- unique(res$p_val)
expect_equal(boot_lm3$p_val,  stata_p_val, tol = tol)
  
  # Test 7
  
  boot_lm7 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  "group_id1",
        bootcluster = c("group_id1", "Q1_immigration","Q2_defense"),
        B = 99999,
        seed = 911,
        param = "treatment",
        conf_int = FALSE,
        sign_level = 0.10,
        type = "rademacher",
        impose_null = FALSE
      ))
  
  test_7 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration i.q2_defense, cluster(group_id1)
boottest treatment, reps(99999) cluster(group_id1 ) bootcluster(group_id1 q1_immigration q2_defense) nograph level(90) weighttype(rademacher) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
res <- RStata::stata(test_7, data.out = TRUE)
stata_p_val <- unique(res$p_val)
expect_equal(boot_lm7$p_val,  stata_p_val, tol= tol)
  
  

skip_test <- TRUE

# bottest.stata currently fails here
if(skip_test){
  # Test 8
  
  boot_lm8 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  c("group_id1", "group_id2"),
        bootcluster = c("group_id1", "Q1_immigration"),
        B = 199999,
        seed = 911,
        param = "treatment",
        conf_int = FALSE,
        sign_level = 0.05,
        type = "rademacher",
        impose_null = FALSE
      ))
  
  test_8 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration i.q2_defense, cluster(group_id1)
boottest treatment, reps(199999) cluster(group_id1 group_id2) bootcluster(group_id1 q1_immigration) nograph level(95) weighttype(rademacher) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_8, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm8$p_val,  stata_p_val, tol= tol)
  
  
  
  # Test 9
  
  boot_lm9 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  c("group_id1", "group_id2"),
        bootcluster = c("group_id1", "Q2_defense"),
        B = 199999,
        seed = 911,
        param = "treatment",
        conf_int = FALSE,
        sign_level = 0.05,
        type = "rademacher",
        impose_null = FALSE
      ))
  
  test_9 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration i.q2_defense, cluster(group_id1)
boottest treatment, reps(199999) cluster(group_id1 group_id2) bootcluster(group_id1 q2_defense) nograph level(95) weighttype(rademacher) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_9, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm9$p_val,  stata_p_val, tol= tol)
  
  
  # Test 10
  boot_lm10 <-
    suppressWarnings(
      boottest(
        object = lm_fit,
        clustid =  c("group_id1", "group_id2"),
        bootcluster = c("group_id1", "Q1_immigration", "Q2_defense"),
        B = 99999,
        seed = 911,
        param = "treatment",
        conf_int = FALSE,
        sign_level = 0.10,
        type = "rademacher",
        impose_null = FALSE
      ))
  
  test_10 <- "
clear
import delimited c:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income i.q1_immigration i.q2_defense, cluster(group_id1)
boottest treatment, reps(99999) cluster(group_id1 group_id2) bootcluster(group_id1 q1_immigration q2_defense) nograph level(90) weighttype(rademacher) nonull
gen p_val = r(p)
//gen conf_int = r(CI)
"
  
  res <- RStata::stata(test_10, data.out = TRUE)
  stata_p_val <- unique(res$p_val)
  expect_equal(boot_lm10$p_val,  stata_p_val, tol= tol)  
  
}


  
  
  
}
