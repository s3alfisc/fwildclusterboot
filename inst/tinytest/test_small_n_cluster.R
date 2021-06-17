# tests for very number of clusters: 
# all tests are currently only run for rademacher weights. 
# results for mammen weights should be the same
# due to runtime, these tests should not be run of cran


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

library(fwildclusterboot)
# 
# # test: for different small cluster sizes so that (number of clusters)^2 < boot_iter, is the 
# # output of boottest() deterministic? It needs to be deterministic as full enumeration is used
# # if (number of clusters)^2 < boot_iter
# 
# fit_func <- function(x){
#   lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
#      data = fwildclusterboot:::create_data(N = 10000, N_G1 = x, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
# } 
# 
# boot_func <- function(fit, x, y){
#   B <- 2^x + 1
#   suppressWarnings(
#     boottest(
#       object = fit, 
#       clustid =  "group_id1", 
#       B = B, 
#       seed = y, 
#       param = "treatment", 
#       type = "rademacher",
#       conf_int = FALSE)
#   )$p_val
# }
# 
# for(x in 2:10){
#   
#   # estimate regression with 4 clusters
#   lm_fit <- fit_func(x = x) 
# 
#   # boottest of regression with x clusters for 4 different seeds
#   res <- vector(mode = "numeric", 10)
#   for(y in 1:10){
#     res[y] <- boot_func(fit = lm_fit, x = x, y = y)
#   }
#   expect_equal(res[1], mean(res[2:10]))
#   expect_true(res[1] %in% 0:x / x)
# }
# 



# Test 2: 
# for small numbers of clusters, only few distinct bootstrap t-statistics can be computed. E.g. for four 
# clusters & rademacher (or mammen) weights, there are 2^4 distinct t-statistics. In consequence, only 2^(4-1) = 8 different 
# p-values can be computed under full enumeration. Here, I test that only this appropriate number of p-values 
# can be computed with boottest() by enforcing full enumeration by setting 2^N_G < B

for(x in 2:10){
  
  iter <- 100
  res <- vector(mode = "numeric", iter)
  z <<- x
  
  for(i in 1:iter){
    
    seed <<- i
    # estimate regression models with 2, 3, ..., 10 clusters
    lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
                 data = fwildclusterboot:::create_data(N = 100, N_G1 = z, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed))
    
    boot_lm <-  suppressWarnings(
      boottest(
        object = lm_fit, 
        clustid =  "group_id1", 
        # guarantees that full enumeration is employed
        B = 2^z + 1, 
        seed = 1, 
        param = "treatment", 
        type = "rademacher",
        conf_int = FALSE)
    )
    
    res[i] <- boot_lm$p_val
  }
  
  # check if all values in res are theoretically feasible 
  potential_p_values <-  0:(2^(z-1)) / (2^(z-1))
  
  # test if the number of distinct p-values is smaller or equal than the number of theoretically feasible p-values
  expect_true(length(unique(res)) <= length(potential_p_values))
  # test if calculated p-values are in the set of potential p-values
  expect_equal(mean(unique(res) %in% potential_p_values), 1)
}





