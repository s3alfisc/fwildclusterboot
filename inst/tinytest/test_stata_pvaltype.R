# test if different p-value type "equal-tailed", ">" & "<" produce equivalent 
# results in STATA and R


run_tests <- length(strsplit(packageDescription("fwildclusterboot")$Version, "\\.")[[1]]) > 3

if(run_tests){
  
  # -------------------------------------------------------------------------- #
  # 0) Preliminaries
  
  library(fwildclusterboot)
  library(data.table)
  library(tinytest)
  library(RStata)
  
  options("RStata.StataVersion" = 16)
  #chooseStataBin()
  options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataIC-64\"")
  
  nthreads <- 4
  tol <- 0.05
  save_test_data_to <- "c:/Users/alexa/Dropbox/fwildclusterboot/"
  save_data <- paste0(save_test_data_to, "voters.csv")
  
  set.seed(12345)
  data1 <<- fwildclusterboot:::create_data(N = 3000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234)
  fwrite(data1, save_data)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income  ,
               data = data1)
  
# --------------------------------------------------------------------------- 
# 1) Test p_val_type = "equal-tailed"
  
test_1 <- "
clear
import delimited using C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income , cluster(group_id1) 
boottest  treatment = 0.01 , reps(999999) cluster(group_id1 ) nograph  ptype(equaltail)
gen p_val = r(p)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]"
  
test_2 <- "
clear
import delimited using C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income , cluster(group_id1)
boottest  5* treatment + 3*ideology1 =  0.2 , reps(999999) cluster(group_id1 ) nograph ptype(equaltail)
gen p_val = r(p)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
"
res_stata <- 
  lapply(c(test_1, test_2), function(x){
    tmp <- RStata::stata(x, data.out = TRUE)
    res <- list()
    res[["p_val"]] <- unique(tmp$p_val)
    res[["conf_int"]] <- cbind(unique(tmp$conf_int_l), unique(tmp$conf_int_u))
    res
})
  
res1 <- 
  boottest(lm_fit, 
           clustid = "group_id1", 
           param = c("treatment"),
           R = 1, 
           beta0 = 0.01,
           B = 999999, 
           conf_int = TRUE, 
           nthreads = nthreads, 
           p_val_type = "equal-tailed")

res2 <- 
  boottest(lm_fit, 
           clustid = "group_id1", 
           param = c("treatment", "ideology1"),
           R = c(5, 3), 
           beta0 = 0.2,
           B = 999999, 
           conf_int = TRUE, 
           nthreads = nthreads, 
           p_val_type = "equal-tailed")

expect_equal(res_stata[[1]]$p_val, res1$p_val, tol = tol)
expect_equal(as.vector(res_stata[[1]]$conf_int), res1$conf_int, tol = tol)

expect_equal(res_stata[[2]]$p_val, res2$p_val, tol = tol)
expect_equal(as.vector(res_stata[[2]]$p_val), res2$p_val, tol = tol)

# --------------------------------------------------------------------------- 
# 2) Test p_val_type = ">"

test_1 <- "
clear
import delimited using C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income, cluster(group_id1) 
boottest  treatment , reps(999999) cluster(group_id1 ) nograph  ptype(upper)
gen p_val = r(p)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]"

test_2 <- "
clear
import delimited using C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income, cluster(group_id1)
boottest  5* treatment + 3*ideology1 =  0.2 , reps(999999) cluster(group_id1 ) nograph ptype(upper)
gen p_val = r(p)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
"
res_stata <- 
  lapply(c(test_1, test_2), function(x){
    
    tmp <- RStata::stata(x, data.out = TRUE)
    
    res <- list()
    res[["p_val"]] <- unique(tmp$p_val)
    res[["conf_int"]] <- cbind(unique(tmp$conf_int_l), unique(tmp$conf_int_u))
    res
  })

res1 <- 
  boottest(lm_fit, 
           clustid = "group_id1", 
           param = c("treatment"),
           R = 1, 
           beta0 = 0,
           B = 999999, 
           nthreads = nthreads, 
           p_val_type = ">", 
           conf_int = FALSE)

res2 <- 
  boottest(lm_fit, 
           clustid = "group_id1", 
           param = c("treatment", "ideology1"),
           R = c(5, 3), 
           beta0 = 0.2,
           B = 999999, 
           nthreads = nthreads, 
           p_val_type = ">", 
           conf_int = FALSE)

expect_equal(res_stata[[1]]$p_val, res1$p_val, tol = tol)
expect_equal(res_stata[[2]]$p_val, res2$p_val, tol = tol)

# --------------------------------------------------------------------------- 
# 3) Test p_val_type = "<"

test_1 <- "
clear
import delimited using C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income, cluster(group_id1) 
boottest  treatment , reps(999999) cluster(group_id1 ) nograph  ptype(lower)
gen p_val = r(p)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]"

test_2 <- "
clear
import delimited using C:/Users/alexa/Dropbox/fwildclusterboot/voters.csv
set seed 1
quietly reg proposition_vote treatment ideology1 log_income, cluster(group_id1)
boottest  5* treatment + 3*ideology1 =  0.2 , reps(999999) cluster(group_id1 ) nograph ptype(lower)
gen p_val = r(p)
gen conf_int_l = r(CI)[1,1]
gen conf_int_u = r(CI)[1,2]
"
res_stata <- 
  lapply(c(test_1, test_2), function(x){
    
    tmp <- RStata::stata(x, data.out = TRUE)
    
    res <- list()
    res[["p_val"]] <- unique(tmp$p_val)
    res[["conf_int"]] <- cbind(unique(tmp$conf_int_l), unique(tmp$conf_int_u))
    res
  })

res1 <- 
  boottest(lm_fit, 
           clustid = "group_id1", 
           param = c("treatment"),
           R = 1, 
           beta0 = 0,
           B = 999999, 
           conf_int = FALSE, 
           nthreads = nthreads, 
           p_val_type = "<")

res2 <- 
  boottest(lm_fit, 
           clustid = "group_id1", 
           param = c("treatment", "ideology1"),
           R = c(5, 3), 
           beta0 = 0.2,
           B = 999999, 
           conf_int = FALSE, 
           nthreads = nthreads, 
           p_val_type = "<")  

expect_equal(res_stata[[1]]$p_val, res1$p_val, tol = tol)
expect_equal(res_stata[[2]]$p_val, res2$p_val, tol = tol)

}

