# test parallel code

# test tol & maxiter arguments


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
# library(fwildclusterboot)



lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , 
             data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                           data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                      data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
feols_fit_c <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, 
                             cluster = "group_id1",
                             data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
felm_fit_c <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 | group_id1,
                        data = fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))

boot_lm <-  suppressWarnings(boottest(object = lm_fit, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min", tol = 1e-10, maxiter = 30, 
                                      nthreads = 2))
#boot_fixest <- suppressWarnings(boottest(object = feols_fit, clustid = c("group_id1"), B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min", tol = 1e-10, maxiter = 30, nthreads = 0))
boot_felm <- suppressWarnings(boottest(object = felm_fit, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min", tol = 1e-10, maxiter = 30,
                                       nthreads = 1))
boot_fixest_c <- suppressWarnings(boottest(object = feols_fit_c, clustid = c("group_id1"), B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min", tol = 1e-10, maxiter = 30, 
                                           nthreads = 0.5))
boot_felm_c <- suppressWarnings(boottest(object = felm_fit_c, clustid =  "group_id1", B = 2999, seed = 911, param = "treatment", conf_int = TRUE, bootcluster = "min", tol = 1e-10, maxiter = 30, 
                                         nthreads = 2))

# point estimates
#expect_equivalent(boot_lm$point_estimate, boot_fixest$point_estimate, tolerance = 0.1)
#expect_equivalent(boot_fixest$point_estimate, boot_felm$point_estimate, tolerance = 0.1)
expect_equivalent(boot_felm$point_estimate, boot_fixest_c$point_estimate, tolerance = 0.1)
expect_equivalent(boot_fixest_c$point_estimate, boot_felm_c$point_estimate, tolerance = 0.1)
expect_equivalent(boot_felm_c$point_estimate, boot_lm$point_estimate, tolerance = 0.1)

# p-vals
#expect_equivalent(boot_lm$p_val, boot_fixest$p_val, tolerance = 0.1)
#expect_equivalent(boot_fixest$p_val, boot_felm$p_val, tolerance = 0.1)
expect_equivalent(boot_felm$p_val, boot_fixest_c$p_val, tolerance = 0.1)
expect_equivalent(boot_fixest_c$p_val, boot_felm_c$p_val, tolerance = 0.1)
expect_equivalent(boot_felm_c$p_val, boot_lm$p_val, tolerance = 0.1)

# t_stats
#expect_equivalent(boot_lm$t_stat, boot_fixest$t_stat, tolerance = 0.1)
# expect_equivalent(boot_fixest$t_stat, boot_felm$t_stat, tolerance = 0.1)
expect_equivalent(boot_felm$t_stat, boot_fixest_c$t_stat, tolerance = 0.1)
expect_equivalent(boot_fixest_c$t_stat, boot_felm_c$t_stat, tolerance = 0.1)
expect_equivalent(boot_felm_c$t_stat, boot_lm$t_stat, tolerance = 0.1)

# confidence intervals
#expect_equivalent(boot_lm$conf_int, boot_fixest$conf_int)
# expect_equivalent(boot_fixest$conf_int, boot_felm$conf_int)
expect_equivalent(boot_felm$conf_int, boot_fixest_c$conf_int)
expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int)
expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int)

}