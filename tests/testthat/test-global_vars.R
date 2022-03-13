test_that("global boot_algo", {
  
  library(fwildclusterboot)
  data(voters)
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
               data = voters)
  
  boot1 <- boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1")
  boot2 <- suppressWarnings(boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1", boot_algo = "WildBootTests.jl"))
  
  expect_equal(boot1$boot_algo, "R")
  expect_equal(boot2$boot_algo, "WildBootTests.jl")
  
  # leave state of global variables clean
  setBoottest_boot_algo(boot_algo = "WildBootTests.jl")
  # switch back to "R" as global variable at end of function
  on.exit(setBoottest_boot_algo(boot_algo = "R"), add = TRUE)
  boot1 <- suppressWarnings(boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1"))
  boot2 <- suppressWarnings(boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1"))
  boot3 <- boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1", boot_algo = "R")
  
  expect_equal(boot1$boot_algo, "WildBootTests.jl")
  expect_equal(boot2$boot_algo, "WildBootTests.jl")
  expect_equal(boot3$boot_algo, "R")
  

})




test_that("global nthreads", {
  
  library(fwildclusterboot)
  data(voters)
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
               data = voters)
  
  boot1 <- boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1")
  boot2 <- boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1", nthreads = 4)
  boot3 <- boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1", nthreads = 0.5)
  
  expect_equal(boot1$nthreads, 1)  
  expect_equal(boot2$nthreads, 4)
  expect_equal(boot3$nthreads, 4)
  
  setBoottest_nthreads(4)
  expect_equal(getOption("boottest_nthreads"), 4)
  expect_equal(getBoottest_nthreads(), 4)
  # switch back to "R" as global variable at end of function
  on.exit(setBoottest_nthreads(), add = TRUE)
  
  boot3 <- boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1")
  boot4 <- boottest(lm_fit, param = "treatment", B = 999, clustid = "group_id1", nthreads = 2)
  
  expect_equal(boot3$nthreads, 4)  
  expect_equal(boot4$nthreads, 2)
  
  
  
})

