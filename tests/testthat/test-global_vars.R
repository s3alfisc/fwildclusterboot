test_that("global boot algo", {
  
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
