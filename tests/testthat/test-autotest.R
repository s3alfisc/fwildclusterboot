test_that("run autotest pkg checks", {
  
  library(autotest)
  x <- autotest_package()
  
  expect_autotest_no_err(x)
  expect_autotest_no_warn(x)
  expect_autotest_notes(x)
  expect_autotest_no_testdata(x)  
  
})