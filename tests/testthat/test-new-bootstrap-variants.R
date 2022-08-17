test_that("new bootstrap variants", {
  
  # run Library(testthat) and library(fwildclusterboot) prior
  
  B <- 9999
  type <- "mammen"
  N_G1 <- 21
  
  data2 <- fwildclusterboot:::create_data(N = 1000,
                                          N_G1 = N_G1,
                                          icc1 = 0.8,
                                          N_G2 = N_G1,
                                          icc2 = 0.8,
                                          numb_fe1 = 10,
                                          numb_fe2 = 5,
                                          seed = 41224,
                                          #seed = 123,
                                          weights = 1:N / N)
  
  object <- lm(
    proposition_vote ~ treatment + ideology1 + log_income   ,
    data = data2
  )
  
  
  # new WCR11 ("fast and reliable") vs old WCR11 ("fast and wild")
  boot_algo <-  "WCR11"
  impose_null <- TRUE
  
  boot1 <- boottest(object,
                   param = "log_income",
                   clustid = c("group_id2"),
                   B = B,
                   impose_null = impose_null,
                   boot_algo = boot_algo,
                   seed = 432,
                   type = type,
                   conf_int = FALSE, 
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  boot2 <- boottest(object,
                   param = "log_income",
                   clustid = c("group_id2"),
                   B = B,
                   impose_null = impose_null,
                   boot_algo = "R",
                   seed = 432,
                   type = type,
                   conf_int = FALSE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(
    teststat(boot1), teststat(boot2), ignore_attr = TRUE
  )
  expect_equal(
    pval(boot1), pval(boot2), ignore_attr = TRUE
  )
  expect_equal(
    boot1$t_boot, boot2$t_boot, ignore_attr = TRUE
  )
  
  
  # new WCU11 ("fast and reliable") vs old WCR11 ("fast and wild")
  boot_algo <-  "WCU11"
  impose_null <- FALSE
  
  boot1 <- boottest(object,
                    param = "log_income",
                    clustid = c("group_id2"),
                    B = B,
                    impose_null = impose_null,
                    boot_algo = boot_algo,
                    seed = 432,
                    type = type,
                    conf_int = FALSE, 
                    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  boot2 <- boottest(object,
                    param = "log_income",
                    clustid = c("group_id2"),
                    B = B,
                    impose_null = impose_null,
                    boot_algo = "R",
                    seed = 432,
                    type = type,
                    conf_int = FALSE,
                    ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
  )
  
  expect_equal(
    teststat(boot1), teststat(boot2), ignore_attr = TRUE
  )
  expect_equal(
    pval(boot1), pval(boot2), ignore_attr = TRUE
  )
  expect_equal(
    boot1$t_boot, boot2$t_boot, ignore_attr = TRUE
  )
  
  
  
  
})