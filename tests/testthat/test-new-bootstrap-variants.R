test_that("new bootstrap variants I", {
  

  B <- 999
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


test_that("new bootstrap variants II - t_stat equivalence", {
  
  N <- 1000
  N_G1 <- 17
  data <- fwildclusterboot:::create_data(
    N = N,
    N_G1 = N_G1,
    icc1 = 0.8,
    N_G2 = N_G1,
    icc2 = 0.8,
    numb_fe1 = 10,
    numb_fe2 = 5,
    seed = 123121,
    weights = 1:N / N
  )
  
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income, 
    data = data
  )
  
  # WCR
  wcr_algos <- c("R", "WCR11", "WCR13", "WCR31", "WCR33")
  p_val <- t_stat <- 
    list()
  
  for(x in wcr_algos){
    
    res <- 
      suppressWarnings(
        boottest(
          lm_fit, 
          param = ~treatment, 
          clustid = ~group_id1,
          B = 9999, 
          impose_null = TRUE,
          boot_algo = x, 
          seed = 123, 
          ssc = boot_ssc(
            adj = FALSE, 
            cluster.adj = FALSE
          )
        )
      )
    
    p_val[[x]] <- pval(res)
    t_stat[[x]] <- teststat(res)
    
  }
  
  df <- data.frame(
    "p_values" = unlist(p_val), 
    "t_statistics" = unlist(t_stat)
  )
  
  expect_equal(df$t_statistics[1], df$t_statistics[2])
  expect_equal(df$t_statistics[2], df$t_statistics[4])
  expect_equal(df$t_statistics[3], df$t_statistics[5])
  
  
  # WCU algos
  
  wcu_algos <- c("R", "WCU11", "WCU13", "WCU31", "WCU33")
  p_val <- t_stat <- 
    list()
  
  for(x in wcu_algos){
    
    res <- 
      suppressWarnings(
        boottest(
          lm_fit, 
          param = ~treatment, 
          clustid = ~group_id1,
          B = 9999, 
          impose_null = FALSE,
          boot_algo = x, 
          seed = 123, 
          ssc = boot_ssc(
            adj = FALSE, 
            cluster.adj = FALSE
          )
        )
      )
    
    p_val[[x]] <- pval(res)
    t_stat[[x]] <- teststat(res)
    
  }
  
  df <- data.frame(
    "p_values" = unlist(p_val), 
    "t_statistics" = unlist(t_stat)
  )
  
  expect_equal(df$t_statistics[1], df$t_statistics[2])
  expect_equal(df$t_statistics[2], df$t_statistics[4])
  expect_equal(df$t_statistics[3], df$t_statistics[5])
  
})




test_that("variants 31 R vs Julia", {
  
  skip_on_cran()
  skip_on_ci()
  
  if(fwildclusterboot:::is_juliaconnector_prepared()){
    
    # fully enumerated - deterministic - tests
    N_G1 <- 10
    B <- 9999 
    
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
  
    
    
    lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income +
                   Q1_immigration,
                 data = data2
    )
    
    
    
    # 1) test WCR31 
    
    boot31_jl <- boottest(lm_fit,
                       B = 9999,
                       param = "treatment",
                       clustid = "group_id1", 
                       boot_algo = "WildBootTests.jl-31"
    )
    
    boot31_r <- boottest(lm_fit,
                          B = 9999,
                          param = "treatment",
                          clustid = "group_id1", 
                          boot_algo = "WCR31"
    )
    
    
    testthat::expect_equal(
      pval(boot31_jl), 
      pval(boot31_r)
    )
    
    testthat::expect_equal(
      sort(boot31_jl$t_boot), 
      sort(boot31_r$t_boot) 
    )
    
    testthat::expect_equal(
      teststat(boot31_jl), 
      teststat(boot31_r)
    )
    
    #2) WCU31
    
    boot31_jl <- boottest(lm_fit,
                          B = 9999,
                          param = "treatment",
                          clustid = "group_id1",
                          impose_null = FALSE, 
                          boot_algo = "WildBootTests.jl-31"
    )
    pval(boot31_jl)
    
    boot31_r <- boottest(lm_fit,
                         B = 9999,
                         param = "treatment",
                         clustid = "group_id1", 
                         impose_null = FALSE, 
                         boot_algo = "WCU31"
    )
    
    
    testthat::expect_equal(
      pval(boot31_jl), 
      pval(boot31_r)
    )
    
    testthat::expect_equal(
      sort(boot31_jl$t_boot), 
      sort(boot31_r$t_boot) 
    )
    
    testthat::expect_equal(
      teststat(boot31_jl), 
      teststat(boot31_r)
    )
  
  }
  
})


