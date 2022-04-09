test_that("seed works for OLS", {
  
  skip_on_cran()
  
  data1 <<- fwildclusterboot:::create_data(N = 5000, N_G1 = 40, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1)
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
    data = data1
  )


  for (boot_algo in c("R", "R-lean", "WildBootTests.jl")) {


    # Case 1: seed set, no internal seeds
    set.seed(123)

    boot_lm_s1 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        B = 999,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    boot_lm_s2 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        B = 999,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    expect_true(boot_lm_s1$p_val != boot_lm_s2$p_val)


    # Case 2: same internal seed

    boot_lm_s1 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        #  full enumeration (N_G = 10, 2^10 = 1024)
        B = 999,
        seed = 1,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    boot_lm_s2 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        # no full enumeration (N_G = 10, 2^10 = 1024)
        B = 999,
        seed = 1,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)


    # Case 3: seed outside and within

    set.seed(9)
    boot_lm_s1 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        #  full enumeration (N_G = 10, 2^10 = 1024)
        B = 999,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    boot_lm_s2 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        # no full enumeration (N_G = 10, 2^10 = 1024)
        B = 999,
        seed = 9,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)


    # Case 4 different seed outside & within

    set.seed(9)
    boot_lm_s1 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        #  full enumeration (N_G = 10, 2^10 = 1024)
        B = 999,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    boot_lm_s2 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        # no full enumeration (N_G = 10, 2^10 = 1024)
        B = 999,
        seed = 2,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    expect_true(boot_lm_s1$p_val != boot_lm_s2$p_val)


    # Case 5: different starting seeds

    set.seed(9)
    boot_lm_s1 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        B = 999,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    set.seed(2)
    boot_lm_s2 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        B = 999,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        boot_algo = boot_algo
      )
    )

    expect_true(boot_lm_s1$p_val != boot_lm_s2$p_val)


    # Case 6: different seeds in boottest()

    boot_lm_s1 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        B = 999,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        seed = 1,
        boot_algo = boot_algo
      )
    )

    set.seed(2)
    boot_lm_s2 <- suppressMessages(
      boottest(
        object = lm_fit,
        clustid = "group_id1",
        B = 999,
        param = "treatment",
        type = "rademacher",
        conf_int = FALSE,
        seed = 2,
        boot_algo = boot_algo
      )
      
      
      
    )
    expect_true(boot_lm_s1$p_val != boot_lm_s2$p_val)
  }
  
  
})



test_that("internal_seed = FALSE", {
  
  
  skip_on_cran()
  
  # tests of internal_seed = FALSE
  
  # test A
  set.seed(123)
  
  boot_lm_s1 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R", 
      internal_seed = FALSE
    )
  )
  
  set.seed(123)
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R", 
      internal_seed = FALSE
    )
  )
  
  expect_false(boot_lm_s1$p_val == boot_lm_s2$p_val)
  
  # test B
  dqrng::dqset.seed(123)
  
  boot_lm_s1 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R", 
      internal_seed = FALSE
    )
  )
  
  dqrng::dqset.seed(123)
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R", 
      internal_seed = FALSE
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  # test C
  boot_lm_s1 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  dqrng::dqset.seed(123)
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R", 
      internal_seed = FALSE
    )
  )
  
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  
  
  # R-lean
  
  # test B
  set.seed(123)
  
  boot_lm_s1 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R-lean", 
      internal_seed = FALSE
    )
  )
  
  set.seed(123)
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R-lean", 
      internal_seed = FALSE
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  # test C
  boot_lm_s1 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R-lean", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R-lean", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  set.seed(123)
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R-lean", 
      internal_seed = FALSE
    )
  )
  
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "R-lean", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  
  
  # test A
  set.seed(123)
  
  boot_lm_s1 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "WildBootTests.jl", 
      internal_seed = FALSE
    )
  )
  
  set.seed(123)
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "WildBootTests.jl", 
      internal_seed = FALSE
    )
  )
  
  expect_false(boot_lm_s1$p_val == boot_lm_s2$p_val)
  
  # test B
  boot_lm_s1 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "WildBootTests.jl", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "WildBootTests.jl", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  # test C
  boot_lm_s1 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "WildBootTests.jl", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "WildBootTests.jl", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  dqrng::dqset.seed(123)
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "WildBootTests.jl", 
      internal_seed = FALSE
    )
  )
  
  boot_lm_s2 <- suppressMessages(
    boottest(
      object = lm_fit,
      clustid = "group_id1",
      B = 999,
      param = "treatment",
      type = "rademacher",
      conf_int = FALSE,
      boot_algo = "WildBootTests.jl", 
      internal_seed = FALSE, 
      seed = 123
    )
  )
  
  expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  
  
  
})



