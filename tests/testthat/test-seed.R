test_that("seed works for OLS", {
  
  skip_on_cran()
  skip_if_not(
    find_proglang("julia"), 
    message = "skip test as julia installation not found."
  )
  
  requireNamespace("fixest")
  requireNamespace("dqrng")
  
  if(TRUE){
    data1 <<-
      fwildclusterboot:::create_data(
        N = 5000,
        N_G1 = 40,
        icc1 = 0.01,
        N_G2 = 10,
        icc2 = 0.01,
        numb_fe1 = 10,
        numb_fe2 = 10,
        seed = 1
      )
    lm_fit <-
      lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
        data = data1
      )
  
  
    for (engine in c("R", "R-lean", "WildBootTests.jl")) {
      # Case 1: seed set, no internal seeds
      set.seed(123)
      dqrng::dqset.seed(123)
  
      boot_lm_s1 <- suppressMessages(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          param = "treatment",
          type = "rademacher",
          conf_int = FALSE,
          engine = engine
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
          engine = engine
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
          engine = engine
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
          engine = engine
        )
      )
  
      expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)

  
      # Case 3: seed outside and within
  
      set.seed(9)
      dqrng::dqset.seed(9)
  
      boot_lm_s1 <- suppressMessages(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          #  full enumeration (N_G = 10, 2^10 = 1024)
          B = 999,
          param = "treatment",
          type = "rademacher",
          conf_int = FALSE,
          engine = engine
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
          engine = engine
        )
      )
  
      expect_equal(boot_lm_s1$p_val, boot_lm_s2$p_val)
  
  
      # Case 4 different seed outside & within
  
      set.seed(9)
      dqrng::dqset.seed(9)
      
      boot_lm_s1 <- suppressMessages(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          #  full enumeration (N_G = 10, 2^10 = 1024)
          B = 999,
          param = "treatment",
          type = "rademacher",
          conf_int = FALSE,
          engine = engine
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
          engine = engine
        )
      )
  
      expect_true(boot_lm_s1$p_val != boot_lm_s2$p_val)
  
  
      # Case 5: different starting seeds
  
      set.seed(9)
      dqrng::dqset.seed(9)
      boot_lm_s1 <- suppressMessages(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          param = "treatment",
          type = "rademacher",
          conf_int = FALSE,
          engine = engine
        )
      )
  
      set.seed(2)
      dqrng::dqset.seed(2)
      boot_lm_s2 <- suppressMessages(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          param = "treatment",
          type = "rademacher",
          conf_int = FALSE,
          engine = engine
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
          engine = engine
        )
      )
  
      set.seed(2)
      dqrng::dqset.seed(2)
      boot_lm_s2 <- suppressMessages(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          param = "treatment",
          type = "rademacher",
          conf_int = FALSE,
          seed = 2,
          engine = engine
        )
      )
      expect_true(boot_lm_s1$p_val != boot_lm_s2$p_val)
    }
  } else {
    message("test-seed.R skipped as JULIA_BINDR not found.")
  }
})
