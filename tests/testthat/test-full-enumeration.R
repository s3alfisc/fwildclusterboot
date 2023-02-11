test_that("test full enumeration cases: r and r-lean", {
  
  # note: these tests are deterministic! therefore exact.
  
    reltol <- 0.05
    
    N <- 2000
    seed <- 187019
    
    set.seed(2351)
    dqrng::dqset.seed(2351)
    
    data1 <<- fwildclusterboot:::create_data(
      N = N,
      N_G1 = 3,
      icc1 = 0.5,
      N_G2 = 20,
      icc2 = 0.2,
      numb_fe1 = 10,
      numb_fe2 = 10,
      seed = seed
      ,
      weights = 1:N / N
    )
    
    lm_fit <- lm(proposition_vote ~ treatment + log_income,
                 data = data1
    )
    
    
    
    lm_fit_weights <- lm(proposition_vote ~ treatment + log_income,
                         weights = data1$weights,
                         data = data1
    )
    
    lm_fits <- list(
      ols = lm_fit
      #,wls = lm_fit_weights
    )

    
    
    for (object in lm_fits) {
        #for (p_val_type in c("two-tailed", "equal-tailed", ">", "<")) {
          for (impose_null in c(TRUE)) {
            
            
            boot_r <- suppressWarnings(
              boottest(
                object, 
                clustid = "group_id1",
                B = 999,
                param = "treatment", 
                impose_null = impose_null,
                conf_int = FALSE, 
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
              )
            )
            
            
            boot_r_lean <- suppressWarnings(
              boottest(
                object,
                clustid = "group_id1", 
                B = 999, 
                param = "treatment",
                impose_null = impose_null,
                engine = "R-lean", 
                conf_int = FALSE, 
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
              )
            )
            
            boot_fnr <- suppressWarnings(
              boottest(
                object,
                clustid = "group_id1", 
                B = 999, 
                param = "treatment",
                impose_null = impose_null,
                engine = "R", 
                bootstrap_type = "11",
                conf_int = FALSE, 
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
              )
            )

    # note: difference in p-values due to discrete jumps: 
    # t-statistics are not directly identical (discrepancies at order e-14)
    # therefore, the actual t-statistic t can be smaller, larger, or lie within
    # the bootstrapped test statisics for which all weights are 1 or -1
    # as there are only 2^N_G_bootcluster t-stats and only 
            # 2^(N_G_bootcluster -1 )
    # different t-stats in abs values, the p-value needs to be 
    # x / (2^N_G_bootcluster), 
    # with x any integer
    # and depending where t lies (smaller, larger, or within), the p-value can 
    # be x / (2^N_G_bootcluster), (x-1) / (2^N_G_bootcluster), 
    # (x+1) / (2^N_G_bootcluster)
    # I therefore set the tolerance to 1 / (2^N_G_bootcluster) for all p-values
    # see sum(sort(boot_r$t_boot) - sort(boot_jl$t_boot)) != 0L
            

            expect_true(
              boot_r_lean$p_val %in% (boot_r$p_val +  -2:2 * 1 / 2^boot_r$N_G)
            )
            expect_true(
              boot_fnr$p_val %in% (boot_r$p_val +  -2:2 * 1 / 2^boot_r$N_G)
            )
            
            expect_equal(
              boot_r_lean$t_stat,
              boot_r$t_stat, 
              ignore_attr = TRUE
            )
            expect_equal(
              boot_fnr$t_stat,
              boot_r$t_stat, 
              ignore_attr = TRUE
            )
            
            expect_equal(
              sort(boot_r_lean$t_boot), 
              sort(boot_r$t_boot),
              ignore_attr = TRUE
            )
            expect_equal(
              sort(boot_fnr$t_boot), 
              sort(boot_r$t_boot),
              ignore_attr = TRUE
            )
          
            
            # check: with 9 clusters, 2^9 unique draws - 
            # hence the p-values calculated
            # must be an integer if multiplied by 512
            
            expect_true(
              (boot_r$p_val * 2^boot_r$N_G) %% 1 == 0
            )
            expect_true(
              (boot_r_lean$p_val * 2^boot_r_lean$N_G) %% 1 == 0
            )
        
            
            # multi-param hypotheses
            # cat("Check 2:", "\n")
            
            boot_r <- suppressWarnings(
              boottest(
                object,
                clustid = "group_id1",
                B = 999,
                param = c("treatment", "log_income"),
                R = c(-0.1, 0.1), 
                r = 0.1,
                impose_null = impose_null,
                conf_int = FALSE
              )
            )
            boot_r_lean <- suppressWarnings(
              boottest(
                object, 
                clustid = "group_id1",
                B = 999, 
                param = c("treatment", "log_income"),
                R = c(-0.1, 0.1), 
                r = 0.1, 
                impose_null = impose_null,
                engine = "R-lean", 
                conf_int = FALSE
              )
            )
            
    
            expect_true(
              boot_r_lean$p_val %in% (boot_r$p_val +  -2:2 * 1 / 2^boot_r$N_G)
            )
            expect_equal(
              boot_r_lean$t_stat,
              boot_r$t_stat, 
              ignore_attr = TRUE
            )
            expect_equal(
              sort(boot_r_lean$t_boot),
              sort(boot_r$t_boot), 
              ignore_attr = TRUE
            )
            
            # check: with 9 clusters, 2^9 unique draws - 
            # hence the p-values calculated
            # must be an integer if multiplied by 512
            
            expect_true(
              (boot_r$p_val * 2^boot_r$N_G) %% 1 == 0
            )
            expect_true(
              (boot_r_lean$p_val * 2^boot_r_lean$N_G) %% 1 == 0
            )
            
        }
      }
    
})




test_that("test full enumeration cases: r and julia", {
  
  # note: these tests are deterministic! therefore exact.
  
  skip_on_cran()
  skip_if_not(
    fwildclusterboot:::find_proglang("julia"), 
    message = "skip test as julia installation not found."
  )
  
    reltol <- 0.05
    
    N <- 2000
    seed <- 187019
    
    data1 <<- fwildclusterboot:::create_data(
      N = N,
      N_G1 = 8,
      icc1 = 0.5,
      N_G2 = 20,
      icc2 = 0.2,
      numb_fe1 = 10,
      numb_fe2 = 10,
      seed = seed
      ,
      weights = 1:N / N
    )
    
    lm_fit <- lm(proposition_vote ~ treatment + log_income,
                 data = data1
    )
    
    
    
    lm_fit_weights <- lm(proposition_vote ~ treatment + log_income,
                         weights = data1$weights,
                         data = data1
    )
    
    lm_fits <- list(
      ols = lm_fit
      #,wls = lm_fit_weights
    )
    
    for (object in lm_fits) {
          for (impose_null in c(TRUE, FALSE)) {
            
            
            boot_r <- suppressWarnings(
              boottest(
                object, 
                clustid = "group_id1",
                B = 999,
                param = "treatment", 
                impose_null = impose_null,
                conf_int = FALSE
              )
            )
           
            boot_jl <- suppressWarnings(
              boottest(object, 
                       clustid = "group_id1",
                       B = 999, 
                       param = "treatment", 
                       impose_null = impose_null,
                       engine = "WildBootTests.jl", 
                       conf_int = FALSE
              )
            )
            
            # note: difference in p-values due to discrete jumps: 
            # t-statistics are not directly identical 
            # (discrepancies at order e-14)
            # therefore, the actual t-statistic t can be smaller, larger, 
            # or lie within
            # the bootstrapped test statisics for which all weights are 1 or -1
            # as there are only 2^N_G_bootcluster t-stats and only 
            # 2^(N_G_bootcluster -1 )
            # different t-stats in abs values, the p-value needs to be 
            # x / (2^N_G_bootcluster), 
            # with x any integer
            # and depending where t lies (smaller, larger, or within), 
            # the p-value can 
            # be x / (2^N_G_bootcluster), (x-1) / (2^N_G_bootcluster), 
            # (x+1) / (2^N_G_bootcluster)
            # I therefore set the tolerance to 1 / (2^N_G_bootcluster) for
            # all p-values
            # see sum(sort(boot_r$t_boot) - sort(boot_jl$t_boot)) != 0L
            
            expect_true(
              boot_r$p_val %in% (boot_jl$p_val +  -2:2 * 1 / 2^boot_r$N_G)
            )
            
            expect_equal(
              boot_r$t_stat, 
              boot_jl$t_stat, 
              ignore_attr = TRUE
            )
            expect_equal(
              sort(boot_r$t_boot), 
              sort(boot_jl$t_boot), 
              ignore_attr = TRUE
            )
            
            
            
            # check: with 9 clusters, 2^9 unique draws - 
            # hence the p-values calculated
            # must be an integer if multiplied by 512
            
            expect_true(
              (boot_r$p_val * 2^boot_r$N_G) %% 1 == 0
            )
       
            expect_true(
              (boot_jl$p_val * 2^boot_jl$N_G) %% 1 == 0
            )
            
            # multi-param hypotheses
            # cat("Check 2:", "\n")
            
            boot_r <- suppressWarnings(
              boottest(
                object,
                clustid = "group_id1",
                B = 999,
                param = c("treatment", "log_income"),
                R = c(-0.1, 0.1), 
                r = 0.1,
                impose_null = impose_null,
                conf_int = FALSE
              )
            )
           
            boot_jl <- suppressWarnings(
              boottest(
                object,
                clustid = "group_id1", 
                B = 999, 
                param = c("treatment", "log_income"), 
                R = c(-0.1, 0.1), 
                r = 0.1,
                impose_null = impose_null,
                engine = "WildBootTests.jl",
                conf_int = FALSE
              )
            )
            
            expect_true(
              boot_r$p_val %in% (boot_jl$p_val +  -2:2 * 1 / 2^boot_r$N_G)
            )
            expect_equal(
              boot_r$t_stat,
              boot_jl$t_stat,
              ignore_attr = TRUE
            )
            expect_equal(
              sort(boot_r$t_boot), 
              sort(boot_jl$t_boot),
              ignore_attr = TRUE
            )
            
            
            
            # check: with 9 clusters, 2^9 unique draws - 
            # hence the p-values calculated
            # must be an integer if multiplied by 512
            
            expect_true(
              (boot_r$p_val * 2^boot_r$N_G) %% 1 == 0
            )
       
            expect_true(
              (boot_jl$p_val * 2^boot_jl$N_G) %% 1 == 0
            )
            
          }   
        }
})

