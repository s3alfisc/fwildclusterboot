# A: test of equivalence between fwildclusterboot and wildboottestjlr
# testing via the tinytest package.
# 1) test boot_lm

# don't run tests automatically, else devtools::check() will fail
run <- TRUE

if(run){

  reltol <- 0.02
  
  N <- 1000
  seed <- 879345
  
  
  lm_fit <- lm(proposition_vote ~ treatment  + log_income  ,
               data = wildboottestjlr:::create_data(N = 1000,
                                                    N_G1 = 20,
                                                    icc1 = 0.5,
                                                    N_G2 = 20,
                                                    icc2 = 0.2,
                                                    numb_fe1 = 10,
                                                    numb_fe2 = 10,
                                                    seed = 90864369,
                                                    weights = 1:N / N
               ))
  lm_fit_weights <- lm(proposition_vote ~ treatment  + log_income  ,
                       weights = weights,
                       data = wildboottestjlr:::create_data(N = 10000,
                                                            N_G1 = 20,
                                                            icc1 = 0.5,
                                                            N_G2 = 20,
                                                            icc2 = 0.2,
                                                            numb_fe1 = 10,
                                                            numb_fe2 = 10,
                                                            seed = 90864369,
                                                            weights = 1:N / N
                       ))
  lm_fits <- list(lm_fit, lm_fit_weights)
  
  # object = lm_fit
  # impose_null = TRUE
  # type = "rademacher"
  
  for(object in lm_fits){
    
    wildboottestjlr::set_julia_seed(12345)
    #fwildclusterboot:::set.fwildclusterboot.seed(12345)
    set.seed(12391786)
    dqrng::dqset.seed(8723467)
    type <- "rademacher"
    #for(type in c("rademacher", "webb", "mammen", "norm")){
      
      for(impose_null in c(TRUE, FALSE)){
        
        # pracma::tic()
        boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "two-tailed")
        # pracma::toc()
        
        # pracma::tic()
        boot_jl1 <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "two-tailed")
        # pracma::toc()
        
        expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol)
        expect_equal(boot_r$conf_int, c(boot_jl1$conf_int), tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "equal-tailed")
        boot_jl2 <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "equal-tailed")
        expect_equal(boot_r$p_val, boot_jl2$p_val[1], tolerance = reltol)
        expect_equal(boot_r$conf_int, c(boot_jl2$conf_int), tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = ">", conf_int = FALSE)
        boot_jl3 <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = ">", conf_int = FALSE)
        expect_equal(boot_r$p_val, boot_jl3$p_val[1], tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "<", conf_int = FALSE)
        boot_jl4 <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "<", conf_int = FALSE)
        expect_equal(boot_r$p_val, boot_jl4$p_val[1], tolerance = reltol)
        
        # multi-param hypotheses
        
        boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), beta0 = 0.1, type = type, p_val_type = "two-tailed")
        boot_jl <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), beta0 = 0.1, type = type, p_val_type = "two-tailed")
        expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
        expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "equal-tailed")
        boot_jl <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "equal-tailed")
        expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
        expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = ">", conf_int = FALSE)
        boot_jl <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = ">", conf_int = FALSE)
        expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "<", conf_int = FALSE)
        boot_jl <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "<", conf_int = FALSE)
        expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
        
        # --------------------------------------------------
        # and all with twoway clustering:
        boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = "two-tailed", nthreads = 4)
        boot_jl1 <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = "two-tailed")
        
        expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol)
        expect_equal(boot_r$conf_int, c(boot_jl1$conf_int), tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = "equal-tailed", nthreads = 4)
        boot_jl2 <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = "equal-tailed")
        expect_equal(boot_r$p_val, boot_jl2$p_val[1], tolerance = reltol)
        expect_equal(boot_r$conf_int, c(boot_jl2$conf_int), tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = ">", conf_int = FALSE, nthreads = 4)
        boot_jl3 <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = ">", conf_int = FALSE)
        expect_equal(boot_r$p_val, boot_jl3$p_val[1], tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = "<", conf_int = FALSE, nthreads = 4)
        boot_jl4 <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = "<", conf_int = FALSE)
        expect_equal(boot_r$p_val, boot_jl4$p_val[1], tolerance = reltol)
        
        # multi-param hypotheses
        
        boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 199999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "two-tailed", nthreads = 4)
        boot_jl <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 199999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "two-tailed")
        expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
        expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "equal-tailed", nthreads = 4)
        boot_jl <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "equal-tailed")
        expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
        expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = ">", conf_int = FALSE, nthreads = 4)
        boot_jl <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = ">", conf_int = FALSE)
        expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
        
        boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "<", conf_int = FALSE, nthreads = 4)
        boot_jl <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = "<", conf_int = FALSE)
        expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
        
        
        # ------------------------------------------------------------------------------ #
        # test subcluster bootstrap
        
        # bootcluster variable not in clustid 1
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "Q1_immigration"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "two-tailed")
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "Q1_immigration"),B = 99999, param = "treatment", type = "rademacher", p_val_type = "two-tailed",small_sample_adjustment = FALSE)
        # expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol)
        #
        # # bootcluster variable not in clustid 2
        # # currently: bug in fwildclusterboot when not all bootcluster variables \in clustid OR specified in lm() (e.g. drop Q2_defense from lm_fit -> error)
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "year"), B = 199999, param = "treatment", type = "rademacher", p_val_type = "two-tailed")
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "year"),B = 199999, param = "treatment", type = "rademacher", p_val_type = "two-tailed",)
        # expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol)
        #
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id2", bootcluster = c("group_id2", "state"), B = 199999, param = "treatment", type = "rademacher", p_val_type = "two-tailed")
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id2", bootcluster = c("group_id2", "state"),B = 199999, param = "treatment", type = "rademacher", p_val_type = "two-tailed",)
        # expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol)
        #
        # # clustid variale not in bootcluster & bootcluster variable not in clustid
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1","group_id2", "Q1_immigration"), B = 99999, param = "treatment", nthreads = 4)
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1","group_id2", "Q1_immigration"),B = 99999, param = "treatment")
        # expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol)
        #
        # # clustid variale not in bootcluster & bootcluster variable not in clustid
        # #boot_r <- fwildclusterboot::boottest(lm_fit, clustid = c("group_id1"), bootcluster = c("group_id1", "dummy"), B = 199999, param = "treatment", nthreads = 4)
        # #boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = c("group_id1"), bootcluster = c("group_id1", "dummy"),B = 199999, param = "treatment")
        # #expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol)
        #
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1"), B = 499999, param = "treatment", nthreads = 4)
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1"),B = 499999, param = "treatment")
        # expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol)
        
  #    }
      
    }
    
    
  }
  
  
  # ------------------------------------------------------------------------------------------------ #
  # Test Suite 2: test that same seeds as specified via rng produce equivalent results:
  boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "two-tailed", rng = 1)
  boot_jl2 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "two-tailed", rng = 1)
  boot_jl3 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = "two-tailed", rng = 2)
  
  expect_equal(boot_jl1$p_val[1], boot_jl2$p_val[1]) # expect exact equality
  expect_equal(boot_jl1$p_val[1], boot_jl3$p_val[1], tolerance = reltol)
  # ------------------------------------------------------------------------------------------------ #
  
  
  # ------------------------------------------------------------------------------------------------------------------- #
  # Test Suite 3: test for exact equality of t_stat, t_boot, p_val under full enumeration (only for rademacher weights)
  
  N <- 1000
  
  lm_fit2 <- lm(proposition_vote ~ treatment + Q1_immigration + Q2_defense,
                weights = weights,
                data = fwildclusterboot:::create_data(N = 1000,
                                                      N_G1 = 5,
                                                      icc1 = 0.5,
                                                      N_G2 = 2,
                                                      icc2 = 0.2,
                                                      numb_fe1 = 5,
                                                      numb_fe2 = 5,
                                                      #seed = 41224,
                                                      seed = 1235107,
                                                      weights = 1:N / N))
  k <- length(coef(lm_fit2))
  
  # oneway clustering
  boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = "group_id1", B = 99999, param = "treatment", type = "rademacher", p_val_type = "two-tailed")
  boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = "group_id1", B = 99999, param = "treatment", type = "rademacher", p_val_type = "two-tailed", small_sample_adjustment = FALSE, floattype = "Float64", fweights = FALSE)
  
  expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot)) * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  
  boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = "group_id1", B = 99999, param = "treatment", type = "rademacher", p_val_type = "equal-tailed")
  boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = "group_id1", B = 99999, param = "treatment", type = "rademacher", p_val_type = "equal-tailed",small_sample_adjustment = FALSE, floattype = "Float64")
  
  expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot)) * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  
  boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = "group_id1", B = 199999, param = "treatment", type = "rademacher", p_val_type = ">", conf_int = FALSE)
  boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = "group_id1", B = 199999, param = "treatment", type = "rademacher", p_val_type = ">", conf_int = FALSE,small_sample_adjustment = FALSE, floattype = "Float64")
  
  expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot)) * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  
  boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = "group_id1", B = 199999, param = "treatment", type = "rademacher", p_val_type = "<", conf_int = FALSE)
  boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = "group_id1", B = 199999, param = "treatment", type = "rademacher", p_val_type = "<", conf_int = FALSE,small_sample_adjustment = FALSE, floattype = "Float64")
  
  expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot)) * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  
  # twoway clustering
  # boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "two-tailed")
  # boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "two-tailed",small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  #
  # boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "equal-tailed", bootcluster = "min")
  # boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "equal-tailed", bootcluster = "min",small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  #
  # #boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "equal-tailed", bootcluster = c("group_id1", "dummy"))
  # #boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "equal-tailed", bootcluster = c("group_id1", "dummy"),small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # #expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # #expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  #
  # boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 199999, param = "treatment", type = "rademacher", p_val_type = ">", conf_int = FALSE)
  # boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 199999, param = "treatment", type = "rademacher", p_val_type = ">", conf_int = FALSE,small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  #
  # boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 199999, param = "treatment", type = "rademacher", p_val_type = "<", conf_int = FALSE, bootcluster = "min")
  # boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 199999, param = "treatment", type = "rademacher", p_val_type = "<", conf_int = FALSE, bootcluster = "min",small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  
  # ------------------------------------------------------------------------------------------------------------------- #
  
  
  
  
  
  
  
}