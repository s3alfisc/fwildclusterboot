# A: test of equivalence between fwildclusterboot and wildboottestjlr
# testing via the tinytest package.
# 1) test boot_lm

# don't run tests automatically, else devtools::check() will fail
run <- TRUE

if(run){

  reltol <- 0.02
  
  N <- 10000
  #seed <- 871239345
  seed <- 875784

  lm_fit <- lm(proposition_vote ~ treatment  + log_income  ,
               data = wildboottestjlr:::create_data(N = 10000,
                                                    N_G1 = 20,
                                                    icc1 = 0.5,
                                                    N_G2 = 20,
                                                    icc2 = 0.2,
                                                    numb_fe1 = 10,
                                                    numb_fe2 = 10,
                                                    seed = 90864369,
                                                    #seed = 89761297,
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
  lm_fits <- list(ols = lm_fit, wls = lm_fit_weights)

  # object = lm_fit
  # impose_null = FALSE
  # type = "rademacher"
  # p_val_type = "equal-tailed"


  
  for(object in lm_fits){

    cat("start ols/wls", "\n")
    wildboottestjlr::set_julia_seed(12345)
    #fwildclusterboot:::set.fwildclusterboot.seed(12345)
    set.seed(12391786)
    dqrng::dqset.seed(8723467)
    #type <- "rademacher"
    for(type in c("rademacher", "webb", "mammen", "norm")){

    for(p_val_type in c("two-tailed", "equal-tailed", ">", "<")){
      
    
      for(impose_null in c(TRUE, FALSE)){

        cat(paste("type:", type, "p-val:", p_val_type, "null imposed:", impose_null), "\n")        
        
        cat("Check 1:", "\n")
        if(p_val_type %in% c("two-tailed", "equal-tailed")){
          boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null)
          boot_jl <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null)
          res <- expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$t_stat, boot_jl$t_stat[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
        } else {
          boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE)
          boot_jl <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE)
          res <- expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$t_stat, boot_jl$t_stat[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
        }
        
        rm(boot_r, boot_jl)
        
  

        # multi-param hypotheses
        cat("Check 2:", "\n")
        if(p_val_type %in% c("two-tailed", "equal-tailed")){
          boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), beta0 = 0.1, type = type, p_val_type = p_val_type, impose_null = impose_null)
          boot_jl <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), beta0 = 0.1, type = type, p_val_type = p_val_type, impose_null = impose_null)
          res <- expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$t_stat, boot_jl$t_stat[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          } else {
          boot_r <- fwildclusterboot::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), beta0 = 0.1, type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE)
          boot_jl <- wildboottestjlr::boottest(object, clustid = "group_id1", B = 99999, param = c("treatment", "log_income"), R = c(1, 0.1), beta0 = 0.1, type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE)
          res <- expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$t_stat, boot_jl$t_stat[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
        }
   
        rm(boot_r, boot_jl)
        
        # --------------------------------------------------
        # and all with twoway clustering:
        cat("Check 3:", "\n")
        if(p_val_type %in% c("two-tailed", "equal-tailed")){
          boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, nthreads = 1)
          boot_jl <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null)
          res <- expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$t_stat, boot_jl$t_stat[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          } else {
          boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, nthreads = 1, conf_int = FALSE)
          boot_jl <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE)
          res <- expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$t_stat, boot_jl$t_stat[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
        }
      
        rm(boot_r, boot_jl)
        
        # multi-param hypotheses

        cat("Check 4:", "\n")
        if(p_val_type %in% c("two-tailed", "equal-tailed")){
          boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 199999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = p_val_type, impose_null = impose_null, nthreads = 1)
          boot_jl <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 199999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = p_val_type, impose_null = impose_null)
          res <- expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$t_stat, boot_jl$t_stat[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$conf_int, c(boot_jl$conf_int), tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
        } else {
          boot_r <- fwildclusterboot::boottest(object, clustid = c("group_id1", "group_id2"), B = 199999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = p_val_type, impose_null = impose_null, nthreads = 1, conf_int = FALSE)
          boot_jl <- wildboottestjlr::boottest(object, clustid = c("group_id1", "group_id2"), B = 199999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE)
          res <- expect_equal(boot_r$p_val, boot_jl$p_val[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
          res <- expect_equal(boot_r$t_stat, boot_jl$t_stat[1], tolerance = reltol)
          if(res == FALSE){print(res)}
          rm(res)
        }

        rm(boot_r, boot_jl)
        
        # ------------------------------------------------------------------------------ #
        # test subcluster bootstrap

        # bootcluster variable not in clustid 1
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "Q1_immigration"), B = 99999, param = "treatment", type = "rademacher", p_val_type = p_val_type)
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "Q1_immigration"),B = 99999, param = "treatment", type = "rademacher", p_val_type = p_val_type,small_sample_adjustment = FALSE)
        # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
        #
        # # bootcluster variable not in clustid 2
        # # currently: bug in fwildclusterboot when not all bootcluster variables \in clustid OR specified in lm() (e.g. drop Q2_defense from lm_fit -> error)
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "year"), B = 199999, param = "treatment", type = "rademacher", p_val_type = p_val_type)
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "year"),B = 199999, param = "treatment", type = "rademacher", p_val_type = p_val_type,)
        # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
        #
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = "group_id2", bootcluster = c("group_id2", "state"), B = 199999, param = "treatment", type = "rademacher", p_val_type = p_val_type)
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id2", bootcluster = c("group_id2", "state"),B = 199999, param = "treatment", type = "rademacher", p_val_type = p_val_type,)
        # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
        #
        # # clustid variale not in bootcluster & bootcluster variable not in clustid
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1","group_id2", "Q1_immigration"), B = 99999, param = "treatment", nthreads = 1)
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1","group_id2", "Q1_immigration"),B = 99999, param = "treatment")
        # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
        #
        # # clustid variale not in bootcluster & bootcluster variable not in clustid
        # #boot_r <- fwildclusterboot::boottest(lm_fit, clustid = c("group_id1"), bootcluster = c("group_id1", "dummy"), B = 199999, param = "treatment", nthreads = 1)
        # #boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = c("group_id1"), bootcluster = c("group_id1", "dummy"),B = 199999, param = "treatment")
        # #print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
        #
        # boot_r <- fwildclusterboot::boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1"), B = 499999, param = "treatment", nthreads = 1)
        # boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1"),B = 499999, param = "treatment")
        # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))

      }

    }


  }

}

  # ------------------------------------------------------------------------------------------------ #
  # Test Suite 2: test that same seeds as specified via rng produce equivalent results:
  boot_jl1 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, rng = 1)
  boot_jl2 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, rng = 1)
  boot_jl3 <- wildboottestjlr::boottest(lm_fit, clustid = "group_id1", B = 99999, param = "treatment", type = type, rng = 2)

  print(expect_equal(boot_jl1$p_val[1], boot_jl2$p_val[1])) # expect exact equality
  print(expect_equal(boot_jl1$p_val[1], boot_jl3$p_val[1], tolerance = reltol))
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
  print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot)) * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G))))
  
  boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = "group_id1", B = 99999, param = "treatment", type = "rademacher", p_val_type = "equal-tailed")
  boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = "group_id1", B = 99999, param = "treatment", type = "rademacher", p_val_type = "equal-tailed",small_sample_adjustment = FALSE, floattype = "Float64")
  
  expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot)) * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G))))
  
  boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = "group_id1", B = 199999, param = "treatment", type = "rademacher", p_val_type = ">", conf_int = FALSE)
  boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = "group_id1", B = 199999, param = "treatment", type = "rademacher", p_val_type = ">", conf_int = FALSE,small_sample_adjustment = FALSE, floattype = "Float64")
  
  expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot)) * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G))))
  
  boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = "group_id1", B = 199999, param = "treatment", type = "rademacher", p_val_type = "<", conf_int = FALSE)
  boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = "group_id1", B = 199999, param = "treatment", type = "rademacher", p_val_type = "<", conf_int = FALSE,small_sample_adjustment = FALSE, floattype = "Float64")
  
  expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G)))
  print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot)) * sqrt((boot_jl_nosmall$N_G-1) / (boot_jl_nosmall$N_G))))
  
  # twoway clustering
  # boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "two-tailed")
  # boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = "rademacher", p_val_type = "two-tailed",small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  #
  # boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = "rademacher", p_val_type = p_val_type, bootcluster = "min")
  # boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 99999, param = "treatment", type = "rademacher", p_val_type = p_val_type, bootcluster = "min",small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  #
  # #boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1"), B = 99999, param = "treatment", type = "rademacher", p_val_type = p_val_type, bootcluster = c("group_id1", "dummy"))
  # #boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1"), B = 99999, param = "treatment", type = "rademacher", p_val_type = p_val_type, bootcluster = c("group_id1", "dummy"),small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # #expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # #print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  #
  # boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 199999, param = "treatment", type = "rademacher", p_val_type = ">", conf_int = FALSE)
  # boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 199999, param = "treatment", type = "rademacher", p_val_type = ">", conf_int = FALSE,small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  #
  # boot_r <- fwildclusterboot::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 199999, param = "treatment", type = "rademacher", p_val_type = "<", conf_int = FALSE, bootcluster = "min")
  # boot_jl_nosmall <- wildboottestjlr::boottest(lm_fit2, clustid = c("group_id1", "group_id2"), B = 199999, param = "treatment", type = "rademacher", p_val_type = "<", conf_int = FALSE, bootcluster = "min",small_sample_adjustment = TRUE, floattype = "Float64")
  #
  # expect_equivalent(boot_r$t_stat, boot_jl_nosmall$t_stat  * sqrt((N-1) / (N-k)))
  # print(expect_equal(sort(boot_r$t_boot), sort(c(boot_jl_nosmall$t_boot[!is.na(boot_jl_nosmall$t_boot)])) *sqrt((N-1) / (N-k)))
  
  # ------------------------------------------------------------------------------------------------------------------- #
  
  
  
  
  
  
  
}

