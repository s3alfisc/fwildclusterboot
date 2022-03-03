# A: test of equivalence between fwildclusterboot and wildboottestjlr
# testing via the tinytest package.

# don't run tests on CRAN
run_tests <- length(strsplit(packageDescription("fwildclusterboot")$Version, "\\.")[[1]]) > 2

if(run_tests){

  reltol <- 0.02

  N <- 10000
  #seed <- 871239345
  seed <- 875784

  data1 <<- fwildclusterboot:::create_data(N = 10000,
                                          N_G1 = 20,
                                          icc1 = 0.5,
                                          N_G2 = 20,
                                          icc2 = 0.2,
                                          numb_fe1 = 10,
                                          numb_fe2 = 10,
                                          seed = 90864369,
                                          weights = 1:N / N)

  lm_fit <- lm(proposition_vote ~ treatment  + log_income ,
               data = data1)



  lm_fit_weights <- lm(proposition_vote ~ treatment  + log_income  ,
                       weights = data1$weights,
                       data = data1)
  lm_fits <- list(ols = lm_fit, wls = lm_fit_weights)

  # object = lm_fit
  # impose_null = FALSE
  # type = "rademacher"
  # p_val_type = ">"


  cat("Part 1: Large B Tests", "\n")


  for(object in lm_fits){

    cat("start ols/wls", "\n")
    fwildclusterboot::set_julia_seed(12345)
    #:set.fwildclusterboot.seed(12345)
    set.seed(12391786)
    dqrng::dqset.seed(8723467)
    #type <- "rademacher"
    for(type in c("rademacher", "webb", "mammen", "norm")){

      for(p_val_type in c("two-tailed", "equal-tailed", ">", "<")){


        for(impose_null in c(TRUE, FALSE)){

          cat(paste("type:", type, "p-val:", p_val_type, "null imposed:", impose_null), "\n")

          cat("Check 1:", "\n")
          if(p_val_type %in% c("two-tailed", "equal-tailed")){
            boot_r <- boottest(object, clustid = "group_id1", B = 29999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null)
            # pracma::tic()
            boot_jl <- boottest(object, clustid = "group_id1", B = 29999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, boot_algo = "WildBootTests.jl")
            # pracma::toc()
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
            boot_r <- boottest(object, clustid = "group_id1", B = 29999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE)
            boot_jl <- boottest(object, clustid = "group_id1", B = 29999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE, boot_algo = "WildBootTests.jl")
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
            boot_r <- boottest(object, clustid = "group_id1", B = 29999, param = c("treatment", "log_income"), R = c(1, 0.1), r = 0.1, type = type, p_val_type = p_val_type, impose_null = impose_null)
            boot_jl <- boottest(object, clustid = "group_id1", B = 29999, param = c("treatment", "log_income"), R = c(1, 0.1), r = 0.1, type = type, p_val_type = p_val_type, impose_null = impose_null, boot_algo = "WildBootTests.jl")
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
            boot_r <- boottest(object, clustid = "group_id1", B = 29999, param = c("treatment", "log_income"), R = c(1, 0.1), r = 0.1, type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE)
            boot_jl <- boottest(object, clustid = "group_id1", B = 29999, param = c("treatment", "log_income"), R = c(1, 0.1), r = 0.1, type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE, boot_algo = "WildBootTests.jl")
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
            boot_r <- boottest(object, clustid = c("group_id1", "group_id2"), B = 29999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, nthreads = 1)
            boot_jl <- boottest(object, clustid = c("group_id1", "group_id2"), B = 29999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, boot_algo = "WildBootTests.jl")
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
            boot_r <- boottest(object, clustid = c("group_id1", "group_id2"), B = 29999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, nthreads = 1, conf_int = FALSE)
            boot_jl <- boottest(object, clustid = c("group_id1", "group_id2"), B = 29999, param = "treatment", type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE, boot_algo = "WildBootTests.jl")
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
            boot_r <- boottest(object, clustid = c("group_id1", "group_id2"), B = 29999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = p_val_type, impose_null = impose_null, nthreads = 1)
            boot_jl <- boottest(object, clustid = c("group_id1", "group_id2"), B = 29999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = p_val_type, impose_null = impose_null, boot_algo = "WildBootTests.jl")
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
            boot_r <- boottest(object, clustid = c("group_id1", "group_id2"), B = 29999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = p_val_type, impose_null = impose_null, nthreads = 1, conf_int = FALSE)
            boot_jl <- boottest(object, clustid = c("group_id1", "group_id2"), B = 29999, param = c("treatment", "log_income"), R = c(1, 0.1), type = type, p_val_type = p_val_type, impose_null = impose_null, conf_int = FALSE, boot_algo = "WildBootTests.jl")
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
          # boot_r <- boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "Q1_immigration"), B = 29999, param = "treatment", type = "rademacher", p_val_type = p_val_type)
          # boot_jl1 <- boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "Q1_immigration"),B = 29999, param = "treatment", type = "rademacher", p_val_type = p_val_type,small_sample_adjustment = FALSE)
          # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
          #
          # # bootcluster variable not in clustid 2
          # # currently: bug in fwildclusterboot when not all bootcluster variables \in clustid OR specified in lm() (e.g. drop Q2_defense from lm_fit -> error)
          # boot_r <- boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "year"), B = 29999, param = "treatment", type = "rademacher", p_val_type = p_val_type)
          # boot_jl1 <- boottest(lm_fit, clustid = "group_id1", bootcluster = c("group_id1", "year"),B = 29999, param = "treatment", type = "rademacher", p_val_type = p_val_type,)
          # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
          #
          # boot_r <- boottest(lm_fit, clustid = "group_id2", bootcluster = c("group_id2", "state"), B = 29999, param = "treatment", type = "rademacher", p_val_type = p_val_type)
          # boot_jl1 <- boottest(lm_fit, clustid = "group_id2", bootcluster = c("group_id2", "state"),B = 29999, param = "treatment", type = "rademacher", p_val_type = p_val_type,)
          # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
          #
          # # clustid variale not in bootcluster & bootcluster variable not in clustid
          # boot_r <- boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1","group_id2", "Q1_immigration"), B = 29999, param = "treatment", nthreads = 1)
          # boot_jl1 <- boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1","group_id2", "Q1_immigration"),B = 29999, param = "treatment")
          # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
          #
          # # clustid variale not in bootcluster & bootcluster variable not in clustid
          # #boot_r <- boottest(lm_fit, clustid = c("group_id1"), bootcluster = c("group_id1", "dummy"), B = 29999, param = "treatment", nthreads = 1)
          # #boot_jl1 <- boottest(lm_fit, clustid = c("group_id1"), bootcluster = c("group_id1", "dummy"),B = 29999, param = "treatment")
          # #print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))
          #
          # boot_r <- boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1"), B = 499999, param = "treatment", nthreads = 1)
          # boot_jl1 <- boottest(lm_fit, clustid = c("group_id1", "group_id2"), bootcluster = c("group_id1"),B = 499999, param = "treatment")
          # print(expect_equal(boot_r$p_val, boot_jl1$p_val[1], tolerance = reltol))

        }

      }


    }

  }




  # ------------------------------------------------------------------------------------------------------------------- #
  # Test Suite 3: test for exact equality of t_stat, t_boot, p_val under full enumeration (only for rademacher weights)

  cat("Part 2: Enumeration tests for exact equality", "\n")

  N <- 10000

  data2 <<- fwildclusterboot:::create_data(N = 10000,
                                           N_G1 = 6,
                                           icc1 = 0.5,
                                           N_G2 = 2,
                                           icc2 = 0.2,
                                           numb_fe1 = 5,
                                           numb_fe2 = 5,
                                           #seed = 41224,
                                           seed = 1235107,
                                           weights = 1:N / N)

  lm_fit2 <- lm(proposition_vote ~ treatment + Q1_immigration + Q2_defense,
                data = data2)

  lm_fit_weights2 <- lm(proposition_vote ~ treatment + Q1_immigration + Q2_defense,
                        weights = data2$weights,
                        data = data2)

  lm_fits <- list(lm_fit2, lm_fit_weights2)
  ssc <- boot_ssc(adj = FALSE,
                  cluster.adj = FALSE)

  for(object in lm_fits){



    cat("start ols/wls", "\n")

    for(p_val_type in c("two-tailed", "equal-tailed", ">", "<")){


      for(impose_null in c(TRUE, FALSE)){

        cat(paste("type:", "rademacher", "p-val:", p_val_type, "null imposed:", impose_null), "\n")

        # oneway clustering
        boot_r <- boottest(object,
                           clustid = "group_id1",
                           B = 29999,
                           param = "treatment",
                           type = "rademacher",
                           p_val_type = p_val_type,
                           impose_null = impose_null,
                           conf_int = FALSE,
                           ssc = boot_ssc(
                                          adj = TRUE,
                                          cluster.adj = TRUE))

        boot_jl_nosmall <- boottest(object,
                                    clustid = "group_id1",
                                    B = 29999,
                                    param = "treatment",
                                    type = "rademacher",
                                    p_val_type = p_val_type,
                                    impose_null = impose_null,
                                    ssc = boot_ssc(
                                                   adj = TRUE,
                                                   cluster.adj = TRUE), 
                                    conf_int = FALSE,
                                    boot_algo = "WildBootTests.jl")

        expect_equal(boot_r$p_val, boot_jl_nosmall$p_val)
        expect_equal(boot_r$t_stat, boot_jl_nosmall$t_stat)

        # twoway clustering
        boot_r <- boottest(object,
                           clustid = c("group_id1", "group_id2"),
                           B = 29999,
                           param = "treatment",
                           type = "rademacher",
                           p_val_type = p_val_type,
                           conf_int = FALSE,
                           bootcluster = "min",
                           ssc = boot_ssc(adj = TRUE))

        boot_jl_nosmall <- boottest(object,
                                    clustid = c("group_id1", "group_id2"),
                                    B = 29999,
                                    param = "treatment",
                                    type = "rademacher",
                                    p_val_type = p_val_type,
                                    conf_int = FALSE,
                                    bootcluster = "min",
                                    ssc = boot_ssc(adj = TRUE),
                                    boot_algo = "WildBootTests.jl", 
                                    floattype = "Float64")

        expect_equal(boot_r$p_val, boot_jl_nosmall$p_val)
        expect_equal(boot_r$t_stat, boot_jl_nosmall$t_stat)

      }

    }
  }







}
