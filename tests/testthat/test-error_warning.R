test_that("errors and warnings q = 1", {
  
  skip_on_cran()

  if(is_juliaconnector_prepared()){

    # ------------------------------------------------------------------ #
    # test for warnings and errors
    # ------------------------------------------------------------------ #
    requireNamespace("lfe")
    requireNamespace("fixest")
    
    
    for (engine in c("R", "WildBootTests.jl", "R-lean")) {
      cat(engine, "\n")
      
      # for(engine in c("R-lean")){
      # test boottest function arguments for errors
      lm_fit <-
        lm(
          proposition_vote ~ treatment + ideology1 + log_income +
            Q1_immigration,
          data = fwildclusterboot:::create_data(
            N = 1000,
            N_G1 = 10,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      feols_fit <-
        fixest::feols(
          proposition_vote ~ treatment + ideology1 + log_income 
          + Q1_immigration,
          data = fwildclusterboot:::create_data(
            N = 1000,
            N_G1 = 10,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      felm_fit <-
       lfe::felm(
          proposition_vote ~ treatment + ideology1 + log_income 
          + Q1_immigration,
          data = fwildclusterboot:::create_data(
            N = 1000,
            N_G1 = 10,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      feols_fit_c <-
        fixest::feols(
          proposition_vote ~ treatment + ideology1 + log_income |
            Q1_immigration,
          data = fwildclusterboot:::create_data(
            N = 1000,
            N_G1 = 10,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      felm_fit_c <-
       lfe::felm(
          proposition_vote ~ treatment + ideology1 + log_income |
            Q1_immigration,
          data = fwildclusterboot:::create_data(
            N = 1000,
            N_G1 = 10,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      
      
      
      
      # sign_level
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sign_level = 1.1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = feols_fit,
          clustid = c("group_id1"),
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sign_level = 1.1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sign_level = 1.1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sign_level = -1.1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = feols_fit,
          clustid = c("group_id1"),
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sign_level = -1.1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sign_level = -1.1,
          engine = engine
        )
      )
      
      # B < 100
      if (engine %in% c("R", "R-lean")) {
        expect_error(
          boottest(
            object = lm_fit,
            clustid = "group_id1",
            B = 99,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            engine = engine
          )
        )
        expect_error(
          boottest(
            object = feols_fit,
            clustid = c("group_id1"),
            B = 99,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            engine = engine
          )
        )
        expect_error(
          boottest(
            object = felm_fit,
            clustid = "group_id1",
            B = 99,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            engine = engine
          )
        )
      }
      
      # param not in data.frame
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment1",
          conf_int = TRUE,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = feols_fit,
          clustid = c("group_id1"),
          B = 999,
          seed = 911,
          param = "treatment1",
          conf_int = TRUE,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment1",
          conf_int = TRUE,
          engine = engine
        )
      )
      
      # rademacher enumeration case
      if (engine != "R-lean") {
        suppressWarnings(expect_warning(
          boottest(
            object = lm_fit,
            clustid = "group_id1",
            B = 9999,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            engine = engine
          )
        ))
        suppressWarnings(expect_warning(
          boottest(
            object = feols_fit,
            clustid = "group_id1",
            B = 9999,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            engine = engine
          )
        ))
        suppressWarnings(expect_warning(
          boottest(
            object = felm_fit,
            clustid = "group_id1",
            B = 9999,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            engine = engine
          )
        ))
      }
      
      suppressWarnings(expect_warning(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 2^10,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          engine = engine
        )
      ))
      
      
      
      # test for banned function arguments and syntax for fixest
      feols_fit <-
        fixest::feols(
          proposition_vote ~ treatment + ideology1 +
            i(log_income, Q1_immigration),
          data = fwildclusterboot:::create_data(
            N = 1000,
            N_G1 = 10,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      expect_error(
        boottest(
          object = feols_fit,
          clustid = c("group_id1"),
          B = 999,
          seed = 911,
          param = "treatment1",
          conf_int = TRUE,
          engine = engine
        )
      )
      
      feols_fit <-
        fixest::feols(
          proposition_vote ~ treatment + ideology1 + log_income
          + Q1_immigration,
          weights = fwildclusterboot:::create_data(
            N = 10000,
            N_G1 = 20,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )$weights,
          data = fwildclusterboot:::create_data(
            N = 10000,
            N_G1 = 20,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      felm_fit <-
       lfe::felm(
          proposition_vote ~ treatment + ideology1 + log_income 
          + Q1_immigration,
          weights = fwildclusterboot:::create_data(
            N = 10000,
            N_G1 = 20,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )$weights,
          data = fwildclusterboot:::create_data(
            N = 10000,
            N_G1 = 20,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      
      # joint fe != NULL and weights = on
      if (engine %in% c("R", "R-lean")) {
        expect_error(
          boottest(
            object = felm_fit,
            clustid = "group_id1",
            B = 999,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            fe = "Q1_immigration",
            engine = engine
          )
        )
        expect_error(
          boottest(
            object = feols_fit,
            clustid = c("group_id1"),
            B = 999,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            fe = "Q1_immigration",
            engine = engine
          )
        )
      }
      
      # nthreads < 1
      
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          nthreads = -1,
          engine = engine
        )
      )
      
      # expect_warning(boottest(object = lm_fit,
      #                       clustid =  "group_id1",
      #                       B = 999, seed = 911,
      #                       ,
      #                       conf_int = TRUE,
      #                       nthreads = 20))
      
      # Warning: In boottest.lm(object = lm_fit, clustid = "group_id1...:
      # Asked for 20 threads while the maximum is 8. Set to 8 threads instead.
      # will probably not run on cran, as max 2 cores
      
      expect_error(
        boottest(
          object = feols_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          nthreads = -1,
          engine = engine
        )
      )
      # expect_warning(boottest(object = feols_fit,
      #                       clustid =  "group_id1",
      #                       B = 999, seed = 911,
      #                       ,
      #                       conf_int = TRUE,
      #                       nthreads = 20))
      
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          nthreads = -1,
          engine = engine
        )
      )
      
      # expect_warning(boottest(object = felm_fit,
      #                       clustid =  "group_id1",
      #                       B = 999, seed = 911,
      #                       ,
      #                       conf_int = TRUE,
      #                       nthreads = 20))
      
      # maxiter
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          maxiter = -1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          maxiter = 0.1,
          engine = engine
        )
      )
      
      expect_error(
        boottest(
          object = feols_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          maxiter = -1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = feols_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          maxiter = 0.1,
          engine = engine
        )
      )
      
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          maxiter = -1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          maxiter = 0.1,
          engine = engine
        )
      )
      
      
      # tol
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          tol = -1,
          engine = engine
        )
      )
      
      expect_error(
        boottest(
          object = feols_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          tol = -1,
          engine = engine
        )
      )
      
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          tol = -1,
          engine = engine
        )
      )
      
      # p-val type
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          p_val_type = "equaltail",
          engine = engine
        )
      )
      
      expect_error(
        boottest(
          object = feols_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          p_val_type = "equaltail",
          engine = engine
        )
      )
      
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          p_val_type = "equaltail",
          engine = engine
        )
      )
      
      # B = 1000
      suppressWarnings(expect_message(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 1000,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          engine = engine
        )
      ))
      
      if (engine != "R-lean") {
        suppressWarnings(expect_message(
          boottest(
            object = feols_fit,
            clustid = "group_id1",
            B = 1000,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            engine = engine
          )
        ))
        
        suppressWarnings(expect_message(
          boottest(
            object = felm_fit,
            clustid = "group_id1",
            B = 1000,
            seed = 911,
            param = "treatment",
            conf_int = TRUE,
            engine = engine
          )
        ))
      }
      
      # banned function arguments
      
      
      # evalute dots ... in methods
      # write sig_level instead of sign_level
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sig_level = 0.1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = feols_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sig_level = 0.1,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = "treatment",
          conf_int = TRUE,
          sig_level = 0.1,
          engine = engine
        )
      )
      
      lm_fit <-
        lm(
          proposition_vote ~ treatment + ideology1 + log_income
          + Q1_immigration,
          data = fwildclusterboot:::create_data(
            N = 1000,
            N_G1 = 10,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        )
      
      res <- boottest(
        lm_fit,
        clustid = "group_id1",
        B = 999,
        seed = 911,
        param = "treatment",
        conf_int = TRUE,
        sign_level = 0.1,
        engine = engine
      )
      
      expect_error(summary(res, a = 1))
      # expect_error(tidy(res, a = 1))
      expect_error(plot(res, a = 1))
      
      
      
      
      # if 2^(number of clusters) < B and rademacher or mammen weights are used,
      # boottest() switches
      # to full enumeration. In consequence, only 2^(number of clusters - 1)
      # unique t statistics can be computed (see Webb, "Reworking wild bootstrap
      # based inference for clustered errors", 2013)
      # This will cause trouble for the inversion of p-values, for two reasons:
      # a) the p-value function will not
      # be sufficiently smooth b) no appropriate starting value for the root
      # finding procedure will be found
      # this set of tests checks if boottest() throws an error in the part
      # of the code that is responsible for
      # calculating p-values
      
      lm_fit <-
        lm(
          proposition_vote ~ treatment + ideology1 + log_income 
          + Q1_immigration,
          data = fwildclusterboot:::create_data(
            N = 100,
            N_G1 = 4,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1
          )
        )
      
      # no confidence intervals calculated: expect warning
      expect_warning(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          # guarantees that full enumeration is employed
          B = 2^4 + 1,
          seed = 1,
          param = "treatment",
          type = "rademacher",
          conf_int = FALSE,
          engine = engine
        )
      )
      
      
      
      # with confidence intervals: expect_error because B < 100
      if (engine == "R") {
        expect_error(
          boottest(
            object = lm_fit,
            clustid = "group_id1",
            # guarantees that full enumeration is employed
            B = 2^4 + 1,
            seed = 1,
            param = "treatment",
            type = "rademacher",
            conf_int = TRUE,
            engine = engine
          )
        )
      }
      
      
      # with confidence intervals: expect_error because B < 100
      if (engine == "R") {
        expect_error(
          boottest(
            object = lm_fit,
            clustid = "group_id1",
            # guarantees that full enumeration is employed
            B = 2^4 + 1,
            seed = 1,
            param = "treatment",
            type = "mammen",
            conf_int = TRUE,
            engine = engine
          )
        )
      }
      
      
      
      
      
      # --------------------------------------------------------------------- #
      # NA values in the cluster variables
      # --------------------------------------------------------------------- #
      
      data <-
        fwildclusterboot:::create_data(
          N = 100,
          N_G1 = 20,
          icc1 = 0.01,
          N_G2 = 10,
          icc2 = 0.01,
          numb_fe1 = 10,
          numb_fe2 = 10,
          seed = 1
        )
      data[1, "group_id1"] <- NA
      data2 <<- data
      
      lm_fit <-
        lm(proposition_vote ~ treatment + ideology1 + log_income
           + Q1_immigration,
           data = data2
        )
      
      # expect error as na_omit = FALSE & missing variable in group_id1
      # (the cluster variable)
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          # guarantees that full enumeration is employed
          B = 999,
          seed = 1,
          param = "treatment",
          type = "rademacher",
          conf_int = TRUE,
          engine = engine
        )
      )
      
      expect_error(
        boottest(
          object = lm_fit,
          clustid = ~ group_id1 + group_id2,
          # guarantees that full enumeration is employed
          B = 999,
          seed = 1,
          param = "treatment",
          type = "rademacher",
          conf_int = TRUE,
          engine = engine
        )
      )
      
      # NAs in a bootcluster variable
      
      expect_error(
        boottest(
          object = lm_fit,
          clustid = ~group_id2,
          bootcluster = ~ group_id1 + group_id2,
          # guarantees that full enumeration is employed
          B = 999,
          seed = 1,
          param = "treatment",
          type = "rademacher",
          conf_int = TRUE,
          engine = engine
        )
      )
      # suppressWarnings(expect_warning(
      #   res <-
      #     boottest(
      #       object = lm_fit,
      #       clustid = "group_id1",
      #       # guarantees that full enumeration is employed
      #       B = 999,
      #       seed = 1,
      #       param = "treatment",
      #       type = "rademacher",
      #       conf_int = TRUE,
      #       na_omit = TRUE, engine = engine
      #     )
      # ))
      # expect_equal(res$N, 99)
      
      # data[2, "group_id1"] <- NA
      # data3 <<- data
      # lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income
      # + Q1_immigration,
      #   data = data3
      # )
      # suppressWarnings(expect_warning(
      #   res <-
      #     boottest(
      #       object = lm_fit,
      #       clustid = "group_id1",
      #       # guarantees that full enumeration is employed
      #       B = 999,
      #       seed = 1,
      #       param = "treatment",
      #       type = "rademacher",
      #       conf_int = TRUE,
      #       na_omit = TRUE, engine = engine
      #     )
      # ))
      # expect_equal(res$N, 98)
      
      
      # expect error when length(R) != length(param)
      
      expect_error(
        boottest(
          object = lm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = c("treatment", "ideology1"),
          R = 1,
          conf_int = TRUE,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = feols_fit,
          clustid = c("group_id1"),
          B = 999,
          seed = 911,
          param = c("treatment", "ideology1"),
          R = 1,
          conf_int = TRUE,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = felm_fit,
          clustid = "group_id1",
          B = 999,
          seed = 911,
          param = c("treatment", "ideology1"),
          R = 1,
          conf_int = TRUE,
          engine = engine
        )
      )
      
      # specify a fixed effect that is also clustering variable OR test variable
      # -> error
      
      expect_error(
        boottest(
          object = feols_fit_c,
          fe = "Q1_immigration",
          clustid = c("Q1_immigration"),
          B = 999,
          seed = 911,
          param = c("treatment", "ideology1"),
          R = 1,
          conf_int = TRUE,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = felm_fit_c,
          fe = "Q1_immigration",
          clustid = "Q1_immigration",
          B = 999,
          seed = 911,
          param = c("treatment", "ideology1"),
          R = 1,
          conf_int = TRUE,
          engine = engine
        )
      )
      
      expect_error(
        boottest(
          object = feols_fit_c,
          clustid = c("Q1_immigration"),
          B = 999,
          seed = 911,
          param = c("Q1_immigration"),
          R = 1,
          conf_int = TRUE,
          engine = engine
        )
      )
      expect_error(
        boottest(
          object = felm_fit_c,
          clustid = "Q1_immigration",
          B = 999,
          seed = 911,
          param = c("Q1_immigration"),
          R = 1,
          conf_int = TRUE,
          engine = engine
        )
      )
      
      
      # test for p-val type & conf_int == TRUE (tba)
      
      # check fixest for deleted singletons. boottest() should throw an error
      
      base <- iris
      names(base) <- c("y", "x1", "x_endo_1", "x_inst_1", "fe")
      set.seed(2)
      base$x_inst_2 <-
        0.2 * base$y + 0.2 * base$x_endo_1 + rnorm(150, sd = 0.5)
      base$x_endo_2 <-
        0.2 * base$y - 0.2 * base$x_inst_1 + rnorm(150, sd = 0.5)
      base$clustid <- sample(1:10, nrow(base), TRUE)
      base$clustid[1] <- 11
      base$clustid[2] <- 12
      # unique singletons -> 11, 12
      
      feols_fit <- fixest::feols(y ~ x1 | clustid, base, fixef.rm = "both")
      # lfe::felm - no such behavior
      # felm_fit =lfe::felm(y ~ x1 | clustid , 
      # base, keepX = TRUE, keepCX = TRUE)
      # dim(felm_fit$X)
      # dim(felm_fit$cX)
      # summary(felm_fit)
      
      expect_error(boottest(
        feols_fit,
        param = "x1",
        B = 999,
        clustid = "clustid",
        engine = engine
      ))
      
      
      data <-
        fwildclusterboot:::create_data(
          N = 1000,
          N_G1 = 10,
          icc1 = 0.01,
          N_G2 = 10,
          icc2 = 0.01,
          numb_fe1 = 10,
          numb_fe2 = 10,
          seed = 1234
        )
      weights_vec <- data$weights
      
      lm_fit <-
        lm(
          proposition_vote ~ treatment + ideology1 + log_income
          + Q1_immigration,
          weights = weights_vec,
          data = data
        )
      feols_fit <-
        fixest::feols(
          proposition_vote ~ treatment + ideology1 + log_income 
          + Q1_immigration,
          weights = weights_vec,
          data = data
        )
      lfe_fit <-
        lfe::felm(
          proposition_vote ~ treatment + ideology1 + log_income
          + Q1_immigration,
          weights = weights_vec,
          data = data
        )
      
      expect_error(
        boottest(
          lm_fit,
          param = "treatment",
          B = 999,
          clustid = "clustid",
          engine = engine
        )
      )
      expect_error(
        boottest(
          feols_fit,
          param = "treatment",
          B = 999,
          clustid = "clustid",
          engine = engine
        )
      )
      expect_error(
        boottest(
          lfe_fit,
          param = "treatment",
          B = 999,
          clustid = "clustid",
          engine = engine
        )
      )
      
      
      # param not in clustid, fe
      feols_fit <-
        fixest::feols(proposition_vote ~ treatment + ideology1 + log_income |
                        Q1_immigration,
                      data = data
        )
      lfe_fit <-
        lfe::felm(proposition_vote ~ treatment + ideology1 + log_income |
                    Q1_immigration,
                  data = data
        )
      
      expect_error(
        boottest(
          feols_fit,
          param = "treatment",
          fe = "treatment",
          B = 999,
          clustid = "group_id1",
          engine = engine
        )
      )
      expect_error(
        boottest(
          lfe_fit,
          param = "treatment",
          fe = "treatment",
          B = 999,
          clustid = "group_id1",
          engine = engine
        )
      )
      
      
      
      if (engine %in% c("R", "R-lean")) {
        # R is matrix
        expect_error(
          boottest(
            lm_fit,
            param = "treatment",
            R = matrix(c(0, 0), 2, 1),
            fe = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = engine
          )
        )
        expect_error(
          boottest(
            feols_fit,
            param = "treatment",
            R = matrix(c(0, 0), 2, 1),
            fe = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = engine
          )
        )
        expect_error(
          boottest(
            felm_fit,
            param = "treatment",
            R = matrix(c(0, 0), 2, 1),
            fe = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = engine
          )
        )
        
        expect_error(
          boottest(
            lm_fit,
            param = "treatment",
            fe = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = engine,
            p_val_type = ">",
            conf_int = TRUE
          )
        )
        expect_error(
          boottest(
            feols_fit,
            param = "treatment",
            fe = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = engine,
            p_val_type = ">",
            conf_int = TRUE
          )
        )
        expect_error(
          boottest(
            felm_fit,
            param = "treatment",
            fe = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = engine,
            p_val_type = ">",
            conf_int = TRUE
          )
        )
      }
      
      
      # no support for R-lean with fe = on
      if (engine == "R-lean") {
        feols_fit_c <-
         fixest::feols(
            proposition_vote ~ treatment + ideology1 + log_income |
              Q1_immigration,
            data = fwildclusterboot:::create_data(
              N = 1000,
              N_G1 = 10,
              icc1 = 0.01,
              N_G2 = 10,
              icc2 = 0.01,
              numb_fe1 = 10,
              numb_fe2 = 10,
              seed = 1234
            )
          )
        expect_error(
          boottest(
            feols_fit_c,
            param = "treatment",
            fe = "Q1_immigration",
            B = 999,
            clustid = "group_id1",
            engine = "R-lean"
          )
        )
        
        felm_fit_c <-
         lfe::felm(
            proposition_vote ~ treatment + ideology1 + log_income |
              Q1_immigration,
            data = fwildclusterboot:::create_data(
              N = 1000,
              N_G1 = 10,
              icc1 = 0.01,
              N_G2 = 10,
              icc2 = 0.01,
              numb_fe1 = 10,
              numb_fe2 = 10,
              seed = 1234
            )
          )
        expect_error(
          boottest(
            felm_fit_c,
            param = "treatment",
            fe = "Q1_immigration",
            B = 999,
            clustid = "group_id1",
            engine = "R-lean"
          )
        )
        
        # no support for R-lean with weights
        data1 <<-
          fwildclusterboot:::create_data(
            N = 1000,
            N_G1 = 10,
            icc1 = 0.01,
            N_G2 = 10,
            icc2 = 0.01,
            numb_fe1 = 10,
            numb_fe2 = 10,
            seed = 1234
          )
        lm_fit <-
          lm(
            proposition_vote ~ treatment + ideology1 + log_income,
            weights = data1$weights,
            data = data1
          )
        feols_fit <-
         fixest::feols(
            proposition_vote ~ treatment + ideology1 + log_income |
              Q1_immigration,
            weights = data1$weights,
            data = data1
          )
        felm_fit <-
         lfe::felm(
            proposition_vote ~ treatment + ideology1 + log_income |
              Q1_immigration,
            weights = data1$weights,
            data = data1
          )
        
        expect_error(
          boottest(
            lm_fit,
            param = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = "R-lean"
          )
        )
        expect_error(
          boottest(
            felm_fit,
            param = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = "R-lean"
          )
        )
        expect_error(
          boottest(
            feols_fit,
            param = "treatment",
            B = 999,
            clustid = "group_id1",
            engine = "R-lean"
          )
        )
        
        
        feols_fit_c <-
         fixest::feols(
            proposition_vote ~ treatment + ideology1 + log_income |
              Q1_immigration,
            data = fwildclusterboot:::create_data(
              N = 1000,
              N_G1 = 10,
              icc1 = 0.01,
              N_G2 = 10,
              icc2 = 0.01,
              numb_fe1 = 10,
              numb_fe2 = 10,
              seed = 1234
            )
          )
        felm_fit_c <-
         lfe::felm(
            proposition_vote ~ treatment + ideology1 + log_income |
              Q1_immigration,
            data = fwildclusterboot:::create_data(
              N = 1000,
              N_G1 = 10,
              icc1 = 0.01,
              N_G2 = 10,
              icc2 = 0.01,
              numb_fe1 = 10,
              numb_fe2 = 10,
              seed = 1234
            )
          )
        
        expect_error(
          boottest(
            feols_fit_c,
            param = "treatment",
            B = 999,
            clustid = "group_id1",
            fe = "Q2_defense"
          )
        )
        
        expect_error(
          boottest(
            felm_fit_c,
            param = "treatment",
            B = 999,
            clustid = "group_id1",
            fe = "Q2_defense"
          )
        )
        
        expect_error(
          boottest(
            feols_fit_c,
            param = "treatment",
            B = 999,
            clustid = "group_id1",
            fe = "treatment"
          )
        )
        
        expect_error(
          boottest(
            felm_fit_c,
            param = "treatment",
            B = 999,
            clustid = "group_id1",
            fe = "treatment"
          )
        )
        
        feols_fit_c <-
         fixest::feols(
            proposition_vote ~ treatment + ideology1 + log_income |
              Q1_immigration^Q2_defense,
            data = fwildclusterboot:::create_data(
              N = 1000,
              N_G1 = 10,
              icc1 = 0.01,
              N_G2 = 10,
              icc2 = 0.01,
              numb_fe1 = 10,
              numb_fe2 = 10,
              seed = 1234
            )
          )
        
        expect_error(
          boottest(
            feols_fit_c,
            param = "treatment",
            B = 999,
            clustid = "group_id1",
            fe = "treatment"
          )
        )
      }
    }
  } else {
    message(
      "test-error_warning.R with q = 1 skipped as JULIA_BINDIR not found."
    )
    
  }
 
})



test_that("error warning IV/WRE and q > 1", {
  
  skip_on_cran()
  requireNamespace("ivreg")
  requireNamespace("fixest")
  requireNamespace("clubSandwich")

  
  if(is_juliaconnector_prepared()){
    
    # drop all NA values from SchoolingReturns
    #SchoolingReturns <-
    #  SchoolingReturns[rowMeans(sapply(SchoolingReturns, is.na)) == 0, ]
    SchoolingReturns <- na.omit(ivreg::SchoolingReturns)
    ivreg_fit <- ivreg::ivreg(
      log(wage) ~ education + age +
        ethnicity + smsa + south + parents14 |
        nearcollege + age + ethnicity + smsa
          + south + parents14,
      data = SchoolingReturns
    )
  
  
    # error because invalid param name
    expect_error(suppressMessages(
      boottest(
        object = ivreg_fit,
        clustid = "kww",
        B = 999,
        param = "res",
        type = "rademacher",
        conf_int = FALSE
      )
    ))
  
    # error due to length(R) != length(param)
    expect_error(suppressMessages(
      boottest(
        object = ivreg_fit,
        clustid = "kww",
        B = 999,
        param = "education",
        R = c(0, 1),
        type = "rademacher",
        conf_int = FALSE
      )
    ))
  
    # enumeration warning
    expect_warning(suppressMessages(
      boottest(
        object = ivreg_fit,
        clustid = "ethnicity",
        B = 999,
        param = "education",
        type = "rademacher",
        conf_int = FALSE
      )
    ))
  
    # drop all NA values from SchoolingReturns
    # SchoolingReturns <-
    #   SchoolingReturns[rowMeans(sapply(SchoolingReturns, is.na)) == 0, ]
    SchoolingReturns <- na.omit(SchoolingReturns)
    ivreg_fit <- ivreg::ivreg(
      log(wage) ~ education + age +
        ethnicity + smsa + south + parents14 |
        nearcollege + age + ethnicity + smsa
          + south + parents14,
      data = SchoolingReturns,
      method = "M"
    )
  
    expect_error(
      boottest(
        object = ivreg_fit,
        B = 999,
        param = "education",
        clustid = "kww",
        type = "mammen",
        impose_null = TRUE
      )
    )
  
  
  
    # bannd args ivreg
    # test for banned function arguments and syntax for fixest with mboottest
  
    feols_fit <- fixest::feols(
      proposition_vote ~ treatment + ideology1,
      data = fwildclusterboot:::create_data(
        N = 1000,
        N_G1 = 10,
        icc1 = 0.01,
        N_G2 = 10,
        icc2 = 0.01,
        numb_fe1 = 10,
        numb_fe2 = 10,
        seed = 1234
      )
    )
    # R <- clubSandwich::constrain_zero(1:2,coef(feols_fit))
    R <- matrix(c(1, 0, 0, 0, 1, 0), 2, 3)
    expect_error(mboottest(
      object = feols_fit,
      clustid = c("group_id1"),
      B = 999,
      seed = 911,
      R = R,
      r = 1:3
    ))
  } else {
    message(
      "test-error_warning.R with q > 1 skipped as JULIA_BINDIR not found."
    )
    
  }
})

test_that("error message when character vars in felm and fixest", {

  requireNamespace("lfe")
  requireNamespace("fixest")

  N <- 1000
  
  real_1000 <- runif(N, 1, 100)
  Year_cont <- sample(1:10, N, replace = TRUE)
  post1 <- sample(0:1, N, replace = TRUE)
  post2 <- sample(0:1, N, replace = TRUE)
  PWSID <- sample(LETTERS, N, TRUE)
  Utility_state <- as.character(sample(1:10, N, TRUE))
  SEclusters <- as.character(sample(1:10, N, TRUE))
  
  event_reg_data <- 
    data.frame(real_1000 = real_1000, 
               Year_cont = Year_cont, 
               post1 = post1, 
               post2 = post2, 
               PWSID = PWSID, 
               Utility_state = Utility_state, 
               SEclusters = SEclusters)
  
  res1 <- fixest::feols(
    real_1000 ~ post1 | SEclusters,
    data = event_reg_data
  )
  
  res2 <- lfe::felm(
    real_1000 ~ post1 | 0 | 0 | SEclusters,
    data = event_reg_data
  )
  
  res3 <- lfe::felm(
    real_1000 ~ post1 | Utility_state | 0 | 0,
    data = event_reg_data
  )
  
  expect_error(
    boottest(
      res1,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  expect_error(
    boottest(
      res2,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  expect_error(
    boottest(
      res3,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  
})

test_that("error message when character vars in felm and fixest", {
  
  requireNamespace("lfe")
  requireNamespace("fixest")

  N <- 1000
  
  real_1000 <- runif(N, 1, 100)
  Year_cont <- sample(1:10, N, replace = TRUE)
  post1 <- sample(0:1, N, replace = TRUE)
  post2 <- sample(0:1, N, replace = TRUE)
  PWSID <- sample(LETTERS, N, TRUE)
  Utility_state <- as.character(sample(1:10, N, TRUE))
  SEclusters <- as.character(sample(1:10, N, TRUE))
  
  event_reg_data <- 
    data.frame(real_1000 = real_1000, 
               Year_cont = Year_cont, 
               post1 = post1, 
               post2 = post2, 
               PWSID = PWSID, 
               Utility_state = Utility_state, 
               SEclusters = SEclusters)
  
  res1 <- fixest::feols(
    real_1000 ~ post1 | SEclusters,
    data = event_reg_data
  )
  
  res2 <- lfe::felm(
    real_1000 ~ post1 | 0 | 0 | SEclusters,
    data = event_reg_data
  )
  
  res3 <- lfe::felm(
    real_1000 ~ post1 | Utility_state | 0 | 0,
    data = event_reg_data
  )
  
  expect_error(
    boottest(
      res1,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  expect_error(
    boottest(
      res2,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  expect_error(
    boottest(
      res3,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  
})



test_that("error message when character vars in felm and fixest", {
  
  requireNamespace("lfe")
  requireNamespace("fixest")
  
  N <- 1000
  
  real_1000 <- runif(N, 1, 100)
  Year_cont <- sample(1:10, N, replace = TRUE)
  post1 <- sample(0:1, N, replace = TRUE)
  post2 <- sample(0:1, N, replace = TRUE)
  PWSID <- sample(LETTERS, N, TRUE)
  Utility_state <- as.character(sample(1:10, N, TRUE))
  SEclusters <- as.character(sample(1:10, N, TRUE))
  
  event_reg_data <- 
    data.frame(real_1000 = real_1000, 
               Year_cont = Year_cont, 
               post1 = post1, 
               post2 = post2, 
               PWSID = PWSID, 
               Utility_state = Utility_state, 
               SEclusters = SEclusters)
  
  res1 <- fixest::feols(
    real_1000 ~ post1 | SEclusters,
    data = event_reg_data
  )
  
  res2 <- lfe::felm(
    real_1000 ~ post1 | 0 | 0 | SEclusters,
    data = event_reg_data
  )
  
  res3 <- lfe::felm(
    real_1000 ~ post1 | Utility_state | 0 | 0,
    data = event_reg_data
  )
  
  expect_error(
    boottest(
      res1,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  expect_error(
    boottest(
      res2,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  expect_error(
    boottest(
      res3,
      param = "post1",
      clustid = "SEclusters",
      B = 999
    )    
  )
  
  requireNamespace("fixest")
    data(base_stagg)
    res_sunab = fixest::feols(
      y ~ x1 + fixest:::sunab(year_treated, year) | id + year,
      base_stagg,
      ssc = fixest::ssc(adj = TRUE, cluster.adj = TRUE)
    )
    
    expect_error(
      boottest(
        res_sunab, 
        param = "year::5", 
        clustid = "year", 
        B = 999
      )
    )



})
