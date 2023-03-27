test_that("Do different, but equivalent ways to specify
          linear models lead to equivalent results?", {
  
  skip_on_cran()
  skip_if_not(
      fwildclusterboot:::find_proglang("julia"), 
      message = "skip test as julia installation not found."
  )    
  
  set.seed(2351)
  dqrng::dqset.seed(2351)
  

  print_results <- FALSE

    data1 <<-
      fwildclusterboot:::create_data(
        N = 10000,
        N_G1 = 20,
        icc1 = 0.01,
        N_G2 = 10,
        icc2 = 0.01,
        numb_fe1 = 10,
        numb_fe2 = 10,
        seed = 71986045
      )
    #sapplydata1, class)
    
    lm_fit <-
      lm(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense,
        data = data1
      )
    feols_fit1 <-
      fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense,
        data = data1
      )
    feols_fit2 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense,
        data = data1,
        cluster = "group_id1"
      )
    feols_fit3 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense,
        data = data1,
        cluster = ~group_id1
      )
    feols_fit4 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense,
        data = data1,
        cluster = c("group_id1", "group_id2")
      )
    feols_fit5 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense,
        data = data1,
        cluster = ~ group_id1 + group_id2
      )
    feols_fit6 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income |
          Q1_immigration + Q2_defense,
        data = data1
      )
    feols_fit7 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income |
          Q1_immigration + Q2_defense,
        data = data1,
        cluster = "group_id1"
      )
    feols_fit8 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income |
          Q1_immigration + Q2_defense,
        data = data1,
        cluster = ~group_id1
      )
    feols_fit9 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income |
          Q1_immigration + Q2_defense,
        data = data1,
        cluster = c("group_id1", "group_id2")
      )
    feols_fit10 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income |
          Q1_immigration + Q2_defense,
        data = data1,
        cluster = ~ group_id1 + group_id2
      )
    feols_fit11 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration |
          Q2_defense,
        data = data1
      )
    feols_fit12 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration,
        fixef = "Q2_defense",
        data = data1
      )
    feols_fit13 <-
     fixest::feols(
        proposition_vote ~ treatment + ideology1 + log_income,
        fixef = c("Q1_immigration", "Q2_defense"),
        data = data1
      )
    
    # can also assign fixed effects via fixef = c("") arguments ...
    
    felm_fit1 <-
     lfe::felm(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense,
        data = data1
      )
    felm_fit2 <-
     lfe::felm(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense |
          0 | 0 | group_id1,
        data = data1
      )
    felm_fit3 <-
     lfe::felm(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration + Q2_defense |
          0 | 0 | group_id1 + group_id2,
        data = data1
      )
    felm_fit4 <-
     lfe::felm(
        proposition_vote ~ treatment + ideology1 + log_income |
          Q1_immigration + Q2_defense | 0 | 0,
        data = data1
      )
    felm_fit5 <-
     lfe::felm(
        proposition_vote ~ treatment + ideology1 + log_income |
          Q1_immigration + Q2_defense | 0 | group_id1,
        data = data1
      )
    felm_fit6 <-
     lfe::felm(
        proposition_vote ~ treatment + ideology1 + log_income |
          Q1_immigration + Q2_defense | 0 | group_id1 + group_id2,
        data = data1
      )
    felm_fit7 <-
     lfe::felm(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q1_immigration |
          Q2_defense | 0 | group_id1 + group_id2,
        data = data1
      )
    felm_fit8 <-
     lfe::felm(
        proposition_vote ~ treatment + ideology1 + log_income +
          Q2_defense |
          Q1_immigration | 0 | group_id1 + group_id2,
        data = data1
      )
    
    
    # one-way clustering
    
    create_models <- function(clustid) {
      R1 <- clubSandwich::constrain_zero(2:3, coefs = coef(lm_fit))
      R2 <- matrix(c(1, 0, 0, 1, 0, 0), 2, 3)
      R3 <- clubSandwich::constrain_zero(1:2, coefs = coef(felm_fit8))
      R4 <- clubSandwich::constrain_zero(1:2, coefs = coef(felm_fit7))
      
      for (engine in c("R", "WildBootTests.jl")) {
        # boottest()
        cat("boottest()", "\n")
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_lm_", engine),
               suppressWarnings(
                 boottest(
                   object = lm_fit,
                   clustid = clustid,
                   B = 19999,
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest1_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit1,
                   clustid = clustid,
                   B = 19999,
                   param = ~treatment,
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest2_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit2,
                   clustid = clustid,
                   B = 19999,
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest3_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit3,
                   clustid = clustid,
                   B = 19999,
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest4_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit4,
                   clustid = clustid,
                   B = 19999,
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest5_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit5,
                   clustid = clustid,
                   B = 19999,
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest6_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit6,
                   clustid = clustid,
                   B = 19999,
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest7_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit7,
                   clustid = clustid,
                   B = 19999,
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest8_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit8,
                   clustid = clustid,
                   B = 19999,

                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest9_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit9,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest10_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit10,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest11_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit11,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest12_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit12,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest13_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit13,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest6fe_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit10,
                   clustid = clustid,
                   fe = ~Q1_immigration,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest7fe_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit10,
                   clustid = clustid,
                   fe = "Q1_immigration",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest8fe_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit10,
                   clustid = clustid,
                   fe = "Q1_immigration",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest9fe_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit10,
                   clustid = clustid,
                   fe = "Q1_immigration",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest10fe_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit10,
                   clustid = clustid,
                   fe = "Q1_immigration",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        # why suddenly fe = Q2_defense? Should give the same models
        assign(paste0("boot_fixest11fe_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit11,
                   clustid = clustid,
                   fe = "Q2_defense",
                   B = 19999,
                   
                   param = ~treatment,
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest12fe_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit12,
                   clustid = clustid,
                   fe = "Q2_defense",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_fixest13fe_", engine),
               suppressWarnings(
                 boottest(
                   object = feols_fit13,
                   clustid = clustid,
                   fe = "Q2_defense",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm1_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit1,
                   clustid = clustid,
                   B = 19999,
                   
                   param = ~treatment,
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm2_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit2,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm3_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit3,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm4_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit4,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm5_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit5,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm6_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit6,
                   clustid = clustid,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm4fe_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit4,
                   clustid = clustid,
                   fe = ~Q1_immigration,
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm5fe_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit5,
                   clustid = clustid,
                   fe = "Q1_immigration",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm6fe_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit6,
                   clustid = clustid,
                   fe = "Q1_immigration",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        set.seed(8965)
        dqrng::dqset.seed(765)
        
        assign(paste0("boot_felm7fe_", engine),
               suppressWarnings(
                 boottest(
                   object = felm_fit7,
                   clustid = clustid,
                   fe = "Q2_defense",
                   B = 19999,
                   
                   param = "treatment",
                   conf_int = TRUE,
                   ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                   engine = engine,
                   floattype = "Float64"
                 )
               ),
               envir = .GlobalEnv
        )
        
        # mboottest()
        
        if (engine == "WildBootTests.jl") {
          cat("mboottest()", "\n")
          
          set.seed(86908)
          assign(paste0("wboot_lm_", engine),
                 suppressWarnings(
                   mboottest(
                     object = lm_fit,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_fixest1_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit1,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          set.seed(86908)
          
          assign(paste0("wboot_fixest2_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit2,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_fixest3_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit3,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_fixest4_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit4,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_fixest5_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit5,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          # why lm_fit? Because mboottest() estimates without fixed
          # effects, so R needs to be of dimension of
          # length(names(coef(object)))
          
          set.seed(86908)
          
          assign(paste0("wboot_fixest6_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit6,
                     R = R2,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_fixest7_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit7,
                     R = R2,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_fixest8_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit8,
                     R = R2,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_fixest9_", engine),
                 suppressWarnings(
                   mboottest(
                     object = feols_fit9,
                     R = R2,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(
            paste0("wboot_fixest10_", engine),
            suppressWarnings(
              mboottest(
                object = feols_fit10,
                R = R2,
                clustid = clustid,
                B = 19999,
                
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                floattype = "Float64"
              )
            ),
            envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(
            paste0("wboot_fixest6fe_", engine),
            suppressWarnings(
              mboottest(
                object = feols_fit6,
                R = R2,
                clustid = clustid,
                fe = "Q1_immigration",
                B = 19999,
                
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                floattype = "Float64"
              )
            ),
            envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(
            paste0("wboot_fixest7fe_", engine),
            suppressWarnings(
              mboottest(
                object = feols_fit7,
                R = R2,
                clustid = clustid,
                fe = "Q1_immigration",
                B = 19999,
                
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                floattype = "Float64"
              )
            ),
            envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(
            paste0("wboot_fixest8fe_", engine),
            suppressWarnings(
              mboottest(
                object = feols_fit8,
                R = R2,
                clustid = clustid,
                fe = "Q1_immigration",
                B = 19999,
                
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                floattype = "Float64"
              )
            ),
            envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(
            paste0("wboot_fixest9fe_", engine),
            suppressWarnings(
              mboottest(
                object = feols_fit9,
                R = R2,
                clustid = clustid,
                fe = "Q1_immigration",
                B = 19999,
                
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                floattype = "Float64"
              )
            ),
            envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(
            paste0("wboot_fixest10fe_", engine),
            suppressWarnings(
              mboottest(
                object = feols_fit10,
                R = R2,
                clustid = clustid,
                fe = "Q1_immigration",
                B = 19999,
                
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                floattype = "Float64"
              )
            ),
            envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          # why suddenly fe = Q2_defense? Should give the same models
          assign(
            paste0("wboot_fixest11fe_", engine),
            suppressWarnings(
              mboottest(
                object = feols_fit11,
                R = R3,
                clustid = clustid,
                fe = "Q2_defense",
                B = 19999,
                
                ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                floattype = "Float64"
              )
            ),
            envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm1_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit1,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm2_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit2,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm3_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit3,
                     R = R1,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm4_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit4,
                     R = R2,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm5_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit5,
                     R = R2,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm6_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit6,
                     R = R2,
                     clustid = clustid,
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm4fe_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit4,
                     R = R2,
                     clustid = clustid,
                     fe = "Q1_immigration",
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm5fe_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit5,
                     R = R2,
                     clustid = clustid,
                     fe = "Q1_immigration",
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm6fe_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit6,
                     R = R2,
                     clustid = clustid,
                     fe = "Q1_immigration",
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
          
          set.seed(86908)
          
          assign(paste0("wboot_felm7fe_", engine),
                 suppressWarnings(
                   mboottest(
                     object = felm_fit7,
                     R = R3,
                     clustid = clustid,
                     fe = "Q2_defense",
                     B = 19999,
                     
                     ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE),
                     floattype = "Float64"
                   )
                 ),
                 envir = .GlobalEnv
          )
        }
      }
    }
    
    run_tests <- function() {
      for (x in c("point_estimate", "p_val", "t_stat", "conf_int")) {
        cat("test boottest", "\n")
        # test all R estimates against each other,
        # then test all Julia estimates against each other
        
        cat(x, "\n")
        # test R models
        for (model in all_feols_felm_models_R) {
          if (print_results) {
            print(expect_equal(boot_lm_R[[x]], get(model)[[x]],
                               ignore_attr = TRUE
            ))
          } else {
            expect_equal(boot_lm_R[[x]], get(model)[[x]],
                         ignore_attr = TRUE
            )
          }
        }
        
        # test Julia models
        for (model in all_feols_felm_models_jl) {
          if (print_results) {
            print(expect_equal(
              boot_lm_WildBootTests.jl[[x]],
              get(model)[[x]],
              ignore_attr = TRUE
            ))
          } else {
            expect_equal(boot_lm_WildBootTests.jl[[x]],
                         get(model)[[x]],
                         ignore_attr = TRUE
            )
          }
        }
        
        # different seeds -> different values
        if (print_results) {
          print(expect_equal(
            boot_lm_R[[x]],
            as.vector(boot_lm_WildBootTests.jl[[x]]),
            tolerance = 0.02,
            ignore_attr = TRUE
          ))
        } else {
          expect_equal(
            boot_lm_R[[x]],
            as.vector(boot_lm_WildBootTests.jl[[x]]),
            tolerance = 0.02,
            ignore_attr = TRUE
          )
        }
      }
      
      cat("test mboottest", "\n")
      
      # test wald models
      for (x in c("p_val", "teststat")) {
        for (model in all_wald_models) {
          if (print_results) {
            print(expect_equal(
              wboot_lm_WildBootTests.jl[[x]],
              get(model)[[x]],
              ignore_attr = TRUE
            ))
          } else {
            expect_equal(
              wboot_lm_WildBootTests.jl[[x]],
              get(model)[[x]],
              tolerance = 0.02,
              ignore_attr = TRUE
            )
          }
        }
      }
    }
    
    all_feols_felm_models_R <- c(
      paste0("boot_fixest", 1:13, "_R"),
      paste0("boot_fixest", 6:13, "fe_R"),
      paste0("boot_felm", 1:6, "_R"),
      paste0("boot_felm", 4:7, "fe_R")
    )
    
    all_feols_felm_models_jl <- c(
      paste0("boot_fixest", 1:13, "_WildBootTests.jl"),
      paste0("boot_fixest", 6:11, "fe_WildBootTests.jl"),
      paste0("boot_felm", 1:6, "_WildBootTests.jl"),
      paste0("boot_felm", 4:7, "fe_WildBootTests.jl")
    )
    
    all_wald_models <- c(
      paste0("wboot_fixest", 1:10, "_WildBootTests.jl"),
      paste0("wboot_fixest", 6:11, "fe_WildBootTests.jl"),
      paste0("wboot_felm", 1:6, "_WildBootTests.jl"),
      paste0("wboot_felm", 4:7, "fe_WildBootTests.jl")
    )
    # one-way clustering
    create_models(clustid = "group_id1")
    run_tests()
    
    
    # two-way clustering
    
    create_models(clustid = c("group_id1", "group_id2"))
    run_tests()

    
  })



test_that("clustid can be fe", {
  

  print_results <- FALSE
  
  data1 <<-
    fwildclusterboot:::create_data(
      N = 10000,
      N_G1 = 20,
      icc1 = 0.01,
      N_G2 = 10,
      icc2 = 0.01,
      numb_fe1 = 10,
      numb_fe2 = 10,
      seed = 71986045
    )
  #sapplydata1, class)
  
  feols_fit1 <-
   fixest::feols(
      proposition_vote ~ treatment + ideology1 + log_income | group_id1,
      data = data1,
      cluster = ~group_id1
    )
  
  felm_fit1 <-
   lfe::felm(proposition_vote ~ treatment + ideology1 + log_income |
           group_id1,
         data = data1
    )
  
  fit1 <-
    boottest(
      feols_fit1,
      param = "treatment",
      B = 999,
      clustid = "group_id1",
      fe = "group_id1",
      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
    )
  
  fit2 <-
    boottest(
      felm_fit1,
      param = "treatment",
      B = 999,
      clustid = "group_id1",
      fe = "group_id1",
      ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE)
    )
  
  expect_equal(fit1$t_stat,
               fixest::tstat(feols_fit1, ssc = fixest::ssc(
                 adj = FALSE, cluster.adj = FALSE
               ))["treatment"],
               ignore_attr = TRUE
  )
  
  expect_equal(fit2$t_stat,
               fixest::tstat(feols_fit1, ssc = fixest::ssc(
                 adj = FALSE, cluster.adj = FALSE
               ))["treatment"],
               ignore_attr = TRUE
  )

})
