test_that("multiplication works", {
  library(fixest)
  library(lfe)
  library(fwildclusterboot)
  
  
  data1 <<- fwildclusterboot:::create_data(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 7645)
  sapply(data1, class)
  
  lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense ,
               data = data1)
  feols_fit1 <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
                      data = data1)
  feols_fit2 <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
                      data = data1,
                      cluster = "group_id1")
  feols_fit3 <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
                      data = data1,
                      cluster = ~ group_id1)
  feols_fit4 <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
                      data = data1,
                      cluster = c("group_id1", "group_id2"))
  feols_fit5 <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
                      data = data1,
                      cluster = ~ group_id1 + group_id2)
  feols_fit6 <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                      data = data1)
  feols_fit7 <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                      data = data1,
                      cluster = "group_id1")
  feols_fit8 <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                      data = data1,
                      cluster = ~ group_id1)
  feols_fit9 <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration+ Q2_defense,
                      data = data1,
                      cluster = c("group_id1", "group_id2"))
  feols_fit10 <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense,
                       data = data1,
                       cluster = ~ group_id1 + group_id2)
  feols_fit11 <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | Q2_defense,
                       data = data1)
  
  # can also assign fixed effects via fixef = c("") arguments ...
  
  felm_fit1 <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense,
                    data = data1)
  felm_fit2 <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense | 0 | 0 | group_id1,
                    data = data1)
  felm_fit3 <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration + Q2_defense | 0 | 0 | group_id1 + group_id2,
                    data = data1)
  felm_fit4 <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense | 0 | 0,
                    data = data1)
  felm_fit5 <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration + Q2_defense | 0 | group_id1,
                    data = data1)
  felm_fit6 <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration  + Q2_defense| 0 | group_id1 + group_id2,
                    data = data1)
  felm_fit7 <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration  | Q2_defense| 0 | group_id1 + group_id2,
                    data = data1)
  felm_fit8 <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q2_defense  | Q1_immigration| 0 | group_id1 + group_id2,
                    data = data1)
  
  
  R_no_fixef <- clubSandwich::constrain_zero(2:3, coefs = coef(lm_fit)) * 0.1
  R_Q1 <- clubSandwich::constrain_zero(1:2, coefs = coef(felm_fit8)) * 0.1
  R_Q2 <- clubSandwich::constrain_zero(1:2, coefs = coef(felm_fit7)) * 0.1
  R_Q22 <- clubSandwich::constrain_zero(1:2, coefs = coef(feols_fit11)) * 0.1
  
  #R_Q1Q2 <- clubSandwich::constrain_zero(2:3, coefs = coef(felm_fit5))
  # one-way clustering
  
  create_models <- function(clustid){
    for(boot_algo in c("R", "WildBootTests.jl")){
      
      # boottest()
      cat("boottest()", "\n")
      assign(paste0("boot_lm_" , boot_algo),  suppressWarnings(boottest(object = lm_fit, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      
      assign(paste0("boot_fixest1_" , boot_algo),suppressWarnings(boottest(object = feols_fit1, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest2_" , boot_algo),suppressWarnings(boottest(object = feols_fit2, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest3_" , boot_algo),suppressWarnings(boottest(object = feols_fit3, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest4_" , boot_algo),suppressWarnings(boottest(object = feols_fit4, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest5_" , boot_algo),suppressWarnings(boottest(object = feols_fit5, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest6_" , boot_algo),suppressWarnings(boottest(object = feols_fit6, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest7_" , boot_algo),suppressWarnings(boottest(object = feols_fit7, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest8_" , boot_algo),suppressWarnings(boottest(object = feols_fit8, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest9_" , boot_algo),suppressWarnings(boottest(object = feols_fit9, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest10_" , boot_algo),suppressWarnings(boottest(object = feols_fit10, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      
      assign(paste0("boot_fixest6fe_" , boot_algo),suppressWarnings(boottest(object = feols_fit10, clustid = clustid, fe = "Q1_immigration", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest7fe_" , boot_algo),suppressWarnings(boottest(object = feols_fit10, clustid = clustid, fe = "Q1_immigration",B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest8fe_" , boot_algo),suppressWarnings(boottest(object = feols_fit10, clustid = clustid, fe = "Q1_immigration",B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest9fe_" , boot_algo),suppressWarnings(boottest(object = feols_fit10, clustid = clustid, fe = "Q1_immigration",B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_fixest10fe_" , boot_algo),suppressWarnings(boottest(object = feols_fit10, clustid = clustid, fe = "Q1_immigration",B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      # why suddenly fe = Q2_defense? Should give the same models
      assign(paste0("boot_fixest11fe_" , boot_algo),suppressWarnings(boottest(object = feols_fit11, clustid = clustid, fe = "Q2_defense",B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      
      assign(paste0("boot_felm1_" , boot_algo),suppressWarnings(boottest(object = felm_fit1, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_felm2_" , boot_algo),suppressWarnings(boottest(object = felm_fit2, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_felm3_" , boot_algo),suppressWarnings(boottest(object = felm_fit3, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_felm4_" , boot_algo),suppressWarnings(boottest(object = felm_fit4, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_felm5_" , boot_algo),suppressWarnings(boottest(object = felm_fit5, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_felm6_" , boot_algo),suppressWarnings(boottest(object = felm_fit6, clustid = clustid, B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      
      assign(paste0("boot_felm4fe_" , boot_algo),suppressWarnings(boottest(object = felm_fit4, clustid = clustid, fe = "Q1_immigration", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_felm5fe_" , boot_algo),suppressWarnings(boottest(object = felm_fit5, clustid = clustid, fe = "Q1_immigration", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_felm6fe_" , boot_algo),suppressWarnings(boottest(object = felm_fit6, clustid = clustid, fe = "Q1_immigration", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      assign(paste0("boot_felm7fe_" , boot_algo),suppressWarnings(boottest(object = felm_fit7, clustid = clustid, fe = "Q2_defense", B = 9999, seed = 911, param = "treatment", conf_int = TRUE, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), boot_algo = boot_algo, floattype = "Float64")), envir = .GlobalEnv)
      
      # waldboottest() 
      
      if(boot_algo == "WildBootTests.jl"){
        cat("waldboottest()", "\n")
        
        assign(paste0("wboot_lm_" , boot_algo),  suppressWarnings(waldboottest(object = lm_fit, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        
        assign(paste0("wboot_fixest1_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit1, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest2_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit2, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest3_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit3, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest4_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit4, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest5_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit5, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        # why lm_fit? Because waldboottest() estimates without fixed effects, so R needs to be of dimension of length(names(coef(object)))
        assign(paste0("wboot_fixest6_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit6, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest7_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit7, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest8_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit8, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest9_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit9, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest10_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit10, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        
        assign(paste0("wboot_fixest6fe_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit6, R = R_Q1, clustid = clustid, fe = "Q1_immigration", B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest7fe_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit7, R = R_Q1, clustid = clustid, fe = "Q1_immigration",B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest8fe_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit8, R = R_Q1, clustid = clustid, fe = "Q1_immigration",B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest9fe_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit9, R = R_Q1, clustid = clustid, fe = "Q1_immigration",B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_fixest10fe_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit10, R = R_Q1, clustid = clustid, fe = "Q1_immigration",B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        # why suddenly fe = Q2_defense? Should give the same models
        assign(paste0("wboot_fixest11fe_" , boot_algo),suppressWarnings(waldboottest(object = feols_fit11, R = R_Q22, clustid = clustid, fe = "Q2_defense",B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        
        assign(paste0("wboot_felm1_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit1, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_felm2_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit2, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_felm3_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit3, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_felm4_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit4, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_felm5_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit5, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_felm6_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit6, R = R_no_fixef, clustid = clustid, B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        
        assign(paste0("wboot_felm4fe_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit4, R = R_Q1, clustid = clustid, fe = "Q1_immigration", B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_felm5fe_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit5, R = R_Q1,clustid = clustid, fe = "Q1_immigration", B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_felm6fe_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit6, R = R_Q1, clustid = clustid, fe = "Q1_immigration", B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        assign(paste0("wboot_felm7fe_" , boot_algo),suppressWarnings(waldboottest(object = felm_fit7, R = R_Q1, clustid = clustid, fe = "Q2_defense", B = 9999, seed = 911, ssc = boot_ssc(adj = FALSE, cluster.adj = FALSE), floattype = "Float64")), envir = .GlobalEnv)
        
      }
      
    }
    
  }
  
  run_tests <- function(){
    for(x in c("point_estimate", "p_val", "t_stat", "conf_int")){
      
      cat("test boottest","\n")
      # test all R estimates against each other,
      # then test all Julia estimates against each other
      
      cat(x, "\n")
      # test R models
      for(model in all_feols_felm_models_R){
        print(expect_equal(boot_lm_R[[x]], get(model)[[x]]), ignore_attr = TRUE)
        # print(fwildclusterboot:::tidy(boot_lm_R), fwildclusterboot:::tidy(get(model)[[x]]))
      }
      
      # test Julia models
      for(model in all_feols_felm_models_jl){
        print(expect_equal(boot_lm_WildBootTests.jl[[x]], get(model)[[x]]), ignore_attr = TRUE)
        # print(fwildclusterboot:::tidy(boot_lm_R), fwildclusterboot:::tidy(get(model)[[x]]))
      }
      
      # different seeds -> different values
      print(expect_equal(boot_lm_R[[x]], as.vector(boot_lm_WildBootTests.jl[[x]]), tol = 0.02), ignore_attr = TRUE)
      
    }
    
    cat("test waldboottest","\n")
    
    # test wald models 
    for(x in c("point_estimate", "p_val", "t_stat")){
      for(model in all_wald_models){
        print(expect_equal(wboot_lm_WildBootTests.jl[[x]], get(model)[[x]]), ignore_attr = TRUE)
      }    
    }
    
    
  }
  
  all_feols_felm_models_R <- c(paste0("boot_fixest", 1:10, "_R"),
                               paste0("boot_fixest", 6:11, "fe_R"),
                               paste0("boot_felm", 1:6, "_R"),
                               paste0("boot_felm", 4:7, "fe_R") 
  )
  
  all_feols_felm_models_jl <- c(paste0("boot_fixest", 1:10, "_WildBootTests.jl"),
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