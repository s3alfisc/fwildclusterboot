# Test 9: compare boot_algo and boot_algo2, multclust
 
   # 1) 
   
   feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
#   felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration | 0 | 0 , weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
   lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = create_data_2(N = 1000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 1234))
   
   preprocess_fixest <- suppressWarnings(fwildclusterboot::preprocess.fixest(object = feols_fit, 
                                                           param = "treatment",
                                                           clustid = c("group_id1", "group_id2"),
                                                           beta0 = 0,
                                                           alpha = 0.05, 
                                                           fe = NULL, 
                                                           seed = 1))
   # preprocess_felm <- suppressWarnings(fwildclusterboot::preprocess.felm(object = felm_fit, 
   #                                                     param = "treatment",
   #                                                     clustid = c("group_id1", "group_id2"),
   #                                                     beta0 = 0,
   #                                                     alpha = 0.05, 
   #                                                     fe = NULL, 
   #                                                     seed = 1))
   preprocess_lm <- suppressWarnings(fwildclusterboot::preprocess.lm(object = lm_fit, 
                                                   param = "treatment",
                                                   clustid = c("group_id1", "group_id2"),
                                                   beta0 = 0,
                                                   alpha = 0.05, 
                                                   seed = 1))
 
   res_fixest <- fwildclusterboot::boot_algo.multclust(preprocess_fixest, B = 1000)
   # res_felm <- fwildclusterboot::boot_algo.multclust(preprocess_felm, B = 1000)
   res_lm <- fwildclusterboot::boot_algo.multclust(preprocess_lm, B = 1000)
   
   res_fixest2 <- fwildclusterboot::boot_algo2.multclust(preprocess_fixest, boot_iter = 1000)
   # res_felm2 <- fwildclusterboot::boot_algo2.multclust(preprocess_felm, boot_iter = 1000)
   res_lm2 <- fwildclusterboot::boot_algo2.multclust(preprocess_lm, boot_iter = 1000)
   
   # check that output contains the same objects - _2 contains pre-computed "ABCD"
   expect_equal(sort(names(res_fixest)), sort(names(res_fixest2)[-which(names(res_fixest2) == "ABCD")]))
  # expect_equal(sort(names(res_felm)), sort(names(res_felm2)[-which(names(res_fixest2) == "ABCD")]))
   expect_equal(sort(names(res_lm)), sort(names(res_lm)[-which(names(res_fixest2) == "ABCD")]))
 
   # 
   #lapply(names(res_felm), function(x) expect_equal(res_fixest[[x]], res_fixest2[[x]]))
   expect_equal(res_fixest[["p_val"]], res_fixest2[["p_val"]])
   #expect_equal(res_felm[["p_val"]], res_felm2[["p_val"]])
   expect_equal(res_fixest[["res_lm"]], res_fixest2[["res_lm2"]])
   
   expect_equal(res_fixest[["t_stat"]], res_fixest2[["t_stat"]])
   #expect_equal(res_felm[["t_stat"]], res_felm2[["t_stat"]])
   expect_equal(res_fixest[["t_stat"]], res_fixest2[["t_stat"]])
   
   # ... test all outputs
   
   