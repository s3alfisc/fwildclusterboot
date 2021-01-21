# # Test 14: expect warnings or errors
# 
# # -----------------------------------------------------------------------------------------
# # Part 1: fixest
# # Errors
# library(fwildclusterboot)
# res <- 
#   feols(proposition_vote ~ treatment + ideology1 + log_income |Q1_immigration + Q2_defense,
#       weights = NULL,
#       cluster = c("group_id1"), 
#       data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 200, numb_fe2 = 100, seed = 1)
# )
# 
# # when B < 100
# expect_error(boottest(res, B = 50, clustid = "group_id1", param = "treatment"))
# # when param not in model
# expect_error(boottest(res, B = 200, clustid = "group_id1", param = "treatment1"))
# # when fe = cluster var
# expect_error(boottest(res, B = 200, clustid = "group_id1", fe = "group_id1",param = "treatment"))
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), fe = "group_id1",param = "treatment"))
# # when alpha outside 0 and 1
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), param = "treatment", alpha = 2))
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), param = "treatment", alpha = 1))
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), param = "treatment", alpha = 0))
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), param = "treatment", alpha = -1))
# # when weights are on because weights = NULL in function
# #expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), weights = "group_id1", param = "treatment"))
# 
# # error because ^ in formula
# res <- 
#   feols(proposition_vote ~ treatment + ideology1 + log_income |Q1_immigration^Q2_defense,
#         weights = NULL,
#         cluster = c("group_id1"), 
#         data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 200, numb_fe2 = 100, seed = 1)
# )
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), weights = "group_id1", param = "treatment"))
# 
# # error because i() in formula
# res <- 
#   feols(proposition_vote ~ treatment + ideology1 + i(log_income, Q1_immigration) |Q2_defense,
#         weights = NULL,
#         cluster = c("group_id1"), 
#         data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 200, numb_fe2 = 100, seed = 1)
#   )
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), weights = "group_id1", param = "treatment"))
# # error because of [] ...
# # error because of c()
# res <- 
#   feols(c(proposition_vote,log_income) ~ treatment + ideology1 |Q2_defense,
#         weights = NULL,
#         cluster = c("group_id1"), 
#         data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 200, numb_fe2 = 100, seed = 1)
# )
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), weights = "group_id1", param = "treatment"))
# 
# # error due to other function arguments in feols
# res <- 
#   feols(proposition_vote ~ treatment + ideology1 |Q2_defense,
#         weights = NULL,
#         cluster = c("group_id1"), 
#         data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 200, numb_fe2 = 100, seed = 1), 
#         subset = 1:100
# )
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), weights = "group_id1", param = "treatment"))
# # error due to use of split
# # error due to use of fsplit
# # error due to panel.id
# 
# # warnings - mainly in relation to missings
# 
# 
# 
# # -----------------------------------------------------------------------------------------
# # Part 2: lm
# 
# # Test 14: expect warnings or errors
# 
# # -----------------------------------------------------------------------------------------
# # Part 1: fixest
# # Errors
# library(fwildclusterboot)
# res <- 
#   lm(proposition_vote ~ treatment + ideology1 + log_income  +Q1_immigration + Q2_defense,
#         weights = NULL,
#         data = fwildclusterboot:::create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 200, numb_fe2 = 100, seed = 1)
#   )
# 
# # when B < 100
# expect_error(boottest(res, B = 50, clustid = "group_id1", param = "treatment"))
# # when param not in model
# expect_error(boottest(res, B = 200, clustid = "group_id1", param = "treatment1"))
# # when fe = cluster var -> no error because ... fe arguments never evaluated
# #expect_error(boottest(res, B = 200, clustid = "group_id1", fe = "group_id1",param = "treatment"))
# #expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), fe = "group_id1",param = "treatment"))
# # when alpha outside 0 and 1
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), param = "treatment", alpha = 2))
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), param = "treatment", alpha = 1))
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), param = "treatment", alpha = 0))
# expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), param = "treatment", alpha = -1))
# # when weights are on - same issue: weights = NULL
# #expect_error(boottest(res, B = 200, clustid = c("group_id1", "group_id2"), weights = "group_id1", param = "treatment"))

