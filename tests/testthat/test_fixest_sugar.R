test_that("test fixest formula sugar", {
  
  library(fwildclusterboot)
  library(fixest)
  library(modelsummary)
  
  data(voters)
  
  fit <- fixest::feols(c(income, proposition_vote) ~ treatment, data = voters)
  res <- lapply(fit, \(x) boottest(x, B = 999, param = "treatment", clustid = "group_id1"))  
  summary(res[[1]])
  
  fit <- fixest::feols(proposition_vote ~ sw(treatment, ideology1), data = voters)
  res <- lapply(fit, \(x) boottest(x, B = 999, param = "treatment", clustid = "group_id1"))  
  
  fit <- fixest::feols(proposition_vote ~ i(treatment, ideology1), data = voters)
  res <- boottest(fit, B = 999, param = "treatment::0:ideology1", clustid = "group_id1")
  
  # fit <- fixest::feols(proposition_vote ~ treatment | Q1_immigration^Q2_defense, data = voters)
  # res <- boottest(fit, B = 999, param = "treatment", clustid = "group_id1")
  
})