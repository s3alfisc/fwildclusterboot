# test_that("test sunab()", {
#   
#   # Simple DiD example
#   data(base_stagg)
#   res_sunab = feols(
#     y ~ x1 + sunab(year_treated, year) | id + year, 
#     base_stagg, 
#     cluster = ~year, 
#     ssc = ssc(adj = TRUE, cluster.adj = TRUE)
#   )
#   
#   #coef_names <- names(coef(res_sunab))
#   coef_names <- "year::4:cohort::4"
#   
#   res <- 
#     lapply(
#     coef_names, 
#     function(x)
#     boottest(
#       res_sunab, 
#       param = x, 
#       B = 999, 
#       clustid = "year"
#     )
#   )
#   
# })