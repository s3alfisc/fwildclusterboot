library(testthat)
library(fwildclusterboot)

test_check("fwildclusterboot")

# test_that("boottest methods for lm_robust and lm generate the same results", {
#   
#   seed <- sample(1:1000, 1)
#   seed
#   
#   gen_cluster <- function(param = c(1, 0), n = sample(1:1000, 1), n_cluster = sample(1:200, 1), rho = runif(1, -1, 1)) {
#     # source: https://yukiyanai.github.io/teaching/rm1/contents/R/clustered-data-analysis.html
#     # Function to generate clustered data
#     # Required package: mvtnorm
#     
#     # individual level
#     Sigma_i <- matrix(c(1, 0, 0, 1 - rho), ncol = 2)
#     values_i <- rmvnorm(n = n, sigma = Sigma_i)
#     
#     # cluster level
#     cluster_name <- rep(1:n_cluster, each = n / n_cluster)
#     Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
#     values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
#     
#     # predictor var consists of individual- and cluster-level components
#     x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
#     
#     # error consists of individual- and cluster-level components
#     error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
#     
#     # data generating process
#     y <- param[1] + param[2]*x + error
#     
#     df <- data.frame(x, y, cluster = cluster_name)
#     data.table::setDT(df)
#     return(df)
#   }
#   # 
#   data <- gen_cluster()
#   
#   lm_fit <- lm(y ~ x, data = data)
#   lm_fit <- lm_robust(y ~ x, data = data)
#   
#   # standard bootstrap
#   B <- sample(1:5000, 1)
#   
#   
#   
# })