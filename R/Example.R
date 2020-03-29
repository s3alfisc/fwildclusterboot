# ==================================================================== #
# Example: 
# - Estimate a model without any clusters and demonstrates that the 
#   wild cluster bootstrap implemented replicates the expected p-values
#   based on sandwich covariance matrices + New Test for truly clustered
#   data
# - note: the function now works for arbitrary N and number of params k
# - also note: the larger the numbers of clusters, the slower the 
#   function is. Real speed gains will be achieved if the number of 
#   clusters is small
# - there is still smth wrong with the p-value for the intercept in 
#   the second example
# - also effectively I broke the first example
# ==================================================================== #


# library(data.table)
# library(estimatr)
# library(magrittr)
# library(mvtnorm)
# 
# N <- 1000
# 
# x1 <- rnorm(N)
# x2 <- rnorm(N)
# x3 <- rnorm(N)
# x4 <- rnorm(N)
# error <- rnorm(N)
# 
# y <- 1 + 0.05*x1 - 0.02*x2 + 0.5*x3 + x4 + error
# 
# data <- data.table(y = y, 
#                    x1 = x1, 
#                    x2 = x2, 
#                    x3 = x3, 
#                    x4 = x4, 
#                    cluster = 1:length(y))
# 
# # just heteroskedasticity robust
# lm_fit <- lm_robust(y ~ x1 + x2 + x3 + x4 , data = data)
# lm_fit %>% 
#   summary()
# 
# boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "(Intercept)")
# boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x1")
# boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x2")
# boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x3")
# boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x4")
# boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x5male")




# Now add an example with actual clustered errors

seed = sample(1:1000, 1)

gen_cluster <- function(param = c(.1, .01), n = 10000, n_cluster = 50, rho = .8, seed = seed) {
  # Function to generate clustered data
  # Required package: mvtnorm
  
  # individual level
  Sigma_i <- diag(length(param)) * rho
  #matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i <- rmvnorm(n = n, sigma = Sigma_i)
  
  # cluster level
  cluster_name <- rep(1:n_cluster, each = n / n_cluster)
  Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = length(param))
  values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
  
  # predictor var consists of individual- and cluster-level components
  x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
  
  # error consists of individual- and cluster-level components
  error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
  
  # data generating process
  y <- 1 + param[2]*x + error
  
  df <- data.frame(x, y, cluster = cluster_name)
  return(df)
}

data <- gen_cluster()

lm_fit <- lm_robust(y ~ x, data = data, clusters = cluster)
lm_fit %>% 
  summary()

boottest(lm_fit, clustid = data$cluster, B = 1000, seed = seed, param = "(Intercept)")
boottest(lm_fit, clustid = data$cluster, B = 1000, seed = seed, param = "x")












