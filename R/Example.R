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


library(data.table)
library(estimatr)
library(magrittr)
library(mvtnorm)
library(multiwayvcov)
library(lmtest)
library(lfe)

# 
N <- 4000
# 
x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
x4 <- rnorm(N)
error <- rnorm(N)
# 
y <- 1 + 0.05*x1 - 0.02*x2 + 0.5*x3 + x4 + error
# 
data <- data.table(y = y, 
                  x1 = x1, 
                  x2 = x2, 
                  x3 = x3, 
                  x4 = x4, 
                  cluster = 1:N)
# 
# # just heteroskedasticity robust
lm_fit <- lm(y ~ x1 + x2 + x3 + x4, data = data)
lm_robust(y ~ x1 + x2 + x3 + x4 , data = data) %>% 
 summary()

# note that this part is rather slow, as a speed gains mainly for few clusters
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "(Intercept)")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x1")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x2")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x3")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x4")





# Now add an example with actual clustered errors

seed = sample(1:1000, 1)

gen_cluster <- function(param = c(1, 0), n = 10000, n_cluster = 20, rho = .8) {
  # source: https://yukiyanai.github.io/teaching/rm1/contents/R/clustered-data-analysis.html
  # Function to generate clustered data
  # Required package: mvtnorm
  
  # individual level
  Sigma_i <- matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i <- rmvnorm(n = n, sigma = Sigma_i)
  
  # cluster level
  cluster_name <- rep(1:n_cluster, each = n / n_cluster)
  Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
  
  # predictor var consists of individual- and cluster-level components
  x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
  
  # error consists of individual- and cluster-level components
  error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
  
  # data generating process
  y <- param[1] + param[2]*x + error
  
  df <- data.frame(x, y, cluster = cluster_name)
  data.table::setDT(df)
  return(df)
}

data <- gen_cluster()
head(data)
data[, mean(y)]


lm_fit <- lm(y ~ x, data = data)
lm_fit %>% 
  summary()

# standard bootstrap
B <- 2000

# basic bootstrap, not parallel
system.time(boot_fit <- multiwayvcov::cluster.boot(lm_fit, 
                          as.factor(data$cluster), 
                          R = B, 
                          boot_type = "residual", 
                          wild_type = "rademacher", 
                          parallel = TRUE))


system.time(
  boottest(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
)

lmtest::coeftest(lm_fit, boot_fit)

boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)")
boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")

lm_robust_fit <- lm_robust(y ~ x, data = data, clusters = cluster)
lm_robust_fit %>% 
  summary()
# does the method work with object lm_robust?
boottest.lm_robust(lm_robust_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)") # error: need to feed in data
boottest.lm_robust(lm_robust_fit, data = data, clustid = data$cluster, B = B, seed = seed, param = "x") 


felm_fit <- lfe::felm(y ~ x1 | x2 | 0 | data$cluster, data = data)
summary(felm_fit)

# felm_fit$coefficients
# felm_fit$formula
# felm_fit$response %>% head()
# felm_fit$clustervar
# felm_fit$keepCX



