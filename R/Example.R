# ==================================================================== #
# Example: 
# - Estimate a model without any clusters and demonstrates that the 
#   wild cluster bootstrap implemented replicates the expected p-values
#   based on sandwich covariance matrices
# - note: so far, the function only takes models with N=2000 obs and 
#   k = 4 parameters
# - also note: the larger the numbers of clusters, the slower the 
#   function is. Real speed gains will be achieved if the number of 
#   clusters is small
# ==================================================================== #


library(data.table)
library(estimatr)
library(magrittr)

N <- 2000
numb_clusters <- 100

x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
error <- rnorm(N)

y <- 0.05*x1 - 0.02*x2 + 0.5*x3 + error

data <- data.table(y = y, 
                   x1 = x1, 
                   x2 = x2, 
                   x3 = x3, 
                   cluster = 1:length(y))


lm_fit <- lm_robust(y ~ x1 + x2 + x3, data = data, clusters = 1:2000)
lm_fit %>% 
  summary()

boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "(Intercept)")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x1")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x2")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x3")



