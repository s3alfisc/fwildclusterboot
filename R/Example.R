# ==================================================================== #
# Example: 
# - Estimate a model without any clusters and demonstrates that the 
#   wild cluster bootstrap implemented replicates the expected p-values
#   based on sandwich covariance matrices
# - note: the function now works for arbitrary N and number of params k
# - also note: the larger the numbers of clusters, the slower the 
#   function is. Real speed gains will be achieved if the number of 
#   clusters is small
# ==================================================================== #


library(data.table)
library(estimatr)
library(magrittr)

N <- 1000
numb_clusters <- 100

x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
x4 <- rnorm(N)
error <- rnorm(N)

y <- 0.05*x1 - 0.02*x2 + 0.5*x3 + x4 + error

data <- data.table(y = y, 
                   x1 = x1, 
                   x2 = x2, 
                   x3 = x3, 
                   x4 = x4, 
                   cluster = 1:length(y))


lm_fit <- lm_robust(y ~ x1 + x2 + x3 + x4, data = data, clusters = 1:nrow(data))
lm_fit %>% 
  summary()

boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "(Intercept)")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x1")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x2")
boottest(lm_fit, 1:2000, B = 1000, seed = 1, param = "x3")



