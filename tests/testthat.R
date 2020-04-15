library(testthat)
library(fwildclusterboot)

test_check("fwildclusterboot")

test_that("test dimensions mat_mean_by_cluster"{
  
  N <- 1000
  k <- 5
  X <- matrix(rnorm(N * k), N, k)
  clustid <- sample(1:3, N, replace = TRUE)
  unique_clusters <- length(unique(clustid))
  
  expect_equal(dim(mat_mean_by_cluster(prod = X, clustid = clustid)), c(unique_clusters,k))
  
})


