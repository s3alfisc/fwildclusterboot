test_that("pinv equals MASS::ginv", {
  
  N <- 100
  k <- 4
  X <- matrix(rnorm(N*k), N, k)
  #X <- tcrossprod(X)
  XX <- crossprod(X)
  Xg <- X[1:10,]
  XgXg <- crossprod(Xg)
  
  expect_equal(MASS::ginv(XX - XgXg), pinv(XX - XgXg))  
  
})