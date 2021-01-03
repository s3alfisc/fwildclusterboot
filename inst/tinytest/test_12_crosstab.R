# Test 12: test crosstab 

a <- sample(1:10, 10, replace = TRUE)
b <- sample(1:4, 10, replace = TRUE)
y <- matrix(rnorm(10), 10, 1)
data <- y
var1 <- data.frame(a = a)
var2 <- data.frame(b = b)

ct2 <- crosstab2(data = data, var1 = var1, var2 = var2)
#ct3 <- crosstab3(data = data, var1 = var1, var2 = var2)
ct4 <- crosstab4(data = data, var1 = var1, var2 = var2)

expect_equal(ct2, ct4)


  