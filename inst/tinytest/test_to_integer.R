# tests the to_integer function

expect_identical(to_integer(c("a", "b", "c")), 1:3)
expect_identical(to_integer(1:3), 1:3)
expect_identical(to_integer(2:4), 1:3)
expect_identical(to_integer(factor(c(1, 2, 3))), 1:3)

df <- data.frame(var1 = c("a", "b"), 
                 var2 = c("b", "c"))
df <- sapply(df, to_integer)
# expect_error(to_integer(c(1, 2, NA)), 1:3)
# expect_error(to_integer(c("1", "2", NA)), 1:3)



                 