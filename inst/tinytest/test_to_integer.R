# tests the to_integer function

expect_identical(fwildclusterboot:::to_integer(c("a", "b", "c")), 1:3)
expect_identical(fwildclusterboot:::to_integer(1:3), 1:3)
expect_identical(fwildclusterboot:::to_integer(2:4), 1:3)
expect_identical(fwildclusterboot:::to_integer(factor(c(1, 2, 3))), 1:3)


                 