secant_rel <- 
  function (f, x1, x2, B, num = 10, eps = 1e-06){
  
  #' secant method 
  #' function adopted from NLRoot::SMfzero
  
  B_crit <- 1 / B * 1.0000001  
  i = 0
  while (abs( (x1 - x2) / (f(x1) - f(x2)) ) > eps & abs( (x1 - x2) / (f(x1) - f(x2)) ) > B_crit & (i < num)) {
    c = x2 - f(x2) * (x2 - x1)/(f(x2) - f(x1))
    x1 = x2
    x2 = c
    i = i + 1
  }
  res <- list(root = x2)
  res
  # print(x2)
  # print(f(x2))
  # if (abs(f(x2)) < eps1) {
  #   print("finding root is successful")
  # }
  # else print("finding root is fail")
  }
