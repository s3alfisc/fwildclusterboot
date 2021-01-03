crosstab3 <- function(data, var1, var2){
  
  #' Function 3 to calculate crosstabs
  #' @param data A matrix to collapse by two dimensions var1 var2
  #' @param var1 a data.frame containing a single variable 
  #' @param var2 a data.frame containing a single variable
  #' @return A collapsed matrix of dimension length(unique(var1)) x length(unique(var2)). If...
  
  unique_var1 <- as.vector(unique(var1[, names(var1)]))
  unique_var2 <- unique(var2)
  unique_var1_len <- length(unique_var1)
  unique_var2_len <- length(unique_var2)
  
  data <- as.vector(data)
  add_zeros <- nrow(unique(expand.grid(var1 = unique_var1, var2 = unique_var2))) - length(data)
  data <- c(data, rep(0, add_zeros))
  res <- collapse::fsum(x = y, g = data.frame(grid))
  dim(res) <- c(unique_var1_len, unique_var2_len)
  res
  
}

# set.seed(1)
# a <- sample(1:5, 10, replace = TRUE)
# b <- sample(1:3, 10, replace = TRUE)
# y <- matrix(rnorm(10), 10, 1)
# a <- as.data.frame(a)
# b <- as.data.frame(b)
# 
# crosstab2(as.matrix(y), var1 = a, var2 = b)

crosstab2<- function(data, var1, var2){
  
  #' Function 2 to calculate crosstabs
  #' @param data A matrix to collapse by two dimensions var1 var2
  #' @param var1 a data.frame containing a single variable 
  #' @param var2 a data.frame containing a single variable
  #' @return A collapsed matrix of dimension length(unique(var1)) x length(unique(var2)). If...

  # data = XinvXXr
  # var1 = clustid
  # var2 = fixed_effect
  dreamerr::check_arg(var1, "data.frame")
  dreamerr::check_arg(var2, "data.frame")
  
  dt <- data.table(var1 = var1, var2 = var2, y = data)
  setnames(dt, names(dt), c("var1", "var2", "y"))
  setkeyv(dt, c("var1", "var2"))
  
  dt <- dt[CJ(var1, var2, unique = TRUE), lapply(.SD, sum), by = .EACHI]
  dt[is.na(y), y:=0]
  
  unique_var1 <- as.vector(unique(var1[, names(var1)]))
  #unique_var2 <- unique(var2)
  unique_var2 <- as.vector(unique(var2[, names(var2)]))
  
  
  unique_var1_len <- length(unique_var1)
  unique_var2_len <- length(unique_var2)
  
  #dt$y
  matrix(dt$y, unique_var1_len, unique_var2_len, byrow = TRUE)
  
}


crosstab4 <- function(data, var1, var2){
  
  #' Function 4 to calculate crosstabs
  #' @param data A matrix to collapse by two dimensions var1 var2
  #' @param var1 a data.frame containing a single variable 
  #' @param var2 a data.frame containing a single variable
  #' @importFrom stats aggregate
  #' @return A collapsed matrix of dimension length(unique(var1)) x length(unique(var2)). If...
  
  dreamerr::check_arg(var1, "data.frame")
  dreamerr::check_arg(var2, "data.frame")
  
  
  length_var1 <- nrow(unique(var1))
  length_var2 <- nrow(unique(var2))
  res <- aggregate(data, data.frame(var1, var2), sum, drop = FALSE)
  #res <- Matrix.utils::aggregate.Matrix(data, list(var1, var2), sum, drop = FALSE)
  res <- res[, 3]
  res[is.na(res)] <- 0 
  dim(res) <- c(length_var1, length_var2)
  res
}

#crosstab<- function(data, var1, var2){
#  
#  #' Function 1 to calculate crosstabs
#  #' @param data A matrix to collapse by two dimensions var1 var2
#  #' @param var1 a data.frame containing a single variable 
#  #' @param var2 a data.frame containing a single variable
#  #' @return A collapsed matrix of dimension length(unique(var1)) x length(unique(var2)). If...
#  
#  dreamerr::check_arg(var1, "data.frame")
#  dreamerr::check_arg(var2, "data.frame")
#  
#  # data <- dt
#  # var1 <- "a"
#  # var2 <- "b"
#  # data <- XinvXXr
#  # var1 = clustid
#  # var2 = fixed_effect
#  
#  unique_var1 <- as.vector(unique(var1[, names(var1)]))
#  unique_var2 <- unique(var2)
#  
#  unique_var1_len <- length(unique_var1)
#  unique_var2_len <- length(unique_var2)
#  df <- expand.grid(unique_var1, unique_var2)
#  # create rownames as collapse::fsum
#  df <- data.frame(names = paste0(df[, 1], ".", df[, 2]))
#  setDT(df)
#  res <- collapse::fsum(data, cbind(var1,var2))
#  res_dt <- data.table(names = rownames(res), values = res)
#  final <- merge(df, res_dt, by = "names", all.x = TRUE)
#  values <- final[, names:=NULL] 
#  values <- matrix(as.matrix(values), unique_var1_len, unique_var2_len)
#  values[is.na(values)] <- 0
#  values
#}


# a <- sample(1:40, 10000, replace = TRUE)
# b <- sample(1:100, 10000, replace = TRUE)
# y <- matrix(rnorm(10000), 10000, 1)
# data <- y
# var1 <- data.frame(a = a)
# var2 <- data.frame(b = b)
# microbenchmark(crosstab2(data = data, var1 = var1, var2 = var2), crosstab4(data = data, var1 = var1, var2 = var2), times = 20)
