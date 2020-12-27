crosstab3 <- function(data, var1, var2){
  
  unique_var1 <- as.vector(unique(var1[, names(var1)]))
  unique_var2 <- unique(var2)
  unique_var1_len <- length(unique_var1)
  unique_var2_len <- length(unique_var2)
  
  data <- as.vector(data)
  add_zeros <- nrow(unique(expand.grid(var1 = unique_var1, var2 = unique_var2))) - length(data)
  data <- c(data, rep(0, add_zeros))
  res <- fsum(x = y, g = data.frame(grid))
  dim(res) <- c(unique_var1_len, unique_var2_len)
  res
  
}

crosstab2<- function(data, var1, var2){
  
  # data = XinvXXr
  # var1 = clustid
  # var2 = fixed_effect
  
  dt <- data.table(var1 = var1, var2 = var2, y = data)
  setnames(dt, names(dt), c("var1", "var2", "y"))
  setkeyv(dt, c("var1", "var2"))
  
  dt <- dt[CJ(var1, var2, unique = TRUE), lapply(.SD, sum), by = .EACHI]
  dt[is.na(y), y:=0]
  
  unique_var1 <- as.vector(unique(var1[, names(var1)]))
  unique_var2 <- unique(var2)
  
  unique_var1_len <- length(unique_var1)
  unique_var2_len <- length(unique_var2)
  
  matrix(dt$y, unique_var1_len, unique_var2_len)
  
}


crosstab<- function(data, var1, var2){
  
  # data <- dt
  # var1 <- "a"
  # var2 <- "b"
   # data <- XinvXXr
   # var1 = clustid
   # var2 = fixed_effect

  unique_var1 <- as.vector(unique(var1[, names(var1)]))
  unique_var2 <- unique(var2)
  
  unique_var1_len <- length(unique_var1)
  unique_var2_len <- length(unique_var2)
  df <- expand.grid(unique_var1, unique_var2)
  # create rownames as collapse::fsum
  df <- data.frame(names = paste0(df[, 1], ".", df[, 2]))
  setDT(df)
  res <- fsum(data, cbind(var1,var2))
  res_dt <- data.table(names = rownames(res), values = res)
  final <- merge(df, res_dt, by = "names", all.x = TRUE)
  values <- final[, names:=NULL] 
  values <- matrix(as.matrix(values), unique_var1_len, unique_var2_len)
  values[is.na(values)] <- 0
  values
}


# microbenchmark(
#   crosstab(data = as.matrix(W %*% u_hat), var1 = var1, var2 = var2), 
#   crosstab2(data = as.matrix(W %*% u_hat), var1 = var1, var2 = var2)
# )


# set.seed(1)
# a <- sample(1:5, 10, replace = TRUE)
# b <- sample(1:3, 10, replace = TRUE)
# y <- matrix(rnorm(10), 10, 1)

crosstab4 <- function(data, var1, var2){
  length_var1 <- length(unique(var1))
  length_var2 <- length(unique(var2))
  res <- aggregate(data, list(var1, var2), sum, drop = FALSE)
  #res <- Matrix.utils::aggregate.Matrix(data, list(var1, var2), sum, drop = FALSE)
  res <- res[, c("V1")]
  res[is.na(res)] <- 0 
  dim(res) <- c(length_var1, length_var2)
  res
}

# res <- crosstab4(data = y, var1 = a, var2 = b)
# class(res)
# library(collapse)
# fsum(y, cbind(a, b))
