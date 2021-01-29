crosstab2 <- function(data, var1, var2) {

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
  dt[is.na(y), y := 0]

  unique_var1 <- as.vector(unique(var1[, names(var1)]))
  # unique_var2 <- unique(var2)
  unique_var2 <- as.vector(unique(var2[, names(var2)]))


  unique_var1_len <- length(unique_var1)
  unique_var2_len <- length(unique_var2)

  # dt$y
  matrix(dt$y, unique_var1_len, unique_var2_len, byrow = TRUE)
}


crosstab4 <- function(data, var1, var2) {

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
  # res <- Matrix.utils::aggregate.Matrix(data, list(var1, var2), sum, drop = FALSE)
  res <- res[, 3]
  res[is.na(res)] <- 0
  dim(res) <- c(length_var1, length_var2)
  res
}


crosstab <- function(data, var1, var2){
  
  #' collapse way to calculate crosstabs
  #' @param data A matrix to collapse by two dimensions var1 var2
  #' @param var1 a data.frame containing a single variable
  #' @param var2 a data.frame containing a single variable
  #' @return A collapsed matrix of dimension length(unique(var1)) x length(unique(var2)). If...
  
  
  dreamerr::check_arg(var1, "data.frame")
  dreamerr::check_arg(var2, "data.frame")
  
  length_var1 <- nrow(unique(var1))
  length_var2 <- nrow(unique(var2))
  
  res <- 
    collapse::fsum(x = data, g = collapse::qF(var1[, 1]):collapse::qF(var2[, 1]))
  
  res <- matrix(res, length_var1, length_var2, byrow = TRUE)
  res[is.na(res)] <- 0
  res
}

