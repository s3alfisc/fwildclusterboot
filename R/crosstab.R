
crosstab <- function(data, groups, rename_var){
  #' function to calculates crosstabs
  #' data
  #' 
  #data <- crosstab_XinvXXRu_prep
  #var <- "prod.v1"
  
  
  setDT(data)
  setkeyv(data, groups)
  group1 <- groups[1]
  group2 <- groups[2]
  
  res <- data[CJ(get(group1), get(group2), unique = TRUE), lapply(.SD, sum), by = .EACHI]
  
  setnames(res, rename_var, "prod")
  
  group1 <- length(unique(data[, get(groups[1])]))
  group2 <- length(unique(data[, get(groups[2])]))
  
  res_mat <- matrix(res$prod, group1, group2)
  
  # grid <- expand.grid(group1 = group1, group2 = group2)
  # res <- merge(grid, tab, by = groups, all.x = TRUE)
  
  res_mat[is.na(res_mat)] <- 0
  
  res_mat
}

#crosstab(data = data, groups = c("clustid.clustid", "fe.fixed_effect_1"))
