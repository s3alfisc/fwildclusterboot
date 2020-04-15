mat_mean_by_cluster <- function(prod, clustid){
  
  mat_prep <- data.table::data.table(prod = prod , clustid = clustid) 
  mat_u <- as.matrix(mat_prep[, lapply(.SD, sum), by = "clustid"][
    , clustid := NULL])
  
  mat_u
  
}
