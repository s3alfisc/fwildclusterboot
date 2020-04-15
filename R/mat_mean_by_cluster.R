mat_mean_by_cluster <- function(prod, clustid){
  
  
  if(!is.matrix(prod)){stop("Prod is not an object of type matrix.")}
  if(!is.vector(clustid)){stop("clustid is not an object of type vector.")}
  
  mat_prep <- data.table::data.table(prod = prod , clustid = clustid) 
  mat_u <- as.matrix(mat_prep[, lapply(.SD, sum), by = "clustid"][
    , clustid := NULL])
  
  mat_u
  
}
