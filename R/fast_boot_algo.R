fast_boot_algo <- function(numb_clusters, clustid_dims, clustid, XinvXXr, u_hat, v, N, k, X, invXX, mc){
  
  
  if(numb_clusters == 1){
    SXinvXXRu_prep <- data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid) 
    SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    if(ncol(SXinvXXRu) == 1){
      SXinvXXRu <- as.vector(SXinvXXRu)
    }
    numer <- SXinvXXRu %*% v 
    
    # if(use_fixef == FALSE){
    
    # "old" code - if no fixed effects used in calculation
    
    SXinvXXRX_prep <- data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid)
    SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    
    SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
    SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    
    J <- (diag(SXinvXXRu) - SXinvXXRX  %*% invXX %*% t(SXu)) %*% v 
  } else if(numb_clusters == 2){
    
    # numerator
    SXinvXXRu_prep <- data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid[,3]) 
    SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid12"][, clustid.clustid12 := NULL])
    if(ncol(SXinvXXRu) == 1){
      SXinvXXRu <- as.vector(SXinvXXRu)
    }
    numer <- SXinvXXRu %*% v 
    
    KK <- list()
    JJ <- list()
    mc <- 1 / c(1,2,3)
    for(i in 1:clustid_dims){
      
      clustid_use <- as.matrix(clustid)[, i]
      SXinvXXRX_prep <- data.table::data.table(prod = matrix(rep(XinvXXr, k), N, k) * X, clustid = clustid_use)
      SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid"][, clustid := NULL])
      
      SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid_use) 
      SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid"][, clustid := NULL])
      
      SXinvXXRu_prep <- data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid_use) 
      SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid"][, clustid := NULL])
      if(ncol(SXinvXXRu) == 1){
        SXinvXXRu <- as.vector(SXinvXXRu)
      }
      K <- (diag(SXinvXXRu) - SXinvXXRX  %*% invXX %*% t(SXu)) 
      KK[[i]] <- mc[i] * K * K
    }
    
    JJ_sum <- Reduce("+", JJ)
    denom <- sqrt(colSums(JJ_sum))
    
  }
   
  
  
  
}