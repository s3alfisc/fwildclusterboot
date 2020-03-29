#' Fast Wild Cluster Bootstrap
#' 
#' This function conducts a fast wild cluster bootstrap as in Cameron et al... 
#' 
#' @param object An regression object of type lm or lm_robust. 
#' @param clustid A vector of clusters. 
#' @param param The coefficient to be tested. 
#' @param B The number of Bootstrap iterations. 
#' @param debug If TRUE, sets debugging on. 
#' @param seed Sets a specified seed. 

#'@export
boottest <- function(object, 
                     clustid, 
                     param, 
                     B,
                     debug = FALSE, 
                     seed = NULL) {
  
  if(!is.null(seed)){
    set.seed(seed)
  } else if(is.null(seed)){
    set.seed(1)
  }
  
  # retrieve clusters / multiple clusters
  if(inherits(clustid, "formula")) {
    clustid_tmp <- expand.model.frame(model, clustid, na.expand = FALSE)
    clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
  } else {
    clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
  }
  
  if(!(param %in% c(names(object$coefficients)))){
    warning("Parameter to test not in model or all. Please specify appropriate parameters to test.")
  }
  
  # how many clustids? uniway/multiway?
  clustid_dims <- ncol(clustid)
  
  
  # Handle omitted or excluded observations
  if(!is.null(object$na.action)) {
    if(class(object$na.action) == "exclude") {
      clustid <- clustid[-object$na.action,]
    } else if(class(object$na.action) == "omit") {
      clustid <- clustid[-object$na.action,]
    }
    clustid <- as.data.frame(clustid)  # silly error somewhere
  }
  if(debug) print(class(clustid))
  
  # Factors in our clustiding variables can potentially cause problems
  # Blunt fix is to force conversion to characters
  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)
  
  # Make all combinations of clustid dimensions
  if(clustid_dims > 1) {
    for(i in acc) {
      clustid <- cbind(clustid, Reduce(paste0, clustid[,i]))
    }
  }
  

  
  N <- ncol(Y)
  
  
  # start estimation here: 
  
  R0 <- as.numeric(param == names(object$coefficients))
  
  Y <- model.frame(object)[, 1]
  X <- model.matrix(object)
  Xr <- model.matrix(object)[, which(names(object$coefficients) != param)]
  
  clustid <- 1:nrow(data)
  #clustid <- rep(1:20, 100)
  N_G <- length(unique(clustid)) #number of clusters
  if(N_G > 2000){
    warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
  }
  
  # error under the null hypothesis
  u_hat <- Y - Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr) %*% Y # N x 1 matrix
  
  invXX <- solve(t(X) %*% X) # k x k matrix
  
  v <- matrix(sample(c(1, -1), N_G * (B + 1), replace = TRUE), N_G, B + 1) # rademacher weights for all replications
  v[,1] <- 1
  
  XinvXXr <- X %*% (invXX %*% R0) # N x 1
  SXinvXXRu_prep <- data.table(prod = XinvXXr * matrix(rep(u_hat, 1), 2000, 1) , clustid = clustid) 
  SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid"][, clustid := NULL])
  
  if(ncol(SXinvXXRu) == 1){
    SXinvXXRu <- as.vector(SXinvXXRu)
  }
  
  SXinvXXRX_prep <- data.table(prod = matrix(rep(XinvXXr, 4), 2000, 4) * X, clustid = clustid)
  SXinvXXRX <- as.matrix(SXinvXXRX_prep[, lapply(.SD, sum), by = "clustid"][, clustid := NULL])
  
  SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, 4), 2000, 4), clustid = clustid) 
  SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid"][, clustid := NULL])
  
  numer <- SXinvXXRu %*% v
  J <- (diag(SXinvXXRu) - SXinvXXRX  %*% invXX %*% t(SXu)) %*% v  
  
  t <- abs(numer)  / sqrt(colSums(J * J))       
  
  p_val <- mean(t[1] < t[2:(B + 1)])
  
  paste("The wild cluster bootstrap p-value for the parameter", param, "is", p_val, ",", "with B", B,  "bootstrap iterations.")
  
  
} 
