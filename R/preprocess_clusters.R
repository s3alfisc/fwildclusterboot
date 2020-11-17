# 
# library("lmtest")
# library("multiwayvcov")
# 
# data(petersen)
# petersen <- petersen[1:100, ]
# set.seed(123)
# petersen[ sample(1:100, 15), 3] <- NA
# colMeans(sapply(petersen, is.na))
# 
# lm_fit <- lm(y ~ x, data = petersen)
# summary(lm_fit)

preprocess_clusters <- function(model, cluster, debug = FALSE){

  if (inherits(cluster, "formula")) {
    cluster_tmp <- expand.model.frame(model, cluster, na.expand = FALSE)
    cluster <- model.frame(cluster, cluster_tmp, na.action = na.pass)
  }
  else {
    cluster <- as.data.frame(cluster, stringsAsFactors = FALSE)
  }
  
  cluster_dims <- ncol(cluster)
  tcc <- 2^cluster_dims - 1
  acc <- list()
  
  for (i in 1:cluster_dims) {
    acc <- append(acc, combn(1:cluster_dims, i, simplify = FALSE))
  }
  if (debug){print(acc)}
  
  acc <- acc[-1:-cluster_dims]
  
  if(debug){print(acc)}
  
  if (!is.null(model$na.action)) {
    if (class(model$na.action) == "exclude") {
      cluster <- cluster[-model$na.action, ]
    }
    else if (class(model$na.action) == "omit") {
      cluster <- cluster[-model$na.action, ]
    }
    cluster <- as.data.frame(cluster)
  }
  
  if (debug) 
    print(class(cluster))
  i <- !sapply(cluster, is.numeric)
  cluster[i] <- lapply(cluster[i], as.character)
  if (cluster_dims > 1) {
    for (i in acc) {
      cluster <- cbind(cluster, Reduce(paste0, cluster[, 
                                                       i]))
    }
  }
  cluster
}

# dim(test_envir(lm_fit, ~firmid + year))
# test_envir(lm_fit, ~firmid + year)
# 
# petersen[sample(1:100, 3), "firmid"] <- NA
# lm_fit <- lm(y ~ x, data = petersen)
# dim(test_envir(lm_fit, ~firmid))
# sum(is.na(test_envir(lm_fit, ~firmid)))
# 
# test_envir(lm_fit, ~firmid + year)

