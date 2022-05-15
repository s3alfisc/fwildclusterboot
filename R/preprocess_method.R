#' #' Fast wild cluster bootstrap inference
#' #'
#' #'
#' #' `preprocess_method` is a S3 method that fetches data from several model
#' #' objectects for use with `boottest()`.
#' #'
#' #' @param objectect An objectect of type lm, fixest, felm or ivreg
#' #' @param ... other arguments
#' #'
#' #' @seealso \link[fwildclusterboot]{preprocess_method.lm}, \link[fwildclusterboot]{preprocess_method.fixest}, \link[fwildclusterboot]{preprocess_method.felm}, \link[fwildclusterboot]{preprocess_method.ivreg}
#' #'
#' #' @export
#' #'
#' #' @return An objectect of class \code{preprocess_method}.
#' 
#' preprocess_method <- function(objectect, ...) {
#'   UseMethod("preprocess")
#' }
#' 
#' 
#' 
#' preprocess_method.fixest <- function(object, clustid, clustid_char, R, param, fe, boot_algo, bootcluster){
#'   
#'   call <- object$call
#'   call_env <- attr(object$terms, ".Environment")
#'   fml <- formula(object)
#'   fml <- Formula::as.Formula(fml)
#'   
#'   fml_full <- formula(fml, collapse = TRUE)
#'   all_varnames <- attr( terms(fml), "term.labels")
#'   
#'   N <- nobs(object)
#'   k <- length(coef(object))
#'   
#'   method <- object$family
#'   is_iv <- ifelse(!is.null(object$fml_all$iv), TRUE, FALSE)
#'   has_fe <- ifelse(!is.null(object$fml_all$fixef), TRUE, FALSE)
#'   
#'   if(!is_iv){
#'     X <- model.matrix(object, type = "rhs", na.rm = TRUE, collin.rm = TRUE)
#'   } else {
#'     X_endog <- model.matrix(object, type = "iv.endo", na.rm = TRUE, collin.rm = TRUE)
#'     X_exog <- model.matrix(object, type = "iv.exo", na.rm = TRUE, collin.rm = TRUE)
#'     instruments <- model.matrix(object, type = "iv.inst", na.rm = TRUE, collin.rm = TRUE)
#'   }
#'   
#'   Y <- model.matrix(object, type = "lhs")
#'   
#'   weights <- weights(object)
#'   if(is.null(weights)){
#'     has_weights <- FALSE
#'     weights <- rep(1, N)
#'   } else {
#'     has_weights <- TRUE
#'   }
#'   
#' 
#'   if(has_fe){
#'     get_fe <- transform_fe(object = object, X = X, Y = Y, fe = fe, N = N, has_weights = has_weights, boot_algo = boot_algo)
#'     X <- get_fe$X
#'     Y <- get_fe$Y
#'     fixed_effect <- get_fe$fixed_effect
#'     W <- get_fe$W
#'     n_fe <- get_fe$n_fe
#'     k2 <- get_fe$k2
#'   } else {
#'     fixed_effect <- NULL
#'     k2 <- 0
#'     W <- n_fe <- NULL
#'   }
#' 
#' 
#'   clustid_fml <- reformulate(clustid)
#'   # get cluster variable
#'   if(!is.null(clustid)){
#' 
#'     clustid_list <- get_cluster2(
#'       object = object, 
#'       clustid = clustid_fml,
#'       N = N, 
#'       clustid_char = clustid,
#'       bootcluster = bootcluster
#'     )  
#'     
#'     vcov_sign <- clustid_list$vcov_sign
#'     clustid <- clustid_list$clustid
#'     clustid_dims <- clustid_list$clustid_dims
#'     N_G <- clustid_list$N_G
#'     cluster_names <- clustid_list$cluster_names
#'     
#'     cluster_bootcluster <- clustid_list$cluster_bootcluster
#'     bootcluster <- clustid_list$bootcluster
#'     all_c <- clustid_list$all_c
#' 
#' 
#'   } else {
#'     vcov_sign <- clustid_dims <- clustid <- bootcluster <- N_G <- cluster_names <- NULL
#'     cluster_bootcluster <- bootcluster <- all_c <- NULL
#'   }
#'   
#'   
#'   #iv prep
#'   if(is_iv){
#'     R0 <- get_R0_iv(object, R, n_exog, n_endog)
#'   } else {
#'       instruments <- X_exog <- X_endog <- NULL
#'       if (!is.matrix(R)) {
#'         R0 <- rep(0, length(colnames(X)))
#'         R0[match(param, colnames(X))] <- R
#'         names(R0) <- colnames(X)
#'       } else {
#'         R0 <- R
#'       }
#'     }
#' 
#' 
#'   
#'   res <- list(
#'     Y = Y,
#'     X = X,
#'     weights = weights,
#'     fixed_effect = fixed_effect,
#'     W = W,
#'     n_fe = n_fe,
#'     N = N,
#'     k =  length(coef(object)),
#'     k2 =  k2,
#'     clustid = clustid,
#'     vcov_sign = vcov_sign,
#'     clustid_dims = clustid_dims,
#'     N_G = N_G,
#'     cluster_bootcluster = cluster_bootcluster,
#'     bootcluster = bootcluster,
#'     N_G_bootcluster = length(unique(bootcluster[[1]])),
#'     R0 = R0,
#'     # model_frame = model_frame,
#'     X_exog = X_exog,
#'     X_endog = X_endog,
#'     instruments = instruments, 
#'     has_fe = has_fe, 
#'     all_c = all_c
#'   )
#'   
#'   if(is_iv){
#'     class(res) <- c("preprocess", "iv")
#'   } else {
#'     class(res) <- c("preprocess", "ols")
#'   }
#'   
#'   res
#'   
#' }
#' 
#' 
#' preprocess_method.felm <- function(object, clustid, clustid_char, R, param, fe, boot_algo, bootcluster){
#'   
#'   call <- object$call
#'   call_env <- attr(object$terms, ".Environment")
#'   fml <- formula(object)
#'   fml <- Formula::as.Formula(fml)
#'   
#'   fe_names <- attr( terms( formula(fml, lhs = 0, rhs = 2)), "term.labels")
#'   X_names <- attr( terms( formula(fml, lhs = 0, rhs = 1)), "term.labels")
#'   
#'   N <- nobs(object)
#'   k <- length(coef(object)) 
#'   p <- object$p
#'   
#'   is_iv <- FALSE
#'   if (suppressWarnings(formula(Formula::as.Formula(eval(object$formula)), lhs = 0, rhs = 3)) != "~0") {
#'     stop("IV regression is currently not supported by boottest() for objects of type 'felm'. You can either use 'fixest::feols()' or 'ivreg::ivreg' for IV-regression.")
#'     is_iv <- TRUE
#'   }
#'   
#'   X <- model_matrix(object, type = "rhs", collin.rm = TRUE)
#'   Y <- model.response(model.frame(object))
#'   has_fe <- ifelse(length(names(object$fe)) > 0, TRUE, FALSE)
#'   
#'   weights <- weights(object)
#'     has_weights <- FALSE
#'     weights <- rep(1, N)
#'   } else {
#'     has_weights <- TRUE
#'   }
#'   
#'   if(has_fe){
#'     get_fe <- transform_fe(object, X = X, Y = Y, fe = fe, N = N, has_weights = has_weights, boot_algo = boot_algo)
#'     X <- get_fe$X
#'     Y <- get_fe$Y
#'     fixed_effect <- get_fe$fixed_effect
#'     W <- get_fe$W
#'     n_fe <- get_fe$n_fe
#'     k2 <- get_fe$k2
#'   } else {
#'     fixed_effect <- NULL
#'     k2 <- 0
#'     W <- n_fe <- NULL
#'     W <- n_fe <- NULL
#'   }
#'   
#'   # bread <- lfe:::bread.felm(object)
#'   
#'   # get cluster variable
#'   if(!is.null(clustid)){
#'   
#'     clustid_list <- get_cluster2(
#'       object = object, 
#'       clustid = clustid,
#'       N = N, 
#'       clustid_char = clustid_char,
#'       bootcluster = bootcluster
#'     )  
#'     
#'     vcov_sign <- clustid_list$vcov_sign
#'     clustid <- clustid_list$clustid
#'     clustid_dims <- clustid_list$clustid_dims
#'     N_G <- clustid_list$N_G
#'     cluster_names <- clustid_list$cluster_names
#'     
#'     cluster_bootcluster <- clustid_list$cluster_bootcluster
#'     bootcluster <- clustid_list$bootcluster
#'     all_c <- clustid_list$all_c
#'     
#'     
#'   } else {
#'     vcov_sign <- clustid_dims <- clustid <- bootcluster <- N_G <- cluster_names <- NULL
#'     cluster_bootcluster <- bootcluster <- all_c <- NULL
#'   }
#'   
#'   
#'   #iv prep
#'   if(is_iv){
#'     R0 <- get_R0_iv(object, R, n_exog, n_endog)
#'   } else {
#'     instruments <- X_exog <- X_endog <- NULL
#'     if (!is.matrix(R)) {
#'       R0 <- rep(0, length(colnames(X)))
#'       R0[match(param, colnames(X))] <- R
#'       names(R0) <- colnames(X)
#'     } else {
#'       R0 <- R
#'     }
#'   }
#'   
#'   res <- list(
#'     Y = Y,
#'     X = X,
#'     weights = weights,
#'     fixed_effect = fixed_effect,
#'     W = W,
#'     n_fe = n_fe,
#'     N = N,
#'     k = length(coef(object)),
#'     k2 =  k2,
#'     clustid = clustid,
#'     vcov_sign = vcov_sign,
#'     clustid_dims = clustid_dims,
#'     N_G = N_G,
#'     cluster_bootcluster = cluster_bootcluster,
#'     bootcluster = bootcluster,
#'     N_G_bootcluster = length(unique(bootcluster[[1]])),
#'     R0 = R0,
#'     # model_frame = model_frame,
#'     X_exog = NULL,
#'     X_endog = NULL,
#'     instruments = NULL, 
#'     has_fe = has_fe, 
#'     all_c = all_c
#'   )
#'   
#'   if(is_iv){
#'     class(res) <- c("preprocess", "iv")
#'   } else {
#'     class(res) <- c("preprocess", "ols")
#'   }
#'   
#'   res
#'   
#'   
#'   
#' }
#' 
#' 
#' preprocess_method.lm <- function(object, clustid, R, param, bootcluster){
#'   
#'   call <- object$call
#'   call_env <- attr(object$terms, ".Environment")
#'   fml <- formula(object)
#'   fml <- Formula::as.Formula(fml)
#'   
#'   # X_names <- attr( terms( formula(fml, lhs = 0, rhs = 1)), "term.labels")
#'   
#'   N <- nobs(object)
#'   k <- length(coef(object)) 
#'   p <- object$p
#'   
#'   is_iv <- FALSE
#' 
#'   X <- model_matrix(object, collin.rm = TRUE)
#'   Y <- model.response(model.frame(object))
#'   has_fe <- FALSE
#'   
#'   weights <- weights(object)
#'   if(is.null(weights)){
#'     has_weights <- FALSE
#'     weights <- rep(1, N)
#'   } else {
#'     has_weights <- TRUE
#'   }
#'   
#'   # bread <- sandwich::bread(object)
#'   clustid_fml <- reformulate(clustid)
#'   # get cluster variable
#'   if(!is.null(clustid)){
#'     
#'     clustid_list <- get_cluster2(
#'       object = object, 
#'       clustid = clustid_fml,
#'       N = N, 
#'       clustid_char = clustid,
#'       bootcluster = bootcluster
#'     )  
#'     
#'     vcov_sign <- clustid_list$vcov_sign
#'     clustid <- clustid_list$clustid
#'     clustid_dims <- clustid_list$clustid_dims
#'     N_G <- clustid_list$N_G
#'     cluster_names <- clustid_list$cluster_names
#'     
#'     cluster_bootcluster <- clustid_list$cluster_bootcluster
#'     bootcluster <- clustid_list$bootcluster
#'     all_c <- clustid_list$all_c
#'     
#'     
#'   } else {
#'     vcov_sign <- clustid_dims <- clustid <- bootcluster <- N_G <- cluster_names <- NULL
#'     cluster_bootcluster <- bootcluster <- all_c <- NULL
#'   }
#'   
#'   
#'   #iv prep
#'   if(is_iv){
#'     R0 <- get_R0_iv(object, R, n_exog, n_endog)
#'   } else {
#'     instruments <- X_exog <- X_endog <- NULL
#'     if (!is.matrix(R)) {
#'       R0 <- rep(0, length(colnames(X)))
#'       R0[match(param, colnames(X))] <- R
#'       names(R0) <- colnames(X)
#'     } else {
#'       R0 <- R
#'     }
#'   }
#'   
#'   res <- list(
#'     Y = Y,
#'     X = X,
#'     weights = weights,
#'     fixed_effect = NULL,
#'     W = NULL,
#'     n_fe = NULL,
#'     N = N,
#'     k =  length(coef(object)),
#'     k2 =  0,
#'     clustid = clustid,
#'     vcov_sign = vcov_sign,
#'     clustid_dims = clustid_dims,
#'     N_G = N_G,
#'     cluster_bootcluster = cluster_bootcluster,
#'     bootcluster = bootcluster,
#'     N_G_bootcluster = length(unique(bootcluster[[1]])),
#'     R0 = R0,
#'     # model_frame = model_frame,
#'     X_exog = NULL,
#'     X_endog = NULL,
#'     instruments = NULL, 
#'     has_fe = has_fe, 
#'     all_c = all_c
#'   )
#'   
#'   class(res) <- c("preprocess", "ols")
#'   
#'   res 
#'   
#'   
#'   
#' }
#' 
#' 
#' 
#' preprocess_method.ivreg <- function(object, clustid, clustid_char, R, param, bootcluster){
#'   
#'   call <- object$call
#'   call_env <- attr(object$terms, ".Environment")
#'   fml <- formula(object)
#'   fml <- Formula::as.Formula(fml)
#'   is_iv <- TRUE
#'   #X_names <- attr( terms( formula(fml, lhs = 0, rhs = 1)), "term.labels")
#'   
#'   N <- nobs(object)
#'   k <- length(coef(object)) 
#'   p <- object$p
#'   
#'   is_iv <- FALSE
#'   has_fe <- FALSE
#'   
#'   X_endog <- model.matrix(object, component = "regressors", na.rm = TRUE)[, object$endogenous, drop = FALSE]
#'   X_exog <- model.matrix(object, component = "instruments", na.rm = TRUE)[, object$exogenous, drop = FALSE]
#'   instruments <- model.matrix(object, component = "instruments", na.rm = TRUE)[, object$instruments, drop = FALSE]
#'   Y <- model.response(model.frame(object))
#'   
#'   n_exog <- length(object$exogenous)
#'   n_endog <- length(object$endogenous)
#'   n_instruments <- length(object$instruments)
#'   
#'   weights <- weights(object)
#'   if(is.null(weights)){
#'     has_weights <- FALSE
#'     weights <- rep(1, N)
#'   } else {
#'     has_weights <- TRUE
#'   }
#'   
#'   bread <- sandwich::bread(object)
#'   
#'   # get cluster variable
#'   if(!is.null(clustid)){
#'     
#'     clustid_list <- get_cluster2(
#'       object = object, 
#'       clustid = clustid,
#'       N = N, 
#'       clustid_char = clustid_char,
#'       bootcluster = bootcluster
#'     )  
#'     
#'     vcov_sign <- clustid_list$vcov_sign
#'     clustid <- clustid_list$clustid
#'     clustid_dims <- clustid_list$clustid_dims
#'     N_G <- clustid_list$N_G
#'     cluster_names <- clustid_list$cluster_names
#'     
#'     cluster_bootcluster <- clustid_list$cluster_bootcluster
#'     bootcluster <- clustid_list$bootcluster
#'     all_c <- clustid_list$all_c    
#'     
#'   } else {
#'     vcov_sign <- clustid_dims <- clustid <- bootcluster <- N_G <- cluster_names <- NULL
#'     cluster_bootcluster <- bootcluster <- all_c <- NULL
#'   }
#'   
#'   
#'   #iv prep
#'   R0 <- get_R0_iv(object, R, n_exog, n_endog)
#' 
#'   res <- list(
#'     Y = Y,
#'     X = NULL,
#'     weights = weights,
#'     fixed_effect = NULL,
#'     W = NULL,
#'     n_fe = NULL,
#'     N = N,
#'     k =  length(coef(object)),
#'     k2 = 0,
#'     clustid = clustid,
#'     vcov_sign = vcov_sign,
#'     clustid_dims = clustid_dims,
#'     N_G = N_G,
#'     cluster_bootcluster = cluster_bootcluster,
#'     bootcluster = bootcluster,
#'     N_G_bootcluster = length(unique(bootcluster[[1]])),
#'     R0 = R0,
#'     # model_frame = model_frame,
#'     X_exog = X_exog,
#'     X_endog = X_endog,
#'     instruments = instruments, 
#'     has_fe = has_fe, 
#'     all_c = all_c
#'   )
#'   
#'   
#'   class(res) <- c("preprocess", "iv")
#' 
#'   res
#'   
#'   
#'   
#' }
#' 
#' 
#' get_cluster2 <- function(object, clustid, clustid_char, bootcluster, N) {
#'   
#'   # ---------------------------------------------------------------------------- #
#'   # Note: a large part of the following code was taken and adapted from the
#'   # sandwich R package, which is distributed under GPL-2 | GPL-3
#'   # Zeileis A, Köll S, Graham N (2020). "Various Versatile Variances: An object-Oriented
#'   # Implementation of Clustered Covariances in R." _Journal of Statistical Software_,
#'   # *95*(1), 1-36. doi: 10.18637/jss.v095.i01 (URL: https://doi.org/10.18637/jss.v095.i01).
#'   
#'   # changes by Alexander Fischer:
#'   # no essential changes, but slight reorganization of pieces of code
#'   
#'   
#'   # Step 1: create cluster df
#'   
#'   cluster_tmp <- 
#'     if("Formula" %in% loadedNamespaces()) { ## FIXME to suppress potential warnings due to | in Formula
#'       suppressWarnings(expand.model.frame(object, clustid, na.expand = FALSE))
#'     } else {
#'       expand.model.frame(object, clustid, na.expand = FALSE)
#'     }
#'   
#'   cluster_df <- model.frame(clustid, cluster_tmp, na.action = na.pass)
#'   N_G <- vapply(cluster_df, function(x) length(unique(x)), numeric(1))
#'   # Step 1: decode bootcluster variable
#'   
#'   # create a bootcluster vector
#'   if (length(bootcluster) == 1) {
#'     if (bootcluster == "max") {
#'       # use both vars
#'       bootcluster_char <- clustid_char
#'     } else if (bootcluster == "min") {
#'       # only minimum var
#'       bootcluster_char <- clustid_char[which.min(N_G)]
#'     } else {
#'       bootcluster_char <- bootcluster
#'     }
#'   } else {
#'     bootcluster_char <- bootcluster
#'   }
#'   
#'   cluster_bootcluster_fml <- update(clustid, paste("~ . +", paste(bootcluster_char, collapse = " + ")))
#'   
#'   cluster_bootcluster_tmp <- 
#'      if("Formula" %in% loadedNamespaces()) { ## FIXME to suppress potential warnings due to | in Formula
#'        suppressWarnings(expand.model.frame(object, cluster_bootcluster_fml, na.expand = FALSE))
#'      } else {
#'        expand.model.frame(object, cluster_bootcluster_fml, na.expand = FALSE)
#'      }
#'   
#'   cluster_bootcluster_df <- model.frame(cluster_bootcluster_fml, cluster_bootcluster_tmp, na.action = na.pass)
#' 
#'   cluster <- cluster_bootcluster_df[, clustid_char, drop = FALSE]
#'   bootcluster <- cluster_bootcluster_df[, bootcluster_char, drop = FALSE]
#'   
#'   if(!any(bootcluster_char %in% clustid_char)){
#'     is_subcluster <- TRUE
#'     if(!(any(names(bootcluster) %in% c(clustid_char, names(coef(object)))))){
#'       stop("A bootcluster variable is neither contained in the cluster variables nor in the model coefficients.")
#'     }
#'   } else {
#'     is_subcluster <- FALSE
#'   }
#'   
#'   ## handle omitted or excluded observations (works for lfe, lm)
#'   if((N != NROW(cluster)) && !is.null(object$na.action) && (class(object$na.action) %in% c("exclude", "omit"))) {
#'     cluster <- cluster[-object$na.action, , drop = FALSE]
#'   }
#'   
#'   if(NROW(cluster) != N) stop("number of observations in 'cluster' and 'nobs()' do not match")
#'   
#'   if((N != NROW(bootcluster)) && !is.null(object$na.action) && (class(object$na.action) %in% c("exclude", "omit"))) {
#'     bootcluster <- bootcluster[-object$na.action, , drop = FALSE]
#'   }
#'   
#'   if(NROW(bootcluster) != N) stop("number of observations in 'bootcluster' and 'nobs()' do not match")
#'   
#'   
#'   clustid_dims <- ncol(cluster)
#'   
#'   # taken from sandwich::vcovCL
#'   
#'   cluster_dims <- ncol(cluster)
#'   
#'   if (cluster_dims > 1L) {
#'     cl <- lapply(1L:cluster_dims, function(i) combn(1L:cluster_dims, i, simplify = FALSE))
#'     cl <- unlist(cl, recursive = FALSE)
#'     vcov_sign <- sapply(cl, function(i) (-1L)^(length(i) + 1L))    
#'     paste_ <- function(...) paste(..., sep = "_")
#'     for (i in (cluster_dims + 1L):length(cl)) {
#'       cluster <- cbind(cluster, Reduce(paste_, unclass(cluster[, cl[[i]] ]))) ## faster than: interaction()
#'     }
#'     #if(multi0) clustid[[length(cl)]] <- 1L:n
#'   } else {
#'     cl <- list(1)
#'     vcov_sign <- 1
#'   }
#'   
#'   
#'   
#'   if (cluster_dims == 1) {
#'     names(cluster) <- cluster_names
#'   } 
#'   
#'   N_G <- sapply(cluster, function(x) length(unique(x)))
#'   
#'   # now do all the other bootcluster things 
#'   c1 <- bootcluster_char[which(!(bootcluster_char %in% clustid_char))]
#'   # both bootstrapping and error cluster: all variables in clustid that are also in bootcluster
#'   c2 <- clustid_char[which(clustid_char %in% bootcluster_char)]
#'   # only error cluster: variables in clustid not in c1, c2
#'   c3 <- clustid_char[which(!(clustid_char %in% c(c1, c2)))]
#'   all_c <- c(c1, c2, c3)
#'   
#'   if (ncol(bootcluster) > 1){
#'     bootcluster <- as.data.frame(Reduce(paste_, bootcluster))
#'     names(bootcluster) <- Reduce(paste_, bootcluster_char)
#'   } 
#'   
#'   res <- list(
#'     vcov_sign = vcov_sign,
#'     # clustid_dims = clustid_dims,
#'     clustid = cluster,
#'     N_G = N_G,
#'     cluster_names = names(cluster), 
#'     all_c = all_c, 
#'     bootcluster = bootcluster, 
#'     cluster_bootcluster = cluster_bootcluster_df
#'   )
#'   
#'   cat("names(cluster): ", "\n")
#'   print(names(cluster))
#' 
#'   cat("all_c: ", "\n")
#'   print(all_c)
#' 
#'   cat("names(bootcluster):", "\n")
#'   print(head(bootcluster[[1]]))
#' 
#'   cat("names(cluster_bootcluster_df", "\n")
#'   print(names(cluster_bootcluster_df))
#'   
#'   
#'   res
#' }
#' 
#' 
#' get_R0_iv <- function(object, R, n_exog, n_endog){
#'   
#'     if (!is.matrix(R)) {
#'       R0 <- rep(0, n_exog + n_endog)
#'       R0[match(param, c(names(object$exogenous), names(object$endogenous)))] <- R
#'       # R0[1:n_exog][match(param, colnames(X_exog))] <- R
#'       # R0[(n_exog +1):(n_exog + n_endog)][match(param, colnames(X_endog))] <- R
#'       names(R0) <- c(names(object$exogenous), names(object$endogenous))
#'     } else {
#'       R0 <- R
#'     }
#'   
#'   R0
#'   
#' }
#' 
#' 
#' demean_fe <- function(X, Y, fe, has_weights, N){
#'   
#'     g <- collapse::GRP(fe, call = FALSE)
#'     X <- collapse::fwithin(X, g)
#'     Y <- collapse::fwithin(Y, g)
#'     
#'     fixed_effect_W <- as.factor(fe[, 1])
#'     
#'     if (!has_weights) {
#'       levels(fixed_effect_W) <- (1 / table(fe)) # because duplicate levels are forbidden
#'     } else {
#'       stop("Currently, boottest() does not jointly support regression weights / WLS and fixed effects. If you want to use
#'             boottest() for inference based on WLS, please set fe = NULL.")
#'       # levels(fixed_effect_W) <- 1 / table(fixed_effect)
#'     }
#'     
#'     W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
#'     n_fe <- length(unique(fe[, 1]))
#'     
#'     res <- list(
#'       X = X, 
#'       Y = Y, 
#'       fixed_effect_W, 
#'       W = W, 
#'       n_fe = n_fe
#'     )
#'     
#'     res
#'   
#'   
#' }
#' 
#' 
#' transform_fe <- function(object, X, Y, fe, has_weights, N, boot_algo){
#' 
#'     
#'     all_fe <- model_matrix(object, type = "fixef", collin.rm = TRUE)
#'     n_fe <- ncol(all_fe)
#'     all_fe_names <- names(all_fe)
#'     k2 <- Reduce("+",lapply(all_fe, function(x) length(unique(x))))
#'     
#'     if(n_fe == 1){
#'       if(!is.null(fe)){
#'         stop("With only one fixed effect specified, the function argument 'fe' needs to be NULL.")
#'       }
#'     }
#'     
#'     # if more than one fixed effect, add dummies if fe != NULL; else error
#'     if(n_fe > 1){
#'       
#'       fe_df <- all_fe[, fe, drop = FALSE]
#'       
#'       if(is.null(fe)){
#'         stop("boottest() allows for only one fixed effect when the function argument 'fe = NULL'.")
#'       } else {
#'         add_fe <- all_fe[,  all_fe_names != fe, drop = FALSE]
#'         add_fe_names <- names(add_fe)
#'         # make sure add_fe consists of factor variables
#'         add_fe <- lapply(add_fe, as.factor)
#'         add_fe_dummies <- model.matrix(reformulate(add_fe_names, response = NULL), model.frame(reformulate(add_fe_names, response = NULL) , data = as.data.frame(add_fe)))
#'         # drop the intercept 
#'         add_fe_dummies <- add_fe_dummies[, -1]
#'         X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
#'       }
#'     } else {
#'       fe_df <- all_fe
#'     }
#'   
#'   if(boot_algo == "R"){
#'     # WildBootTests.jl does demeaning internally
#'     prep_fe <- demean_fe(X, Y, fe_df, has_weights, N)
#'     X <- prep_fe$X
#'     Y <- prep_fe$Y
#'     W <- prep_fe$W
#'     n_fe <- prep_fe$n_fe
#'   } else {
#'     W <- n_fe <- NULL
#'   }
#'   
#'   res <- list(X = X, 
#'               Y = Y, 
#'               W = W, 
#'               n_fe = n_fe, 
#'               k2 = k2, 
#'               fixed_effect = fe_df)  
#'   
#'   res
#' }
#' 
#' 
#' 
#' 
#' # get_bootcluster <- function(object, bootcluster, clustid_char, N_G, X, clustid) {
#' # 
#' #   
#' #   # for WildBootTests.jl
#' #   # Order the columns of `clustid` this way:
#' #   # 1. Variables only used to define bootstrapping clusters, as in the subcluster bootstrap.
#' #   # 2. Variables used to define both bootstrapping and error clusters.
#' #   # 3. Variables only used to define error clusters.
#' #   # In the most common case, `clustid` is a single column of type 2.
#' #   
#' #   # create a bootcluster vector
#' #   if (length(bootcluster) == 1) {
#' #     if (bootcluster == "max") {
#' #       # use both vars
#' #       bootcluster_n <- clustid_char
#' #     } else if (bootcluster == "min") {
#' #       # only minimum var
#' #       bootcluster_n <- names(clustid[which.min(N_G)])
#' #     }
#' #   } else {
#' #     bootcluster_n <- bootcluster
#' #   }
#' #   
#' #   c1 <- bootcluster_n[which(!(bootcluster_n %in% clustid_char))]
#' #   # both bootstrapping and error cluster: all variables in clustid that are also in bootcluster
#' #   c2 <- clustid_char[which(clustid_char %in% bootcluster_n)]
#' #   # only error cluster: variables in clustid not in c1, c2
#' #   c3 <- clustid_char[which(!(clustid_char %in% c(c1, c2)))]
#' #   all_c <- c(c1, c2, c3)
#' #   
#' # 
#' #   if(length(bootcluster) == 1){
#' #     bootcluster <- cluster_bootcluster_df <- clustid
#' #     if(bootcluster == "max"){
#' #       cluster_bootcluster_df <- clustid
#' #       bootcluster <- clustid[which.max(N_G)]
#' #       names(bootcluster) <- paste0(c2[1],"-", c2[2])
#' #     } else if(bootcluster == "min"){
#' #       cluster_bootcluster_df <- clustid
#' #       bootcluster <- clustid[, bootcluster_n, drop = FALSE]
#' #     } else {
#' #       cluster_bootcluster_df <- bootcluster <- clustid
#' #     }
#' #   } else if (length(bootcluster) > 1){
#' #       if(length(c1) != 0){
#' #       # if there is a bootcluster variable not contained in the cluster variables, 
#' #       # obtain it from the design matrix (should throw error when projected out)
#' #         if(!(c1 %in% colnames(X))){
#' #           stop(paste("The subcluster variable", x, "is not contained in the model coefficients. This is not allowed."))
#' #         }
#' #         cluster_bootcluster_df <- collapse::add_vars(
#' #           clustid[,c(c2, c3), drop = FALSE], 
#' #           as.data.frame(X)[, c1, drop = FALSE], 
#' #           pos = "end"
#' #         )
#' #         
#' #         i <- sapply(cluster_bootcluster_df, is.numeric)
#' #         cluster_bootcluster_df[i] <- lapply(cluster_bootcluster_df[i], as.character)
#' #         
#' #         bootcluster <- as.data.frame(Reduce(paste0, cluster_bootcluster_df[, c(c1, c2), drop = FALSE]))
#' #         names(bootcluster) <- paste0(c1,"-", c2)
#' #       } else {
#' #       stop("If not using subclusters for the bootstrap, please use 'min' or 'max' to specify the error clustering.")
#' #     }
#' #   } 
#' #   
#' #   cat(bootcluster_n, "\n")
#' #   cat(all_c, "\n")
#' #   cat(names(cluster_bootcluster_df), "\n")
#' #   cat(names(bootcluster), "\n")
#' #   
#' #   res <- list(
#' #     bootcluster_df = cluster_bootcluster_df, 
#' #     bootcluster = bootcluster,
#' #     all_c = all_c
#' #   )
#' #   
#' #   res
#' #   
#' # }
#' # 
#' # 
