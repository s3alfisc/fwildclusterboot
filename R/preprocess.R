preprocess.lm <- function(object, param, clustid, beta0, alpha, seed){
  
  #' function that pre-processes regression objects of type lm
  #'@param object An object of class lm
  #'@param clustid A vector with the clusters
  #'@param param The univariate coefficients for which a hypothesis is to be tested
  #'@param beta0 A numeric. Shifts the null hypothesis  
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@return preprocessed object of class boottest_preprocessed
  #'@import sandwich
  #'@import lmtest
  
  # print warnings as they occur
  #options(warn=1)
  
  check_arg(clustid, "character scalar | character vector")
  check_arg(beta0, "numeric scalar | NULL")
  check_arg(alpha, "numeric scalar | NULL")
  
  if(!is.null(seed)){
    seed <- seed
  } else if(is.null(seed)){
    seed <- 2
  }
  
  set.seed(seed)
  
  fml <- object$call$formula
  fml_all_clustid <- formula(paste0(as.character(fml), "+", clustid, collapse = "+"))
  
  data <- model.frame(formula = fml, 
                      data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                      drop.unused.levels = TRUE) 
  data_clustid <- model.frame(formula = fml_all_clustid, 
                      data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
                      drop.unused.levels = TRUE) 
  
  data_diff <- nrow(data) - nrow(data_clustid)
  
  if(data_diff == 1){
    warning(paste(data_diff, 
                  "observation deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."),
            .call = FALSE)
  } else if(data_diff > 1){
    warning(paste(data_diff, 
                  "observations deleted due to NA values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."), 
            .call = FALSE)
  } else if(data_diff < 0){
    stop("nrow(data_cluster) < nrow(data_clustid) - this cannot be correct.", 
         .call = FALSE)
  }
  
  clustid <- as.data.frame(data_clustid[, clustid])
  
  #formula <- object$call$fml
  weights <- object$call$weights
  
  if(!is.null(weights)){
    stop("Currently, boottest does not support weighted least squares. weights 
         must be NULL.")
  }
  
  
  if(is.null(alpha)){
    alpha <- 0.05
  }
  
  # # retrieve clusters / multiple clusters
  # if(inherits(clustid, "formula")) {
  #   clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
  #   clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
  # } else {
  #   clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
  # }
  
  if(!(param %in% c(names(object$coefficients)))){
    warning("Parameter to test not in model or all. Please specify appropriate parameters to test.")
  }
  
  if(sum(!(names(clustid)) %in% c(names(object$coefficients)))>1){
    clustid_not_model <- TRUE
  }
  
  # how many clustids? uniway/multiway?
  clustid_dims <- ncol(clustid)
  
  
  # # Handle omitted or excluded observations
  # if(!is.null(object$na.action)) {
  #   if(class(object$na.action) == "exclude") {
  #     clustid <- clustid[-object$na.action,]
  #   } else if(class(object$na.action) == "omit") {
  #     clustid <- clustid[-object$na.action,]
  #   }
  #   clustid <- as.data.frame(clustid)  # silly error somewhere
  # }
  
  # clustid_na <- is.na(clustid)
  # delete_clustid <- rowSums(clustid_na)
  # delete_clustid_sum <- sum(rowSums(clustid_na))
  # 
  # if(delete_clustid_sum > 0){
  #   warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
  # }
  # 
  # data <- data[-delete_clustid, ]
  
  #if(debug) print(class(clustid))
  
  if(is.null(beta0)){
    beta0 <- 0
  }
  
  # Factors in our clustiding variables can potentially cause problems
  # Blunt fix is to force conversion to characters
  i <- !sapply(clustid, is.numeric)
  clustid[i] <- lapply(clustid[i], as.character)
  
  # Make all combinations of clustid dimensions
  # if(clustid_dims > 1) {
  #   for(i in acc) {
  #     clustid <- cbind(clustid, Reduce(paste0, clustid[,i]))
  #   }
  # }
  
  if(clustid_dims == 2){
    #clustid_1 <- names(clustid)[1]
    #clustid_2 <- names(clustid)[2]
    names(clustid) <- c("clustid_1", "clustid_2")
    #clustid <- paste0(clustid_1, "-", clustid_2)
    clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
  }
  
  
  
  # start estimation here: 
  
  R0 <- as.numeric(param == names(object$coefficients))
  groupvars <- names(coef(object))
  
  # if(object_type == "felm"){
  #   
  #   depvar <- names(object$response)
  #   Y <- object$response
  #   X <- lfe:::model.matrix.felm(felm_fit) 
  # }
  
  
  depvar <- all.vars(as.formula(object$call))[1]
  #measurevar <- "y"
  #formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  
  model_frame <- model.frame(fml, data_clustid)
  X <- model.matrix(model_frame, data = data_clustid)
  Y <- model.response(model_frame)

  #res <- boottest_fun(Y = Y, X = X, R0 = R0, clustid = clustid, B = B, param = param)
  
  N <- length(Y)
  k <- ncol(X)
  
  #Xr1 <- X
  #Xr1[, which(R0 == 1)] <- beta0 + Xr1[, which(R0 == 1)]
  
  # fixed_effect <- NULL
  # W <- NULL
  
  #clustid <- as.vector(clustid)
  #clustid <- rep(1:20, 100)
  N_G <- sapply(clustid, function(x) length(unique(x)))
  
  #numb_clusters <- ncol(clustid)
  if(clustid_dims == 1){
    if(max(N_G) > 200){
      warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
    }
  } else if(clustid_dims > 1){
    if(max(N_G) > 200){
      warning(paste("You are estimating a model with", max(N_G), "clusters. Note that the speed gains from the algorithm underlying fwildclusterboot mainly apply for small number of clusters."))
    }
  }

  
  
  res_preprocess <- list(fixed_effect = NULL, 
                         data = data, 
                         param = param, 
                         clustid = clustid, 
                         clustid_dims = clustid_dims,
                         N = N, 
                         k = k, 
                         Y = Y, 
                         X = X, 
                         #depvar = depvar, 
                         #groupvars = groupvars, 
                         beta0 = beta0, 
                         clustid_dims, 
                         R0 = R0, 
                         N_G = N_G, 
                         alpha = alpha, 
                         W = NULL, 
                         n_fe = NULL, 
                         seed = seed)
  if(clustid_dims == 1){
    class(res_preprocess) <- "oneclust"
  } else if(clustid_dims > 1){
    class(res_preprocess) <- "multclust"
  }
  res_preprocess
  
}



#' preprocess.felm <- function(object, param, clustid, beta0, alpha, fe){
#'   
#'   #' function that pre-processes regression objects of type felm
#'   #'@param object An object of class felm
#'   #'@param clustid A vector with the clusters
#'   #'@param param The univariate coefficients for which a hypothesis is to be tested
#'   #'@param beta0 A numeric. Shifts the null hypothesis  
#'   #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
#'   #'@return preprocessed object of class boottest_preprocessed
#'   
#'   # print warnings as they occur
#'   options(warn=1)
#'   
#'   check_arg(clustid, "os formula | data.frame | named list")
#'   check_arg(beta0, "numeric scalar | NULL")
#'   check_arg(alpha, "numeric scalar | NULL")
#' 
#'   
#'   if(length(object$fe) == 0){
#'     # if there are no fixed effects in the model
#'     fml <- formula(Formula::Formula(object$formula), lhs = 1, rhs = 1)
#'     
#'     fixed_effect <- NULL
#'     numb_fe <- NULL
#'     #fml_exclude_fe <- object$fml
#'     data <- model.frame(formula = fml, 
#'                         data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                         drop.unused.levels = TRUE)  
#'     
#'     # retrieve clusters / multiple clusters
#'     if(inherits(clustid, "formula")) {
#'       clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
#'       clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
#'     } else {
#'       clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
#'     }
#'     
#'     # Handle omitted or excluded observations
#'     if(!is.null(object$na.action)) {
#'       if(class(object$na.action) == "exclude") {
#'         clustid <- clustid[-object$na.action,]
#'       } else if(class(object$na.action) == "omit") {
#'         clustid <- clustid[-object$na.action,]
#'       }
#'       clustid <- as.data.frame(clustid)  # silly error somewhere
#'     }
#'     
#'     clustid_na <- is.na(clustid)
#'     delete_clustid <- rowSums(clustid_na)
#'     delete_clustid_sum <- sum(rowSums(clustid_na))
#'     
#'     if(delete_clustid_sum > 0){
#'       warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
#'     }
#'     
#'     data <- data[-delete_clustid, ]
#'     
#'     model_frame <- model.frame(fml, data)
#'     X <- model.matrix(model_frame, data = data)
#'     Y <- model.response(model_frame)
#'     
#'     N <- nrow(X)
#'     k <- ncol(X)
#'     W <- NULL
#'     n_fe <- NULL
#'     
#'   } else {
#'     fml <- object$formula
#'     # if there are any fixed effects in the model, get data.frame with fe
#'     fml_wo_fe <- formula(Formula::Formula(eval(fml, envir =  attr(object$terms, ".Environment"))), lhs = 1, rhs = 1)
#'     #fml_fe <- formula(Formula::Formula(eval(fml, envir =  attr(object$terms, ".Environment"))), lhs = 0, rhs = 2)
#'     
#'     names_covariates <- rownames(object$coefficients)
#'     names_fe <- names(object$fe)
#'     numb_fe <- length(names_fe)
#'     #depvar <- all.vars(fml)[!(all.vars(fml) %in% c(names_covariates, names_fe))]
#'     #depvar <- colnames(object$coefficients)
#'     depvar <- colnames(object$response)
#'     
#'     #fe_fml_prep <- eval(quote(paste0(names_fe, collapse = "+")))
#'     #substitute(numb_fe)
#'     
#'     #str(quote(paste0(names_fe, collapse = "+")))      
#'     
#'     if(numb_fe == 1){ 
#'       # there is fixed effect in model formula, but not fe specified
#'       if(is.null(fe)){
#'         # need to assign fe if not provided
#'         fe <- names_fe
#'       }
#'       # get data including fixed effect
#'       fml <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       data_all <- model.frame(formula = fml,
#'                              data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                              drop.unused.levels = TRUE)   
#'       
#'       # retrieve clusters / multiple clusters
#'       if(inherits(clustid, "formula")) {
#'         clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
#'         clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
#'       } else {
#'         clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
#'       }
#'       
#'       # Handle omitted or excluded observations
#'       if(!is.null(object$na.action)) {
#'         if(class(object$na.action) == "exclude") {
#'           clustid <- clustid[-object$na.action,]
#'         } else if(class(object$na.action) == "omit") {
#'           clustid <- clustid[-object$na.action,]
#'         }
#'         clustid <- as.data.frame(clustid)  # silly error somewhere
#'       }
#'       
#'       clustid_na <- is.na(clustid)
#'       delete_clustid <- rowSums(clustid_na)
#'       delete_clustid_sum <- sum(rowSums(clustid_na))
#'       
#'       if(delete_clustid_sum > 0){
#'         warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
#'       }
#'       
#'       data_all <- data_all[-delete_clustid, ]
#'       
#'       data <- subset(data_all, select = -c(get(fe)))
#'       fixed_effect <- data_all[, fe]
#'       # only one fe - do not have to add fe from fe part to formula
#'       #demean_data <- fixest::demean(data, fixed_effect)
#'       #demean_data <- as.data.frame(demean_data)
#'       model_frame <- model.frame(update(fml_wo_fe, ~ . +  0), data)
#'       X <- model.matrix(model_frame, data = data)
#'       Y <- model.response(model_frame)
#'       X <- collapse::fwithin(X, fixed_effect)#
#'       Y <- collapse::fwithin(Y, fixed_effect)
#'       
#'     } else if(numb_fe > 1 & is.null(fe)){
#'       # there are several candidates that can be projected out in bootstrap. choose the 
#'       # factor with the most groups
#'       fml_fe <- formula(paste0("~", paste0(names_fe, collapse = "+")))
#'       fixed_effects <- model.frame(formula = fml_fe, 
#'                                    data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                                    drop.unused.levels = TRUE)
#'       max_fe <- which.max(sapply(fixed_effects, function(x) length(unique(x))))
#'       fe <- names_fe[max_fe]
#'       # get the data
#'       fml <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       data_all <- model.frame(formula = fml, 
#'                               data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                               drop.unused.levels = TRUE)   
#'       
#'       # retrieve clusters / multiple clusters
#'       if(inherits(clustid, "formula")) {
#'         clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
#'         clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
#'       } else {
#'         clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
#'       }
#'       
#'       # Handle omitted or excluded observations
#'       if(!is.null(object$na.action)) {
#'         if(class(object$na.action) == "exclude") {
#'           clustid <- clustid[-object$na.action,]
#'         } else if(class(object$na.action) == "omit") {
#'           clustid <- clustid[-object$na.action,]
#'         }
#'         clustid <- as.data.frame(clustid)  # silly error somewhere
#'       }
#'       
#'       clustid_na <- is.na(clustid)
#'       delete_clustid <- rowSums(clustid_na)
#'       delete_clustid_sum <- sum(rowSums(clustid_na))
#'       
#'       if(delete_clustid_sum > 0){
#'         warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
#'       }
#'       
#'       data_all <- data_all[-delete_clustid, ]
#'       
#'       data <- subset(data_all, select = -c(get(fe)))
#'       fixed_effect <- data_all[, fe]
#'       # now update names_fe - exclude the fixed effect that will be projected out
#'       names_fe <- names_fe[names_fe != fe]
#'       # update fml_wo_fe (add factors that are not projected out)
#'       fml <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       model_frame <- model.frame(update(fml_wo_fe, ~ . +  0), data)
#'       X <- model.matrix(model_frame, data = data)
#'       Y <- model.response(model_frame)
#'       # now demean X and Y separately
#'       X <- collapse::fwithin(X, fixed_effect)
#'       Y <- collapse::fwithin(Y, fixed_effect)
#'     } else if(numb_fe > 1 & !is.null(fe)){
#'       fml <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       data_all <- model.frame(formula = fml, 
#'                               data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                               drop.unused.levels = TRUE)   
#'       
#'       # retrieve clusters / multiple clusters
#'       if(inherits(clustid, "formula")) {
#'         clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
#'         clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
#'       } else {
#'         clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
#'       }
#'       
#'       # Handle omitted or excluded observations
#'       if(!is.null(object$na.action)) {
#'         if(class(object$na.action) == "exclude") {
#'           clustid <- clustid[-object$na.action,]
#'         } else if(class(object$na.action) == "omit") {
#'           clustid <- clustid[-object$na.action,]
#'         }
#'         clustid <- as.data.frame(clustid)  # silly error somewhere
#'       }
#'       
#'       clustid_na <- is.na(clustid)
#'       delete_clustid <- rowSums(clustid_na)
#'       delete_clustid_sum <- sum(rowSums(clustid_na))
#'       
#'       if(delete_clustid_sum > 0){
#'         warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
#'       }
#'       
#'       data_all <- data_all[-delete_clustid, ]
#'       
#'       data <- subset(data_all, select = -c(get(fe)))
#'       fixed_effect <- data_all[, fe]
#'       # now update names_fe - exclude the fixed effect that will be projected out
#'       names_fe <- names_fe[names_fe != fe]
#'       # update fml_wo_fe (add factors that are not projected out)
#'       fml_wo_fe <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       model_frame <- model.frame(update(fml_wo_fe, ~ . +  0), data)
#'       X <- model.matrix(model_frame, data = data)
#'       Y <- model.response(model_frame)
#'     }
#'     
#'     N <- nrow(X)
#'     k <- ncol(X)
#'     
#'     fixed_effect_W <- fixed_effect
#'     levels(fixed_effect_W) <- 1 / table(fixed_effect)
#'     W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
#'     n_fe <- length(unique(fixed_effect))
#'   }
#'   
#'   
#'   
#'   
#'   
#'   
#'   
#'   #data <- get_model_frame(object)
#'   #try_fe <- suppressWarnings(try(get_model_fe(object)))
#'   
#'   #if(length(object$fe) == 0){
#'   #  fixed_effects <- NULL
#'   #  numb_fe <- NULL
#'   #} else {
#'   #  fixed_effects <- get_model_fe(object)
#'   #  # fixed_effects object$fe
#'   #  numb_fe <- ncol(fixed_effects)
#'   #  fml_only_fe <- formula(paste0("~",names(fixed_effects), collapse = "+"))
#'   #}
#'   
#'  
#' 
#'   
#'   #fixed_effects <- suppressWarnings(try(get_model_fe(object), TRUE))
#'   #numb_fe <- ncol(fixed_effects)
#'   
#'   
#'   # if(is.null(numb_fe) || numb_fe == 0){
#'   #   fixed_effects <- NULL
#'   #   numb_fe <- NULL
#'   # }
#'   
#'   if(is.null(alpha)){
#'     alpha <- 0.05
#'   }
#'   if(!is.numeric(alpha) || alpha > 1 || alpha < 0 || length(alpha) > 1){
#'     stop("The level of significance alpha must be a numeric between 0 and 1")
#'   }
#'   
#'   
#'   weights <- object$call$weights
#'   if(!is.null(weights)){
#'     stop("Currently, boottest does not support weighted least squares. weights 
#'          must be NULL.")
#'   }
#'   
#'   fml <- Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment")))
#'   fml_exclude_fe <- suppressWarnings(formula(fml, lhs = 1, rhs = 1))
#'   fml_only_fe <- suppressWarnings(formula(fml, lhs = 0, rhs = 2))
#'   fml_cluster <- suppressWarnings(formula(fml,lhs = 0, rhs = 4))
#'   
#'   # if(fml_cluster == ~0){
#'   #   stop("In contrast to objects of class lm and fixest, you need to specify the desired level 
#'   #        of clustering for objects of type felm in the felm() estimation command")
#'   # }
#'   #use_fixed_effects <- suppressWarnings(formula(fml, lhs = 0, rhs = 2) == "~0")
#'   
#'   fml_test_iv <- suppressWarnings(formula(fml, lhs = 0, rhs = 3))
#'   if(fml_test_iv != ~0){
#'     stop("The boottest() function currently does not support instrumental variables
#'          estimation.")
#'   }
#'   
#'   if(!is.null(seed)){
#'     set.seed(seed)
#'   } else if(is.null(seed)){
#'     set.seed(2)
#'   }
#'   
#' 
#'   N_G <- sapply(clustid, function(x) length(unique(x)))
#'   
#'   #numb_clusters <- ncol(clustid)
#'   if(clustid_dims == 1){
#'     if(max(N_G) > 200){
#'       warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
#'     }
#'   } else if(clustid_dims > 1){
#'     if(max(N_G) > 200){
#'       warning(paste("You are estimating a model with more than 200 clusters. The more clusters in the data, the longer the estimation process."))
#'     }
#'   }
#'   
#'   
#'   if(!(param %in% c(rownames(object$coefficients)))){
#'     stop("Parameter to test not in model or all. Please specify appropriate parameters to test.")
#'   }
#'   
#'   # how many clustids? uniway/multiway?
#'   clustid_dims <- ncol(clustid)
#'   if(clustid_dims == 2){
#'     #clustid_1 <- names(clustid)[1]
#'     #clustid_2 <- names(clustid)[2]
#'     names(clustid) <- c("clustid_1", "clustid_2")
#'     #clustid <- paste0(clustid_1, "-", clustid_2)
#'     clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
#'   }
#'   
#'  
#'   
#'   if(is.null(beta0)){
#'     beta0 <- 0
#'   }
#'   
#'   # Factors in our clustiding variables can potentially cause problems
#'   # Blunt fix is to force conversion to characters
#'   i <- !sapply(clustid, is.numeric)
#' 
#'   N_G <- sapply(clustid, function(x) length(unique(x)))
#'   
#'   if(clustid_dims == 1){
#'     if(max(N_G) > 200){
#'       warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
#'     }
#'   } else if(clustid_dims > 1){
#'     if(max(N_G) > 200){
#'       warning(paste("You are estimating a model with more than 200 clusters. The more clusters in the data, the longer the estimation process."))
#'     }
#'   }
#'   
#'   
#'   
#'   # groupvars <- names(coef(object))
#'   # depvar <- names(object$response)
#'   
#'   #formula <- formula(Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment"))), lhs = 1, rhs = 1)
#'   
#'   #fe_formula <- formula(paste0("~", names(fixed_effects), collapse = "+"))
#'   
#'   
#'   # # if fixed effects are specified, demean: 
#'   # if(!is.null(numb_fe)){
#'   #   #if(!is.null(numb_fe)){
#'   #   demean_data <- fixest::demean(data, fixed_effects)
#'   #   data <- as.data.frame(demean_data) 
#'   #   model_frame <- model.frame(fml_exclude_fe, data = data)
#'   #   X <- model.matrix(fml_exclude_fe, data = data)
#'   #   Y <- model.response(model_frame)
#'   # } else if(!is.null(numb_fe)){
#'   #   model_frame_fe <- model.frame(fml_only_fe, data = fixed_effects)
#'   #   X_fe <- model.matrix(model_frame_fe, data = fixed_effects)
#'   #   # update: get rid of intercept (because of fe)
#'   #   model_frame <- model.frame(update(fml_exclude_fe, ~ . +  0), data = data)
#'   #   X <- model.matrix(model_frame, data = data)
#'   #   X <- cbind(X, X_fe)
#'   #   Y <- model.response(model_frame)
#'   #   #fe_dummies <- fastDummies::dummy_cols(fixed_effects, remove_first_dummy = TRUE, ignore_na = TRUE)
#'   # } else {
#'   #   # case where is.null(numb_fe) == TRUE
#'   #   model_frame <- model.frame(fml_exclude_fe, data = data)
#'   #   X <- model.matrix(model_frame, data = data)
#'   #   Y <- model.response(model_frame)
#'   # }
#'   # 
#'   #if(demean == TRUE){
#'   
#'   #} else {
#'   #  model_frame <- model.frame(formula(paste("~", names(data), "+ 0")),
#'   #                             data = data)
#'   #  fe <- model.frame(formula(paste("~", names(fixed_effects), "+ 0")), 
#'   #                    fixed_effects)
#'   #  X <- model.matrix(fml_exclude_fe, data = data)
#'   #  X_fe <- model.matrix(fe, data = fixed_effects)
#'   #  X <- cbind(X, X_fe)
#'   #}
#'   
#'   
#'   R0 <- as.numeric(param == colnames(X))
#'   
#'   # N <- length(Y)
#'   # k <- ncol(X)
#'   
#'   res_preprocess <- list(fixed_effect = fixed_effect, 
#'                          data = data, 
#'                          param = param, 
#'                          clustid = clustid, 
#'                          clustid_dims = clustid_dims, 
#'                          N = N, 
#'                          k = k, 
#'                          Y = Y, 
#'                          X = X, 
#'                          #depvar = depvar, 
#'                          #groupvars = groupvars, 
#'                          beta0 = beta0, 
#'                          clustid_dims, 
#'                          R0 = R0, 
#'                          N_G = N_G, 
#'                          alpha = alpha, 
#'                          W = W, 
#'                          n_fe = n_fe)
#'   
#'   if(clustid_dims == 1){
#'     class(res_preprocess) <- "oneclust"
#'   } else if(clustid_dims > 1){
#'     class(res_preprocess) <- "multclust"
#'   }
#'   res_preprocess  
#' }


#' preprocess.fixest <- function(object, param, clustid, beta0, alpha, fe){
#'   
#'   #' function that pre-processes regression objects of type fixest
#'   #'@param object An object of class fixest
#'   #'@param clustid A vector with the clusters
#'   #'@param param The univariate coefficients for which a hypothesis is to be tested
#'   #'@param beta0 A numeric. Shifts the null hypothesis  
#'   #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
#'   #'@return preprocessed object of class boottest_preprocessed
#'   
#'   # print warnings as they occur
#'   #options(warn=1)
#'   
#'   # object <- feols(proposition_vote ~ treatment + ideology1 + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
#'   # param <- "treatment"
#'   # clustid <- ~ group_id1
#'   # beta0 = 0
#'   # alpha = 0.05
#' 
#'   # Part 1) Check Arguments
#'   check_arg(clustid, "os formula | data.frame | named list")
#'   check_arg(beta0, "numeric scalar | NULL")
#'   check_arg(alpha, "numeric scalar | NULL")
#'   check_arg(fe, "character scalar | NULL")
#'   
#'   # PArt 2: Check arguments further
#'   #if(!(fe %in% object$fixef_vars) | !is.null(fe)){
#'   #  stop("The fixed effect in boottest() is not a fixed effect in the model.")
#'   #}
#'   
#'   if(is.null(alpha)){
#'     alpha <- 0.05
#'   }
#'   
#'   if(!is.numeric(alpha) || alpha > 1 || alpha < 0 || length(alpha) > 1){
#'     stop("The level of significance alpha must be a numeric between 0 and 1")
#'   }
#'   
#'   
#'   if(!is.null(object$call$weights)){
#'     stop("Function currently does not allow weights.")
#'   }
#'   
#' 
#'   if(!(param %in% c(names(object$coefficients)))){
#'     stop("Parameter to test not in model or all. Please specify appropriate parameters to test.")
#'   }
#'   
#'   if(is.null(beta0)){
#'     beta0 <- 0
#'   }
#'   
#'   # Part 3) preprocess the covariates, depvar and fixed effects
#'   # 4 different cases:
#'   # - only one fixed effect specified in feols() and fe = NULL
#'   # - only one fixed effect specified in feols() and fe specified
#'   # - more than one fixed effect specified in feols() and fe = NULL
#'   # - more than one fixed effect specified in feols() and fe != NULL
#'   
#'   
#' 
#'   if(is.null(object$fixef_vars)){
#'     # if there are no fixed effects in the model
#'     fml <- object$fml
#'     fixed_effect <- NULL
#'     numb_fe <- NULL
#'     n_fe <- NULL
#'     #fml_exclude_fe <- object$fml
#'     data <- model.frame(formula = fml, 
#'                         data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                         drop.unused.levels = TRUE)  
#'     #data <- data_all[-object$obsRemoved,]
#'     
#'     if(inherits(clustid, "formula")) {
#'       clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
#'       clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
#'     } else {
#'       clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
#'     }
#'     
#'     clustid_dims <- ncol(clustid)
#'     if(is.null(clustid_dims)){clustid_dims <- 1}
#'     
#'     # Important: Handling of omitted or excluded observations
#'     clustid <- clustid[-object$obsRemoved,]
#'     clustid_na <- is.na(clustid)
#'     delete_clustid <- rowSums(clustid_na)
#'     delete_clustid_sum <- sum(rowSums(clustid_na))
#'     
#'     if(delete_clustid_sum > 0){
#'       warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
#'     }
#'     
#'     data <- data[-delete_clustid, ]
#'     
#'     if(nrow(data) != nrow(clustid)){
#'       stop("data and clustid have different dimensions.")
#'     }
#'     
#'     model_frame <- model.frame(fml, data)
#'     X <- model.matrix(model_frame, data = data)
#'     Y <- model.response(model_frame)
#'     
#'     N <- nrow(X)
#'     k <- ncol(X)
#'     W <- NULL
#'     
#'   } else {
#'     fml <- object$call$fml
#'     # if there are any fixed effects in the model, get data.frame with fe
#'     fml_wo_fe <- formula(Formula::Formula(eval(fml, envir =  attr(object$terms, ".Environment"))), lhs = 1, rhs = 1)
#'     #fml_fe <- formula(Formula::Formula(eval(fml, envir =  attr(object$terms, ".Environment"))), lhs = 0, rhs = 2)
#'     
#'     depvar <- as.character(formula.tools::lhs(object$fml))
#'     names_covariates <- names(object$coefficients)
#'     names_fe <- object$fixef_vars
#'     numb_fe <- length(names_fe)
#'     
#'     if(numb_fe == 1){ 
#'       # there is fixed effect in model formula, but not fe specified
#'       if(is.null(fe)){
#'         # need to assign fe if not provided
#'         fe <- names_fe
#'       }
#'       # get data including fixed effect
#'       fml <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       data_all <- model.frame(formula = fml, 
#'                           data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                           drop.unused.levels = TRUE)   
#'       #data_all <- data_all[-object$obsRemoved,]
#'       
#'       # handle clusters and missings in clusters
#'       if(inherits(clustid, "formula")) {
#'         clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
#'         clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
#'       } else {
#'         clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
#'       }
#'       
#'       clustid_dims <- ncol(clustid)
#'       if(is.null(clustid_dims)){clustid_dims <- 1}
#'       
#'       # Important: Handling of omitted or excluded observations
#'       clustid <- clustid[-object$obsRemoved,]
#'       clustid_na <- is.na(clustid)
#'       delete_clustid <- rowSums(clustid_na)
#'       delete_clustid_sum <- sum(rowSums(clustid_na))
#'       
#'       if(delete_clustid_sum > 0){
#'         warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
#'       }
#'       
#'       data_all <- data_all[-delete_clustid, ]
#'       
#'       if(nrow(data_all) != nrow(clustid)){
#'         stop("data and clustid have different dimensions.")
#'       }
#'       
#'       # stop handling of clusters and missings in clusters
#'       
#'       data <- subset(data_all, select = -c(get(fe)))
#'       fixed_effect <- data_all[, fe]
#'       # only one fe - do not have to add fe from fe part to formula
#'       #demean_data <- fixest::demean(data, fixed_effect)
#'       #demean_data <- as.data.frame(demean_data)
#'       model_frame <- model.frame(update(fml_wo_fe, ~ . +  0), data)
#'       X <- model.matrix(model_frame, data = data)
#'       Y <- model.response(model_frame)
#'       X <- collapse::fwithin(X, fixed_effect)#
#'       Y <- collapse::fwithin(Y, fixed_effect)
#'       
#'     } else if(numb_fe > 1 & is.null(fe)){
#'       # there are several candidates that can be projected out in bootstrap. choose the 
#'       # factor with the most groups
#'       fml_fe <- formula(paste0("~", paste0(names_fe, collapse = "+")))
#'       fixed_effects <- model.frame(formula = fml_fe, 
#'                                    data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                                    drop.unused.levels = TRUE)
#'       max_fe <- which.max(sapply(fixed_effects, function(x) length(unique(x))))
#'       fe <- names_fe[max_fe]
#'       # get the data
#'       fml <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       
#'       data_all <- model.frame(formula = fml, 
#'                               data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                               drop.unused.levels = TRUE)   
#'       
#'       # handle clusters and missings in clusters
#'       if(inherits(clustid, "formula")) {
#'         clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
#'         clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
#'       } else {
#'         clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
#'       }
#'       
#'       clustid_dims <- ncol(clustid)
#'       if(is.null(clustid_dims)){clustid_dims <- 1}
#'       
#'       # Important: Handling of omitted or excluded observations
#'       clustid <- clustid[-object$obsRemoved,]
#'       clustid_na <- is.na(clustid)
#'       delete_clustid <- rowSums(clustid_na)
#'       delete_clustid_sum <- sum(rowSums(clustid_na))
#'       
#'       if(delete_clustid_sum > 0){
#'         warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
#'       }
#'       
#'       data_all <- data_all[-delete_clustid, ]
#'       
#'       if(nrow(data_all) != nrow(clustid)){
#'         stop("data and clustid have different dimensions.")
#'       }
#'       
#'       # stop handling of clusters and missings in clusters
#'       
#'       #data_all <- data_all[-object$obsRemoved,]
#'       data <- subset(data_all, select = -c(get(fe)))
#'       fixed_effect <- data_all[, fe]
#'       # now update names_fe - exclude the fixed effect that will be projected out
#'       names_fe <- names_fe[names_fe != fe]
#'       # update fml_wo_fe (add factors that are not projected out)
#'       fml_wo_fe <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       model_frame <- model.frame(update(fml_wo_fe, ~ . +  0), data)
#'       X <- model.matrix(model_frame, data = data)
#'       Y <- model.response(model_frame)
#'       # now demean X and Y separately
#'       X <- collapse::fwithin(X, fixed_effect)
#'       Y <- collapse::fwithin(Y, fixed_effect)
#'     } else if(numb_fe > 1 & !is.null(fe)){
#'       fml <- formula(paste0(as.character(fml_wo_fe), "+", paste0(names_fe, collapse = "+")))
#'       data_all <- model.frame(formula = fml, 
#'                               data = eval(object$call$data, envir =  attr(object$terms, ".Environment")),
#'                               drop.unused.levels = TRUE)  
#'       
#'       # handle clusters and missings in clusters
#'       if(inherits(clustid, "formula")) {
#'         clustid_tmp <- expand.model.frame(object, clustid, na.expand = FALSE)
#'         clustid <- model.frame(clustid, clustid_tmp, na.action = na.pass)
#'       } else {
#'         clustid <- as.data.frame(clustid, stringsAsFactors = FALSE)
#'       }
#'       
#'       clustid_dims <- ncol(clustid)
#'       if(is.null(clustid_dims)){clustid_dims <- 1}
#'       
#'       # Important: Handling of omitted or excluded observations
#'       clustid <- clustid[-object$obsRemoved,]
#'       clustid_na <- is.na(clustid)
#'       delete_clustid <- rowSums(clustid_na)
#'       delete_clustid_sum <- sum(rowSums(clustid_na))
#'       
#'       if(delete_clustid_sum > 0){
#'         warning(paste(delete_clustid_sum, "observations deleted due to missing values in the cluster variables. In consequence, the bootstrap is estimated on a different sample than the regression model. If you want to guarantee that both bootstrap and model are estimated on the same sample, please delete missing values from the cluster variables prior to using boottest()."))
#'       }
#'       
#'       data_all <- data_all[-delete_clustid, ]
#'       
#'       if(nrow(data_all) != nrow(clustid)){
#'         stop("data and clustid have different dimensions.")
#'       }
#'       
#'       # stop handling of clusters and missings in clusters
#'       
#'       #data_all <- data_all[-object$obsRemoved,]
#'       data <- subset(data_all, select = -c(get(fe)))
#'       fixed_effect <- data_all[, fe]
#'       # now update names_fe - exclude the fixed effect that will be projected out
#'       names_fe <- names_fe[names_fe != fe]
#'       # update fml_wo_fe (add factors that are not projected out)
#'       fml_wo_fe <- formula(paste0(depvar, "~", paste0(c(names_covariates, names_fe), collapse = "+")))
#'       model_frame <- model.frame(update(fml_wo_fe, ~ . +  0), data)
#'       X <- model.matrix(model_frame, data = data)
#'       Y <- model.response(model_frame)
#'     }
#'       
#'     N <- nrow(X)
#'     k <- ncol(X)
#'     
#'     fixed_effect_W <- fixed_effect
#'     levels(fixed_effect_W) <- 1 / table(fixed_effect)
#'     W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
#'     n_fe <- length(unique(fixed_effect))
#'   }
#'   
#' 
#'   # Part 4) preprocess clusters
#'   
#'   # Factors in our clustiding variables can potentially cause problems
#'   # Blunt fix is to force conversion to characters
#'   i <- !sapply(clustid, is.numeric)
#'   clustid[i] <- lapply(clustid[i], as.character)
#'   
#'   if(clustid_dims == 2){
#'     names(clustid) <- c("clustid_1", "clustid_2")
#'     clustid$clustid <- paste0(clustid$clustid_1, "-", clustid$clustid_2)
#'   }
#' 
#'   N_G <- sapply(clustid, function(x) length(unique(x)))
#'   
#'   #numb_clusters <- ncol(clustid)
#'   if(max(N_G) > 200){
#'     warning(paste("You are estimating a model with more than 200 clusters. Are you sure you want to proceed with bootstrap standard errors instead of asymptotic sandwich standard errors? The more clusters in the data, the longer the estimation process."))
#'   }
#'   
#'   R0 <- as.numeric(param == colnames(X))
#' 
#'   res_preprocess <- list(fixed_effect = fixed_effect, 
#'                          param = param, 
#'                          data = data, 
#'                          clustid = clustid, 
#'                          clustid_dims = clustid_dims, 
#'                          N = N, 
#'                          k = k, 
#'                          Y = Y, 
#'                          X = X, 
#'                          #depvar = depvar, 
#'                          #groupvars = groupvars, 
#'                          beta0 = beta0, 
#'                          clustid_dims, 
#'                          R0 = R0, 
#'                          N_G = N_G, 
#'                          alpha = alpha, 
#'                          n_fe = n_fe, 
#'                          W = W)
#'   
#'   if(clustid_dims == 1){
#'     class(res_preprocess) <- "oneclust"
#'   } else if(clustid_dims > 1){
#'     class(res_preprocess) <- "multclust"
#'   }
#'   res_preprocess  
#'   
#' }
