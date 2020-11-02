invert_p_val_fwc <- function(object, data, clustid, X, Y, param, R0, B, N, k, seed, N_g, invXX, v, Xr, XinvXXr, SXinvXXRX, sign_level = 0.95){
  
  if(sign_level > 1 | sign_level < 0){stop("Significance level needs to be between 0 and 1.")}
  sign_level <- 1 - sign_level
  # this needs to be rewritten so that correct fixed effects are used
  if(class(object) == "lm"){
    lm_robust_fit <- estimatr::lm_robust(eval(object$call$formula), clusters = as.factor(clustid$clustid), data = data, se_type = "stata")
    estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
    st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  } else if(class(object) == "lm_robust"){
    lm_robust_fit <-  estimatr::lm_robust(object$call$formula, 
                               clusters = eval(object$call$clusters), 
                               data = data, 
                               se_type = "stata")
    estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
    st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  } else if(class(object) == "felm"){
    lm_robust_fit <-  estimatr::lm_robust(formula(Formula::Formula(eval(object$call$formula, envir =  attr(object$terms, ".Environment"))), lhs = 1, rhs = 1), 
                               clusters = clustid[, "clustid"], 
                               data = data, se_type = "stata")
    estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
    st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  } else if(class(object) == "fixest"){
    fit <- fixest:::summary.fixest(object, se = "cluster", cluster = clustid)
    estimate <- fit$coefficients[names(object$coefficients) == param]
    st_error_guess <- fit$se[names(object$se) == param]
  } else {
    stop("Function only designed for objects of type lm, lm_robust, felm and feols.")
  }

  
  # --------------------------------------------------------------------------------------------- #
  # start inversion 
  #tidy_obj <- broom::tidy(lm_robust_fit)
  #setDT(tidy_obj)
  #estimate <- as.numeric(tidy_obj[term == param, "estimate"])
  #st_error_guess <- as.numeric(tidy_obj[term == param, "std.error"])
  # estimate <- lm_robust_fit$coefficients[names(lm_robust_fit$coefficients) == param]
  # st_error_guess <- lm_robust_fit$std.error[names(lm_robust_fit$coefficients) == param]
  
  p_val_null <- function(beta0, R0, Y, X, Xr,XinvXXr, clustid, 
                         SXinvXXRu_prep,  k,  N, v, B){
    
    Yr <- Y -Xr0 * beta0
    u_hat <- Yr - XrinvXrXrtXr %*% Yr # N x 1 matrix 
    #XrinvXrXrtXr <- Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr)
    
    SXinvXXRu_prep <- data.table::data.table(prod = XinvXXr * matrix(rep(u_hat, 1), N, 1) , clustid = clustid) 
    SXinvXXRu <- as.matrix(SXinvXXRu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL])
    if(ncol(SXinvXXRu) == 1){
      SXinvXXRu <- as.vector(SXinvXXRu)
    }
    
    #a <- function(){
    SXu_prep <- data.table::data.table(prod = X * matrix(rep(u_hat, k), N, k), clustid = clustid) 
    SXu <- as.matrix(SXu_prep[, lapply(.SD, sum), by = "clustid.clustid"][, clustid.clustid := NULL]) 
    #}
    #b <- function(){
    #Sxu1 <- as.matrix(aggregate(X * matrix(rep(u_hat, k), N, k), clustid, sum))
    #}
    #c <-function(){
    #  Sxu2 <- aggregate.Matrix(x = X * matrix(rep(u_hat, k), N, k), groupings = clustid, fun = "sum")
    #}
    #benchmark(a(), b(), c(), replications = 1000)
    
    #d <- function(){
    #  mat <- X * matrix(rep(u_hat, k), N, k)
    #  lapply(ncol(mat), function(i) lm(mat[,i] ~ 1 + factor(clustid$clustid)))
    #}
    
    #benchmark(a(), d(), replications = 100)
    
    
    numer <- SXinvXXRu %*% v 
    J <- (diag(SXinvXXRu) - SXinvXXRX_invXX %*% t(SXu)) %*% v  
    t <- abs(numer)  / sqrt(colSums(J * J))    # note: absolute value is taken here - no negative t-stats
    t_boot <- t[2:(B + 1)]
    mean(abs(t[1] - beta0) < (t_boot))
  }
  
  # can be smaller than zero bc of -0.5
  p_val_null_x <- function(beta0){
    p_val_null(beta0, R0 = R0, Y = Y, X = X, Xr = Xr, XinvXXr = XinvXXr, clustid = clustid, 
               SXinvXXRu_prep = SXinvXXRu_prep, k = k, N = N, v = v, B = B) - sign_level
  }
  
  # p-value must cross sign_level
  check <- FALSE
  inflate_se <- c(1, 3, 5, 10)
  j <- 1
  while(check == FALSE){
    
    if(j > 4){
      break("Boottest confidence set calculation fails because no p-value < sign_level could succesfully
            be guessed.")
    }
    starting_vals <- as.numeric(estimate + c(-inflate_se[j], inflate_se[j]) * st_error_guess)
    
    test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
    
    XrinvXrXrtXr <- Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr)
    SXinvXXRX_invXX <- SXinvXXRX  %*% invXX
    Xr0 <- X[, which(R0 == 1)]
    
    
    #benchmark(p_val_null_x(test_vals[1]))
    # get test values
    p <- rep(NaN, length(test_vals))
    
    for(i in 1:length(test_vals)){
      p[i] <- p_val_null_x(test_vals[i]) 
    }
    
    p <- p + sign_level
    
    #if(sum(p < sign_level) < 1){warning("Need to djust starting values: they are not p < sign_level. Therefore, choose more
    #                              extreme starting values.")}
    
    crossings <-  (p < sign_level) - (p > sign_level)
    
    check <- mean(crossings == -1) != 1
    j <- j + 1
    check    
  }
 

  x_crossings <- rep(NA, length(test_vals))
  for(i in 1:25){
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }
  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]

  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]

  
  secant_method <- function(f, x1, x2, num = 10, eps = 1e-06, eps1 = 1e-06){
    i = 0
    while ((abs(x1 - x2) > eps) & (i < num)) {
      c = x2 - f(x2) * (x2 - x1)/(f(x2) - f(x1))
      x1 = x2
      x2 = c
      i = i + 1
    }
    
    if (abs(f(x2)) < eps1) {
      success <- "finding root is successful"
    }
    success <- "finding root is fail"
    
    res <- 
      list(root = x2, 
      function_val = f(x2),
      success = success)
 
    res
  }
  
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
    #tmp <-  NLRoot::SMfzero(p_val_null_x , x1 = min(x), x2 = max(x), num = 10, eps = 1e-06)
    #tmp <- secant_method(p_val_null_x, x1 = min(x), x2 = max(x))
    #tmp
    #tmp <- pracma::newtonRaphson(p_val_null_x, x0 =  x, dfun = NULL, maxiter = 25, tol = 1e-4)
    #tmp$root
    #tmp <- pracma::fzero(p_val_null_x , x = test_vals_higher, maxiter = 10, tol = 1e-12)
    #tmp$x
    tmp <- pracma::bisect(p_val_null_x , a = min(x), b = max(x), maxiter = 10)
    tmp$root
  })


  conf_int <- unlist(res)
  #p_val_null_x(conf_int[1])
  #p_val_null_x(conf_int[2] + 0.00001)
  
  
  return(conf_int)
  #conf_int
  #lm_robust_fit
  #summary(lm_fit)  
  
  #toc()
  
}
