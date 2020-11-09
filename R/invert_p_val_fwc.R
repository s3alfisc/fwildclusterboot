invert_p_val_fwc <- function(object, data, clustid, X, Y, param, R0, B, N, k, seed, N_g, invXX, v, Xr, XinvXXr, SXinvXXRX, alpha = 0.05){
  
  if(alpha > 1 | alpha < 0){stop("Significance level needs to be between 0 and 1.")}
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
  SXinvXXRX_invXX <- SXinvXXRX  %*% invXX
  Xr0 <- matrix(X[, which(R0 == 1)], nrow(X), 1)
  
  Q <- Y - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Y))
  P <- Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Xr0)) - Xr0
  
  p_val_null <- function(beta0, Q, P, R0, Y, X, Xr,XinvXXr, clustid, 
                         SXinvXXRu_prep,  k,  N, v, B){
    
    #Yr <- Y - matrix(Xr0, length(Xr0), 1) %*% matrix(beta0, 1, length(beta0))
    #u_hat <- Yr - Xr %*% (solve(t(Xr) %*% Xr) %*% (t(Xr) %*% Yr)) # N x 1 matrix 
    u_hat <- Q + P %*% matrix(beta0, 1, length(beta0))
    #XrinvXrXrtXr <- Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr)
    
    SXinvXXRu_prep <- data.table::data.table(prod = as.vector(XinvXXr) * u_hat  , clustid = clustid) 
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
    p_val_null(beta0, P = P, Q = Q, R0 = R0, Y = Y, X = X, Xr = Xr, XinvXXr = XinvXXr, clustid = clustid, 
               SXinvXXRu_prep = SXinvXXRu_prep, k = k, N = N, v = v, B = B) - alpha
  }
  
  #XrinvXrXrtXr <- Xr %*% solve(t(Xr) %*% Xr) %*% t(Xr)
  # SXinvXXRX_invXX <- SXinvXXRX  %*% invXX
  # Xr0 <- X[, which(R0 == 1)]
  
  #p_val_null_x_vectorized <- Vectorize(p_val_null_x)
  # p-value must cross alpha
  check <- FALSE
  inflate_se <- c(2, 3, 5, 10)
  j <- 1
  while(check == FALSE){
    
    if(j > 4){
      break("Boottest confidence set calculation fails because no p-value < alpha could succesfully
            be guessed.")
    }
    # start guesses by taking sandwich cluster confidence intervals + inflation factor
    starting_vals <- as.numeric(estimate + c(-inflate_se[j], inflate_se[j]) * st_error_guess)
    # take 25 starting values in between the guesses
    test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
    

    
    #benchmark(p_val_null_x(test_vals[1]))
    # get test values

    # calculate the p-values for all 26 guesses
    p <- rep(NaN, length(test_vals))
    
    for(i in 1:length(test_vals)){
      p[i] <- p_val_null_x(test_vals[i]) 
    }
    
    # substract alpha in function so that I will not need to 
    # do it in root finding algorithm, but then I will need to add 
    # alpha here
    p <- p + alpha 
    
    #if(sum(p < alpha) < 1){warning("Need to djust starting values: they are not p < alpha. Therefore, choose more
    #                              extreme starting values.")}
    
    crossings <-  (p < alpha) - (p > alpha)
    
    x_crossings <- rep(NA, length(test_vals))
    for(i in 1:25){
      x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
    }
    
    check <- sum(x_crossings == 1, na.rm = TRUE) == 4
    j <- j + 1
    check    
  }
 


  
  #p_val[which(x_crossings == 1)]
  #test_vals[which(x_crossings == 1)]
  
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]

  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]

  if(length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0){
    stop("test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property x1 < 0.05 < x2 has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
         will not work.")
  }  
  # secant_method <- function(f, x1, x2, num = 10, eps = 1 / B, eps1 = 1 / B){
  #  i = 0
  #  while ((abs(x1 - x2) > eps) & (i < num)) {
  #    c = x2 - f(x2) * (x2 - x1)/(f(x2) - f(x1))
  #    x1 = x2
  #    x2 = c
  #    i = i + 1
  #  }
  #  
  #  if (abs(f(x2)) < eps1) {
  #    success <- "finding root is successful"
  #  }
  #  success <- "finding root is fail"
  #  
  #  res <- 
  #    list(root = x2, 
  #    function_val = f(x2),
  #    success = success)
  # 
  #  res
  # }
  
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
    #tmp <-  NLRoot::SMfzero(p_val_null_x , x1 = min(x), x2 = max(x), num = 10, eps = 1e-06)
    #tmp <- secant_method(p_val_null_x, x1 = min(x), x2 = max(x))
    #tmp
    #tmp <- try(pracma::newtonRaphson(p_val_null_x, x0 =  x, dfun = NULL, maxiter = 10, tol = 1/B))
    #tmp$root
    #tmp <- pracma::fzero(p_val_null_x , x = x, maxiter = 10, tol = 1 / B)
    #tmp$x
    tmp <- pracma::bisect(p_val_null_x , a = min(x), b = max(x))
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
