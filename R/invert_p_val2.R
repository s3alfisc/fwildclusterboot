
invert_p_val2 <- function(object, B, point_estimate, se_guess, clustid, alpha, vcov_sign){
  
  #' Inverts the bootstrap p-value and calculates confidence sets
  #'@param object A  object of type boottest
  #'@param B An integer. Number of bootstrap iterations
  #'@param point_estimate A scalar. Point estimate of the coefficient of interest from the regression model
  #'@param se_guess A scalar vector of dimension 2. A guess of the standard error that initiates the p-value inversion. 
  #'@param clustid A vector with the clusters
  #'@param alpha A numeric between 0 and 1. Sets to confidence level: alpha = 0.05 returns 0.95% confidence intervals
  #'@param vcov_sign Controls addition / substraction of individual covariance matrices for multiway clustering
  #'@importFrom utils setTxtProgressBar txtProgressBar 
  #'@export

  check_arg(point_estimate, "numeric scalar")
  check_arg(se_guess, "numeric scalar")
  check_arg(clustid, "data.frame")
  
  if(alpha > 1 | alpha < 0){stop("Significance level needs to be between 0 and 1.")}
  boot_iter <- B
  rm(B)
  
  G <- sapply(clustid, function(x) length(unique(x)))
  small_sample_correction <- G / (G - 1)
  
  # prepare summation of individual terms for multiway clustering
    small_sample_correction <- vcov_sign * small_sample_correction 
  
  
  ABCD <- object$ABCD
  A <- ABCD$A
  B <- ABCD$B
  CC <- ABCD$CC
  CD <- ABCD$CD
  DD <- ABCD$DD
  
  p_val_null2_x <- function(beta0, alpha){
    p_val_null2(beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction = small_sample_correction)$p_val - alpha
  }
  
  p_val_null2_x_cmp <- compiler::cmpfun(p_val_null2_x)
  
  # p-value must cross alpha
  check <- FALSE
  inflate_se <- c(2, 3, 5, 10, 15, 25, 50, 100)
  len_inflate <- length(inflate_se)
  j <- 1
  
  while(check == FALSE){
    #print("check")
    if(j > len_inflate){
      break("Boottest confidence set calculation fails because no p-value < alpha could succesfully
            be guessed.")
    }
    # start guesses by taking sandwich cluster confidence intervals + inflation factor
    starting_vals <- as.numeric(point_estimate + c(-inflate_se[j], inflate_se[j]) * se_guess)
    # take 25 starting values in between the guesses
    p_start <- rep(NaN, length(starting_vals))
    
    # find starting value
    for(i in 1:length(starting_vals)){
      p_start[i] <- p_val_null2_x_cmp(starting_vals[i], alpha = alpha) 
    }
    p_start <- p_start + alpha 
    
    if(sum(p_start < alpha) == 2){
      check <- TRUE
    }
    j <- j + 1
  }
  
  test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1])/ 25)      
  
  # later: don't have to evaluate all guesses at all points - extreme points suffice - if < alpha at both extreme points
  # then evaluate all 26 points
  p <- rep(NaN, length(test_vals))
  
  
  pb = txtProgressBar(min = 0, max = (length(test_vals) - 2), initial = 0, style = 3, char = "-") 
  
  for(i in 2:(length(test_vals) - 1)){
    p[i] <- p_val_null2_x_cmp(test_vals[i], alpha) 
    setTxtProgressBar(pb,i)
  }
  close(pb)

  # substract alpha in function so that I will not need to 
  # do it in root finding algorithm, but then I will need to add 
  # alpha here
  p <- p + alpha 
  
  p[1] <- p_start[1]
  p[26] <- p_start[2]

  crossings <-  (p <= alpha) - (p > alpha)
  
  x_crossings <- rep(NA, length(test_vals))
  # should this be 2:25?
  for(i in 1:26){
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }
  
  # find starting values: max of test_vals_higher and min of test_vals_lower
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]  
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]
  
  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]  
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]
  
  # error if no proper starting vals found
  if(length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0){
    stop("test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property |p(x1) < 0.05| has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
         will not work.")
  }  
  
  
  # loop over higher and lower starting values
  # optimizer: stats::uniroot (secant method), max 10 iterations & abs tol 1e-6
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x){
    
    p_val_null2_x_alpha <- function(x){
      p_val_null2_x_cmp(x, alpha = alpha)
    }
    tmp <- suppressWarnings(stats::uniroot(f = p_val_null2_x_alpha, lower = min(x), upper = max(x), tol = 1e-6, maxiter = 10))
    
    tmp$root
  })
  
  # collect results from optimizer
  conf_int <- unlist(res)
  
  # package results
  res_all <- list(conf_int = conf_int, 
                  p_test_vals = p, 
                  test_vals = test_vals)
  
  res_all
  
  
}
