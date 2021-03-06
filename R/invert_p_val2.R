invert_p_val2 <- function(object, boot_iter, point_estimate, se_guess, clustid, sign_level, vcov_sign, impose_null, p_val_type, tol, maxiter) {
  
  #' Calculation of Confidence Sets
  #' 
  #' Inverts the bootstrap p-value and calculates confidence sets
  #' 
  #' @param object A  object of type boottest
  #' @param boot_iter An integer. Number of bootstrap iterations
  #' @param point_estimate A scalar. Point estimate of the coefficient of interest from the regression model
  #' @param se_guess A scalar vector of dimension 2. A guess of the standard error that initiates the p-value inversion.
  #' @param clustid A vector with the clusters
  #' @param sign_level A numeric between 0 and 1. Sets to confidence level: sign_level = 0.05 returns 0.95% confidence intervals
  #' @param vcov_sign Controls addition / substraction of individual covariance matrices for multiway clustering
  #' @param impose_null Logical. Controls if the null hypothesis is imposed on the bootstrap dgp or not. Null imposed - WCR - by default. If FALSE, unrestricted WCU
  #' @param p_val_type type Type of p-value. By default "two-tailed". Other options: "equal-tailed", ">", "<"
  #' @param tol the desired accuracy (convergence tolerance) for confidence interval inversion. 1e-6 by default.
  #' @param maxiter maximum number of iterations for confidence interval inversion. 10 by default.
  #' @importFrom stats uniroot
  #' 
  #' @return A list containing the calculated confidence interval, the t-statistics
  #'         and corresponding p-values used in the grid search. 
  
  check_arg(point_estimate, "numeric scalar")
  check_arg(se_guess, "numeric scalar")
  check_arg(clustid, "data.frame")
  
  if (sign_level > 1 | sign_level < 0) {
    stop("Significance level needs to be between 0 and 1.", 
         call. = FALSE
    )
  }

  
  ABCD <- object$ABCD
  # note: A, B are matrices, CC, CD and DD are lists
  A <- ABCD$A
  B <- ABCD$B
  CC <- ABCD$CC
  CD <- ABCD$CD
  DD <- ABCD$DD
  
  small_sample_correction <- object$small_sample_correction
  
  p_val_null2_x <- function(beta0, sign_level) {
    p_val_null2(beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction = small_sample_correction, point_estimate = point_estimate, impose_null = impose_null, p_val_type = p_val_type)$p_val - sign_level
  }
  
  p_val_null2_x_cmp <- compiler::cmpfun(p_val_null2_x)
  
  # Step 1: find two values x where p(x) crosses the sign_level
  
  check <- FALSE
  inflate_se <- c(1, 2, 3, 5, 10, 15, 25, 50, 100, 500, 1000, 5000, 10000, 50000, 500000, 1000000, 100000000)
  #inflate_se <- 2^(0:10)
  len_inflate <- length(inflate_se)
  j <- 1
  
  # note: for more extreme test values, p-values should be weakly decreasing
  # this is currently not the case for small numbers of clusters. why? 
  while (check == FALSE & j <= len_inflate) {
    # print("check")
    if (j > len_inflate) {
      break("Boottest confidence set calculation fails because no p-value < sign_level could succesfully
            be guessed.",
            call. = FALSE
      )
    }
    #cat("j: ", j, "\n")
    # start guesses by taking sandwich cluster confidence intervals + inflation factor
    starting_vals <- as.numeric(point_estimate + c(-1, 1) * inflate_se[j] * se_guess)
    #cat("starting_vals: ",starting_vals, "\n")
    # take 25 starting values in between the guesses
    p_start <- rep(NaN, length(starting_vals))
    
    # find starting value
    for (i in 1:length(starting_vals)) {
      p_start[i] <- p_val_null2_x_cmp(starting_vals[i], sign_level = sign_level)
    }
    #cat("p-val 1: ",p_start, "\n")
    p_start <- p_start + sign_level
    #cat("p-val 2",p_start, "\n")
    if (sum(p_start < sign_level) == 2) {
      check <- TRUE
    }
    j <- j + 1
  }

  # check if root finding was successful: find two confidence interval boundaries 
  # x1 & x2 so that 0 < p(x) < sign_level for x = {x1, x2}
  if(check == FALSE){
    stop(("The inflation factor for initial guesses for standard errors was not large 
         enough.
         In consequence, the root-finding procedure to compute confidence intervals
         via p-value inversion could not be initiated.\n
         In a future release, it will be possible to specify a costum inflation factor 
         as a function argument to boottest().
         Until then, you can still use boottest() to calculate p-values by setting the
         boottest() function argument conf_int to FALSE."))
  }
  
  test_vals <- seq(starting_vals[1], starting_vals[2], (starting_vals[2] - starting_vals[1]) / 25)
  
  # later: don't have to evaluate all guesses at all points - extreme points suffice - if < sign_level at both extreme points
  # then evaluate all 26 points
  p <- rep(NaN, length(test_vals))
  
  
  for (i in 2:(length(test_vals) - 1)) {
    p[i] <- p_val_null2_x_cmp(test_vals[i], sign_level)
  }

  # substract sign_level in function so that I will not need to
  # do it in root finding algorithm, but then I will need to add
  # sign_level here
  p <- p + sign_level
  
  p[1] <- p_start[1]
  p[26] <- p_start[2]
  
  crossings <- (p <= sign_level) - (p > sign_level)
  
  x_crossings <- rep(NA, length(test_vals))
  # should this be 2:25?
  for (i in 1:26) {
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }
  
  # find starting values: max of test_vals_higher and min of test_vals_lower
  test_vals_higher <- (test_vals[which(x_crossings == 1)])[3:4]
  test_vals_higher_max <- test_vals_higher[which.min(abs(test_vals_higher))]
  
  test_vals_lower <- (test_vals[which(x_crossings == 1)])[1:2]
  test_vals_lower_max <- test_vals_lower[which.min(abs(test_vals_higher))]
  
  # error if no proper starting vals found
  if (length(test_vals_higher_max) == 0 || length(test_vals_lower_max) == 0) {
    stop("
         Technical note: test_vals_lower or test_vals higher is logical(0). This means that no 
          starting value x with property |p(x1) < 0.05| has been found for one of the 
          confidence set boundary guesses. As a consequence, the numerical root finding
         will not work.", 
         call. = FALSE
    )
  }
  
  
  # loop over higher and lower starting values
  # optimizer: stats::uniroot (secant method), max 10 iterations & abs tol 1e-6
  res <- lapply(list(test_vals_lower, test_vals_higher), function(x) {
    p_val_null2_x_sign_level <- function(x) {
      p_val_null2_x_cmp(x, sign_level = sign_level)
    }
    tmp <- suppressWarnings(stats::uniroot(f = p_val_null2_x_sign_level, 
                                           lower = min(x), 
                                           upper = max(x), 
                                           tol = tol,
                                           maxiter = maxiter))
    
    tmp$root
  })
  
  # collect results from optimizer
  conf_int <- unlist(res)
  
  # package results
  res_all <- list(
    conf_int = conf_int,
    p_test_vals = p,
    test_vals = test_vals
  )
  
  res_all
}
