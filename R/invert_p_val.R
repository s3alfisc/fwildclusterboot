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
#' @noRd

invert_p_val <- function(object, boot_iter, point_estimate, se_guess, clustid, sign_level, vcov_sign, impose_null, p_val_type, tol, maxiter) {
  
  check_arg(point_estimate, "numeric scalar")
  check_arg(se_guess, "numeric scalar")
  check_arg(clustid, "data.frame")
  check_arg(sign_level, "numeric scalar")
  #check_arg(vcov_sign)
  #check_arg(impose_null)
  #check_arg()
  #check_arg(tol)
  #check_arg(maxiter)
  

  # retain information from input "object"
  ABCD <- object$ABCD
  # note: A, B are matrices, CC, CD and DD are lists
  A <- ABCD$A
  B <- ABCD$B
  CC <- ABCD$CC
  CD <- ABCD$CD
  DD <- ABCD$DD
  
  small_sample_correction <- object$small_sample_correction
  
  # pass constant function values to p_val_null, substract sign.level & compile
  # function will be used to create grid points, and, based on the results
  # from the grid points, the root finding algorithm
  p_val_null2_x <- function(beta0, sign_level) {
    p_val_null2(beta0, A = A, B = B, CC = CC, CD = CD, DD = DD, clustid = clustid, boot_iter = boot_iter, small_sample_correction = small_sample_correction, point_estimate = point_estimate, impose_null = impose_null, p_val_type = p_val_type)$p_val - sign_level
  }
  
  p_val_null2_x_cmp <- compiler::cmpfun(p_val_null2_x)
  
  # --------------------------------------------------------------------- # 
  # The inversion of p-values to obtain confidence sets is handled in 
  # multiple steps: 
  # 
  # - find two spots (x1,x2;x3,x4) where p(x) crosses the
  #         significance level
  # - between the two starting values, create an equal spaced grid
  #         of 26 points between the starting values
  # - evaluate p(x) at all these points. 
  # - from the two spots (upper and lower) where p(x) is closest 
  #         to the significance level, start a root-finding alogorithm 
  #         with tolerance e=1e-06 and maxiter = 10 by default
  # --------------------------------------------------------------------- # 
  
  # --------------------------------------------------------------------- #
  # Step 1: find two values x where p(x) crosses the sign_level
  
  # define functions to find boundaries separately
  get_start_vals <- function(point_estimate, se_guess, sign_level, 
                             upper){
  
    check <- FALSE
    inflate_se <- c(2^(0:100 / 2))
    len_inflate <- length(inflate_se)
    j <- 1
    
    check_arg(upper, "logical scalar")
    
    while (check == FALSE & j <= len_inflate) {
      # cat(j, "\n")
      if (j > len_inflate) {
        break("Boottest confidence set calculation fails because no p-value < sign_level could succesfully
            be guessed.",
              call. = FALSE
        )
      }
      # start guesses by taking confidence interval guess times inflation factor
      if(upper == TRUE){
        starting_vals <- as.numeric(point_estimate + inflate_se[j] * se_guess)
      } else if(upper == FALSE){
        starting_vals <- as.numeric(point_estimate - inflate_se[j] * se_guess)
      }

      # find starting value
      p_candidate <- p_val_null2_x_cmp (beta0 = starting_vals, sign_level = sign_level)
    
      # need to add sign_level
      p_candidate <- p_candidate + sign_level
      # cat(p_candidate, "\n")
      
      # smaller than: less "extreme" values -> higher p-values; 
      # -> sign. level will be crossed 
      if (p_candidate < sign_level) {
        check <- TRUE
      }
      
      j <- j + 1
    } 
    res <- c(p_candidate, starting_vals)
    names(res) <- c("p_candidate", "starting_vals")
    res
  }
 
  # find starting values
  start_vals <- lapply(c(TRUE, FALSE), function(x) get_start_vals(
                                                             point_estimate = point_estimate, 
                                                             se_guess = se_guess, 
                                                             sign_level = sign_level, 
                                                             upper = x))
  
  # order: first the "lower" p-value, then the "upper"
  p_start <- c(start_vals[[2]]["p_candidate"], start_vals[[1]]["p_candidate"])
  starting_vals <- c(start_vals[[2]]["starting_vals"], start_vals[[1]]["starting_vals"])

  # check if root finding was successful: find two confidence interval boundaries 
  # x1 & x2 so that 0 < p(x) < sign_level for x = {x1, x2}
  if(sum(p_start < sign_level) < 2){
    stop(("The inflation factor for initial guesses for standard errors was not large enough. In consequence, the root-finding procedure to compute confidence intervalsvia p-value inversion could not be initiated. In a future release, it will be possible to specify a costum inflation factor as a function argument to boottest(). Until then, you can still use boottest() to calculate p-values by setting the boottest() function argument conf_int to FALSE."))
  }
  
  # ---------------------------------------------------------------------------------------------------- # 
  # Step 2: create equal spaced grid between starting values
  grid_vals <- seq(from = min(starting_vals), to = max(starting_vals), by = abs(starting_vals[2] - starting_vals[1]) / 25)
  
  # later: don't have to evaluate all guesses at all points - extreme points suffice - if < sign_level at both extreme points
  # then evaluate all 26 points
  p <- rep(NaN, length(grid_vals))
  
  # evaluate all test values
  for (i in 2:(length(grid_vals) - 1)) {
    p[i] <- p_val_null2_x_cmp(grid_vals[i], sign_level = sign_level)
  }
  # substract sign_level in function so that I will not need to
  # do it in root finding algorithm, but then I will need to add
  # sign_level here
  p <- p + sign_level
  # add starting p-vals (sign_level already added)
  p[1] <- p_start[1]
  p[26] <- p_start[2]
  
  # -------------------------------------------------------------------- # 
  # Step 3: find spots where p crosses the significance level
  
  crossings <- (p <= sign_level) - (p > sign_level)
  x_crossings <- rep(NA, length(grid_vals))
  # should this be 2:25?
  for (i in 1:26) {
    x_crossings[i] <- ifelse(crossings[i] + crossings[i + 1] == 0 || crossings[i] + crossings[i - 1] == 0, 1, 0)
  }

  # find starting values: max of grid_vals_higher and min of grid_vals_lower
  grid_vals_lower <- grid_vals[which(x_crossings == 1)][1:2]
  grid_vals_higher <- (grid_vals[which(x_crossings == 1)])[3:4]

  # loop over higher and lower starting values
  # optimizer: stats::uniroot (secant method), max 10 iterations & abs tol 1e-6
  res <- lapply(list(grid_vals_lower, grid_vals_higher), function(x) {
    p_val_null2_x_sign_level <- function(x) {
      p_val_null2_x_cmp (x, sign_level = sign_level)
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
    p_grid_vals = p,
    grid_vals = grid_vals
  )
  
  res_all
}
