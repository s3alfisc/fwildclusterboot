 tidy <- function(x, ...){
   #' S3 method to summarize objects of class boottest into tidy data.frame
   #'@export 
   #'@param x object of type boottest
   #'@param ... other arguments
   UseMethod("tidy", x)
 }
  
 summarize_boot <- function(x, ...){
   #' S3 method to summarize objects of class boottest 
   #'@export 
   #'@param x object of type boottest
   #'@param ... other arguments
   UseMethod("summarize_boot", x)
 }


tidy.boottest <- function(object){
  #' S3 method to summarize objects of class boottest into tidy data.frame
  #'@export 
  #'@param object object of type boottest
  #'@method tidy boottest

  
  stopifnot(inherits(object, "boottest"))
  
  # if(class(object$regression) == "felm"){
  #   estimate <- object$regression$coefficients[rownames(object$regression$coefficients) == object$param]
  # } else{
  #   estimate <- object$regression$coefficients[names(object$regression$coefficients) == object$param]
  # }

  estimate <- object$point_estimate
  t_stat <- object$t_stat
  p_val <- object$p_val
  conf_int_lower <- min(object$conf_int)
  conf_int_upper <- max(object$conf_int)
  
  res <- data.frame(estimate, t_stat, p_val, conf_int_lower, conf_int_upper)
  colnames(res) <- c("Estimate", "t value", "Pr(>|t|)", "CI Lower", "CI Upper")
  #rownames(res) <- NA
  
  return(res)
  
  
}

summarize_boot.boottest <- function(object, digits = 3){
  #' S3 method to summarize objects of class boottest 
  #'@export 
  #'@param object object of type boottest
  #'@param digits rounding of output
  #'@method summarize_boot boottest

  
  stopifnot(inherits(object, "boottest"))

  
  N <- object$N
  B <- object$B
  depvar <- object$depvar
  #clustid <- 
  estim_function <- class(object$regression)
  numb_clusters <- object$N_G
  
  #if(class(object$regression) %in% c("lm", "lm_robust", "felm")){
  #  adj_r_squared <- summary(object$regression)$adj.r.squared
  #} else{
  #  adj_r_squared <- NA
  #}

  tidy_object <- round(tidy(object), digits)
  #treatment_name <- rownames(tidy_object)
  #tidy_object <- as.data.frame(round(tidy_object, digits = 3))
  #rownames(tidy_object) <- treatment_name
  
  cat("\t\n", 
      sprintf("OLS estimation, Dep.Var: %s\n", depvar), 
      sprintf("Estimation Function: %s\n", estim_function), 
      sprintf("Observations:%s\n", N), 
      sprintf("Standard-errors: Clustered  %s\n", ""), 
      sprintf("Number of Clusters:  %s\n", numb_clusters), 
      #sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)), 
      sprintf("%s\n", "")) 

  return(tidy_object)
}

plot_boot <- function(object){
  
  #' Plot the bootstrap distribution of t-statistics
  #' @param object An object of type boottest
  #' @importFrom graphics abline grid lines
  #' @export 
  stopifnot(inherits(object, "boottest"))
  
  test_vals <- object$test_vals
  p_test_vals <- object$p_test_vals
  conf_int <- object$conf_int
  
  plot(test_vals, p_test_vals,  type = "b", pch = 20, lty = 2, xlab = "Constraint", ylab = "p-value")
  lines(test_vals, p_test_vals, type = "l", lty = 1)
  abline(v=conf_int[1], col = "blue")
  abline(v=conf_int[2], col = "blue")
  abline(h = 0.05, col = "red")
  
  
}
