 tidy <- function(x, ...){
   #' S3 method to summarize objects of class boottest into tidy data.frame
   #'@export 
   #'@param x object of type boottest
   #'@param ... other arguments
   UseMethod("tidy", x)
 }
  
#'  summary <- function(x, ...){
#'    #' S3 method to summarize objects of class boottest 
#'    #'@export 
#'    #'@param x object of type boottest
#'    #'@param ... other arguments
#'    UseMethod("summary", x)
#'  }
#' 
#' plot <- function(x, ...){
#'    #' S3 generic to plot bootstrap t statistics
#'    #'@export 
#'    #'@param x object of type boottest
#'    #'@param ... other arguments
#'    UseMethod("plot", x)
#' }

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

summary.boottest <- function(object, digits = 3){
  #' S3 method to summarize objects of class boottest 
  #'@export 
  #'@param object object of type boottest
  #'@param digits rounding of output
  #'@method summary boottest

  
  stopifnot(inherits(object, "boottest"))

  
  N <- object$N
  B <- object$B
  alpha <- object$alpha
  signif_level <- paste0((1 - alpha) * 100, "%")
  call <- object$call
  N_G <- object$N_G
  B <- object$B
  #clustid <- 
  estim_function <- class(object$regression)
  if(length(object$clustid) == 1){
    clustering_type <- "oneway"  
    numb_clusters <- N_G
  } else if(length(object$clustid) == 2){
    clustering_type <- "twoway"
    numb_clusters <- paste(N_G[1], "x", N_G[2])
  }
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
  
  print(call)
  cat("\t\n", 
      sprintf("Observations: %s\n", N), 
      sprintf("Bootstr. Iter: %s\n", B), 
      sprintf("Clustering: %s\n", clustering_type), 
      sprintf("Confidence Sets: %s\n", signif_level),
      sprintf("Number of Clusters: %s\n", numb_clusters), 
      
      #sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)), 
      sprintf("%s\n", "")) 

  return(tidy_object)
}

plot.boottest <- function(object){
  
  #' Plot the bootstrap distribution of t-statistics
  #' @param object An object of type boottest
  #' @importFrom graphics abline grid lines
  #' @method plot boottest
  #' @export 
  stopifnot(inherits(object, "boottest"))
  
  test_vals <- object$test_vals
  p_test_vals <- object$p_test_vals
  conf_int <- object$conf_int
  alpha <- object$alpha
  
  graphics::plot(x = test_vals, y = p_test_vals,  type = "b", pch = 20, lty = 2, xlab = "Constraint", ylab = "p-value")
  lines(test_vals, p_test_vals, type = "l", lty = 1)
  abline(v=conf_int[1], col = "blue")
  abline(v=conf_int[2], col = "blue")
  abline(h = alpha, col = "red")
  
  
}
