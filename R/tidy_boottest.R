 tidy <- function(object,...){
   #' S3 method to summarize objects of class boottest into tidy data.frame
   #'@export 
   #'@param object object of type boottest
   #'@param ... other arguments
   UseMethod("tidy", object)
 }
  
#'  summary <- function(object,...){
#'    #' S3 method to summarize objects of class boottest 
#'    #'@export 
#'    #'@param object object of type boottest
#'    #'@param ... other arguments
#'    UseMethod("summary", object)
#'  }
#' 
#' plot <- function(object,...){
#'    #' S3 generic to plot bootstrap t statistics
#'    #'@export 
#'    #'@param object object of type boottest
#'    #'@param ... other arguments
#'    UseMethod("plot", object)
#' }

tidy.boottest <- function(object, ...){
  #' S3 method to summarize objects of class boottest into tidy data.frame
  #'@export 
  #'@param object object of type boottest
  #'@param ... Further arguments passed to or from other methods.
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

summary.boottest <- function(object, digits = 3, ...){
  #' S3 method to summarize objects of class boottest 
  #'@export 
  #'@param object object of type boottest
  #'@param digits rounding of output
  #'@param ... Further arguments passed to or from other methods.
  #'@method summary boottest

  
  stopifnot(inherits(object, "boottest"))

  
  N <- object$N
  B <- object$B
  alpha <- object$alpha
  signif_level <- paste0((1 - alpha) * 100, "%")
  call <- object$call
  N_G <- object$N_G
  B <- object$B
  type <- ifelse(object$type %in% c("rademacher", "mammen", "norm", "webb"), object$type, "custom")
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
      sprintf("Bootstr. Type: %s\n", type), 
      sprintf("Clustering: %s\n", clustering_type), 
      sprintf("Confidence Sets: %s\n", signif_level),
      sprintf("Number of Clusters: %s\n", numb_clusters), 
      
      #sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)), 
      sprintf("%s\n", "")) 

  return(tidy_object)
}

plot.boottest <- function(x, ...){
  
  #' Plot the bootstrap distribution of t-statistics
  #' @param x An object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom graphics abline grid lines
  #' @method plot boottest
  #' @export 
  stopifnot(inherits(x, "boottest"))
  
  test_vals <- x$test_vals
  p_test_vals <- x$p_test_vals
  conf_int <- x$conf_int
  alpha <- x$alpha
  
  graphics::plot(x = test_vals, y = p_test_vals,  type = "b", pch = 20, lty = 2, xlab = "Constraint", ylab = "p-value")
  lines(test_vals, p_test_vals, type = "l", lty = 1)
  abline(v=conf_int[1], col = "blue")
  abline(v=conf_int[2], col = "blue")
  abline(h = alpha, col = "red")
  
  
}
