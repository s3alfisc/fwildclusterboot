tidy.waldboottest <- function(object, ...) {
  #' S3 method to summarize objects of class waldboottest into tidy data.frame
  #' @param object object of type waldboottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics tidy
  #' @export
  #' @method tidy waldboottest
  #' @return A tidy data.frame with estimation results for objects of type
  #'         waldboottest
  
  stopifnot(inherits(object, "waldboottest"))
  #dreamerr::validate_dots(stop = TRUE)
  
  R <- object$R
  hypothesis <- "Multivariate waldboottest"
  
  term <- hypothesis
  estimate <- object$point_estimate
  statistic <- object$t_stat
  p.value <- object$p_val
  #std.error <- NA
  conf.low <- min(object$conf_int)
  conf.high <- max(object$conf_int)
  
  res <- data.frame(term, estimate, statistic, p.value, conf.low, conf.high)
  
  return(res)
}

summary.waldboottest <- function(object, digits = 3, ...) {
  #' S3 method to summarize objects of class waldboottest
  #' @param object object of type waldboottest
  #' @param digits rounding of output. 3 by default
  #' @param ... Further arguments passed to or from other methods.
  #' @method summary waldboottest
  #' @export
  #' @return Returns result summaries for objects of type waldboottest
  
  
  
  stopifnot(inherits(object, "waldboottest"))
  dreamerr::validate_dots(stop = TRUE)
  
  N <- object$N
  B <- object$B
  sign_level <- object$sign_level
  signif_level <- paste0((1 - sign_level) * 100, "%")
  call <- object$call
  N_G <- object$N_G
  B <- object$B
  type <- ifelse(object$type %in% c("rademacher", "mammen", "norm", "webb"), object$type, "custom")
  # clustid <-
  estim_function <- class(object$regression)
  
  clustering_type <-  paste0(length(object$clustid), "-way")
  numb_clusters <- object$N_G
  
  tidy_names <- c("term","estimate", "statistic", "p.value", "conf.low", "conf.high")
  
  tidy_object <- lapply(tidy_names,
                        function(x){
                          if(is.numeric(tidy(object)[[x]])){
                            round(tidy(object)[[x]], digits = digits)
                          } else{
                            tidy(object)[[x]]
                          }
                        })
  
  tidy_object <- as.data.frame(tidy_object)
  names(tidy_object) <- tidy_names
  
  R <- object$R
  
  hypothesis <- "Multivariate waldboottest"
  
  print(call)
  cat(
    "\t\n",
    sprintf("Hypothesis: %s\n", hypothesis),
    sprintf("Observations: %s\n", N),
    sprintf("Bootstr. Iter: %s\n", B),
    sprintf("Bootstr. Type: %s\n", type),
    sprintf("Clustering: %s\n", clustering_type),
    sprintf("Confidence Sets: %s\n", signif_level),
    sprintf("Number of Clusters: %s\n", Reduce(paste, numb_clusters)),
    
    # sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)),
    sprintf("%s\n", "")
  )
  
  tidy(object)
  
}
