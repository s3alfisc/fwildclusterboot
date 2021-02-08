tidy.boottest <- function(object, ...) {
  #' S3 method to summarize objects of class boottest into tidy data.frame
  #' @param object object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics tidy
  #' @export
  #' @method tidy boottest

  stopifnot(inherits(object, "boottest"))
  #dreamerr::validate_dots(stop = TRUE)
  
  # if(class(object$regression) == "felm"){
  #   estimate <- object$regression$coefficients[rownames(object$regression$coefficients) == object$param]
  # } else{
  #   estimate <- object$regression$coefficients[names(object$regression$coefficients) == object$param]
  # }

  term <- object$param
  estimate <- object$point_estimate
  statistic <- object$t_stat
  p.value <- object$p_val
  #std.error <- 1
  conf.low <- min(object$conf_int)
  conf.high <- max(object$conf_int)

  res <- data.frame(term, estimate, statistic, p.value, conf.low, conf.high)
  #res <- tibble::as_tibble(res)
  #rownames(res) <- NULL
  return(res)
}

summary.boottest <- function(object, digits = 3, ...) {
  #' S3 method to summarize objects of class boottest
  #' @param object object of type boottest
  #' @param digits rounding of output
  #' @param ... Further arguments passed to or from other methods.
  #' @method summary boottest
  #' @export
  

  stopifnot(inherits(object, "boottest"))
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
  if (length(object$clustid) == 1) {
    clustering_type <- "oneway"
    numb_clusters <- N_G
  } else if (length(object$clustid) == 2) {
    clustering_type <- "twoway"
    numb_clusters <- paste(N_G[1], "x", N_G[2])
  }
  numb_clusters <- object$N_G

  # if(class(object$regression) %in% c("lm", "lm_robust", "felm")){
  #  adj_r_squared <- summary(object$regression)$adj.r.squared
  # } else{
  #  adj_r_squared <- NA
  # }

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
  
  # treatment_name <- rownames(tidy_object)
  # tidy_object <- as.data.frame(round(tidy_object, digits = 3))
  # rownames(tidy_object) <- treatment_name

  print(call)
  cat(
    "\t\n",
    sprintf("Observations: %s\n", N),
    sprintf("Bootstr. Iter: %s\n", B),
    sprintf("Bootstr. Type: %s\n", type),
    sprintf("Clustering: %s\n", clustering_type),
    sprintf("Confidence Sets: %s\n", signif_level),
    sprintf("Number of Clusters: %s\n", Reduce(paste, numb_clusters)),

    # sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)),
    sprintf("%s\n", "")
  )

  return(tidy_object)
}

plot.boottest <- function(x, ...) {

  #' Plot the bootstrap distribution of t-statistics
  #' @param x An object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @method plot boottest
  #' @export

  stopifnot(inherits(x, "boottest"))
  dreamerr::validate_dots(stop = TRUE)
  
  test_vals <- x$test_vals
  p_test_vals <- x$p_test_vals
  conf_int <- x$conf_int
  sign_level <- x$sign_level

  graphics::plot(x = test_vals, y = p_test_vals, type = "b", pch = 20, lty = 2, xlab = "Constraint", ylab = "p-value")
  graphics::lines(test_vals, p_test_vals, type = "l", lty = 1)
  graphics::abline(v = conf_int[1], col = "blue")
  graphics::abline(v = conf_int[2], col = "blue")
  graphics::abline(h = sign_level, col = "red")
}

glance.boottest <- function(x, ...){
  
  #' S3 method to summarize objects of class boottest
  #' @param x object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics glance
  #' @method glance boottest
  #' @export

  stopifnot(inherits(x, "boottest"))
  broom::glance(eval(x$call$object))

}

