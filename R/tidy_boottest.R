tidy.boottest <- function(object, ...) {
  #' S3 method to summarize objects of class boottest into tidy data.frame
  #' @param object object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics tidy
  #' @export
  #' @method tidy boottest
  #' @return A tidy data.frame with estimation results for objects of type
  #'         boottest

  stopifnot(inherits(object, "boottest"))
  # dreamerr::validate_dots(stop = TRUE)

  if (object$boot_algo == "WildBootTests.jl") {
    R <- object$R[which(object$R != 0)]
    hypothesis <- paste(paste0(paste0(R, "*"), object$param, collapse = "+"), "=", object$r)
  } else {
    hypothesis <- paste(paste0(paste0(object$R, "*"), object$param, collapse = "+"), "=", object$r)
  }

  term <- hypothesis
  estimate <- object$point_estimate
  statistic <- object$t_stat
  p.value <- object$p_val
  # std.error <- NA
  if(!is.null(object$conf_int)){
    conf.low <- object$conf_int[1]
    conf.high <- object$conf_int[2]
  } else {
    conf.low <- conf.high <- NA
  }

  res <- data.frame(term, estimate, statistic, p.value, conf.low, conf.high)

  return(res)
}

summary.boottest <- function(object, digits = 3, ...) {
  #' S3 method to summarize objects of class boottest
  #' @param object object of type boottest
  #' @param digits rounding of output. 3 by default
  #' @param ... Further arguments passed to or from other methods.
  #' @method summary boottest
  #' @export
  #' @return Returns result summaries for objects of type boottest



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

  clustering_type <- paste0(length(object$clustid), "-way")
  numb_clusters <- object$N_G

  tidy_names <- c("term", "estimate", "statistic", "p.value", "conf.low", "conf.high")

  tidy_object <- lapply(
    tidy_names,
    function(x) {
      if (is.numeric(tidy.boottest(object)[[x]])) {
        round(tidy.boottest(object)[[x]], digits = digits)
      } else {
        tidy.boottest(object)[[x]]
      }
    }
  )

  tidy_object <- as.data.frame(tidy_object)
  names(tidy_object) <- tidy_names

  if (object$boot_algo == "WildBootTests.jl") {
    R <- object$R[which(object$R != 0)]
    hypothesis <- paste(paste0(paste0(R, "*"), object$param, collapse = "+"), "=", object$r)
  } else {
    hypothesis <- paste(paste0(paste0(object$R, "*"), object$param, collapse = "+"), "=", object$r)
  }

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

  return(tidy_object)
}

plot.boottest <- function(x, ...) {

  #' Plot the bootstrap distribution of t-statistics
  #' @param x An object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @method plot boottest
  #' @export
  #' @return A plot of bootstrap t-statistics under different null hypotheses


  stopifnot(inherits(x, "boottest"))
  dreamerr::validate_dots(stop = TRUE)

  if(is.null(x$conf_int)){
    stop("No plot method if boottest()'s function argument 'conf_int = FALSE'.")
  }
  test_vals <- x$grid_vals
  p_test_vals <- x$p_grid_vals
  conf_int <- x$conf_int
  sign_level <- x$sign_level
  xlab <- x$param

  graphics::plot(x = test_vals, y = p_test_vals, type = "b", pch = 20, lty = 2, xlab = xlab, ylab = "p-value")
  graphics::lines(test_vals, p_test_vals, type = "l", lty = 1)
  graphics::abline(v = conf_int[1], col = "blue")
  graphics::abline(v = conf_int[2], col = "blue")
  graphics::abline(h = sign_level, col = "red")
}

glance.boottest <- function(x, ...) {

  #' S3 method to glance at objects of class boottest
  #' @param x object of type boottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics glance
  #' @method glance boottest
  #' @export
  #' @return A single row summary "glance" of an object of type boottest
  #'         - lists characteristics of the input regression model

  stopifnot(inherits(x, "boottest"))
  broom::glance(eval(x$call))
  
}
