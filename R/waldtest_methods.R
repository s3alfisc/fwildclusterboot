tidy.mboottest <- function(object, ...) {
  #' S3 method to summarize objects of class mboottest into tidy data.frame
  #' @param object object of type mboottest
  #' @param ... Further arguments passed to or from other methods.
  #' @importFrom generics tidy
  #'
  #' @export
  #'
  #' @method tidy mboottest
  #' @return A tidy data.frame with estimation results for objects of type
  #'         mboottest

  stopifnot(inherits(object, "mboottest"))
  # dreamerr::validate_dots(stop = TRUE)

  statistic <- object$teststat
  p.value <- object$p_val

  res <- data.frame(teststat = statistic, p_val = p.value)

  return(res)
}

summary.mboottest <- function(object, digits = 3, ...) {
  #' S3 method to summarize objects of class mboottest
  #' @param object object of type mboottest
  #' @param digits rounding of output. 3 by default
  #' @param ... Further arguments passed to or from other methods.
  #' @method summary mboottest
  #'
  #' @export
  #'
  #' @return Returns result summaries for objects of type mboottest



  stopifnot(inherits(object, "mboottest"))
  dreamerr::validate_dots(stop = TRUE)

  N <- object$N
  B <- object$B
  call <- object$call
  N_G <- object$N_G
  B <- object$B
  type <- ifelse(object$type %in% c("rademacher", "mammen", "norm", "webb"), object$type, "custom")

  clustering_type <- paste0(length(object$clustid), "-way")
  numb_clusters <- object$N_G

  tidy_names <- c("F_stat", "p_val")

  # rounding
  tidy_object <- lapply(
    tidy_names,
    function(x) {
      if (is.numeric(tidy(object)[[x]])) {
        round(tidy(object)[[x]], digits = digits)
      } else {
        tidy(object)[[x]]
      }
    }
  )

  tidy_object <- as.data.frame(tidy_object)
  names(tidy_object) <- tidy_names

  hypothesis <- "Multivariate mboottest"

  print(call)
  cat(
    "\t\n",
    sprintf("Hypothesis: %s\n", hypothesis),
    sprintf("Observations: %s\n", N),
    sprintf("Bootstr. Iter: %s\n", B),
    sprintf("Bootstr. Type: %s\n", type),
    sprintf("Clustering: %s\n", clustering_type),
    sprintf("Number of Clusters: %s\n", Reduce(paste, numb_clusters)),

    # sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)),
    sprintf("%s\n", "")
  )

  tidy(object)
}

