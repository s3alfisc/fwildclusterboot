to_integer <- function(vec) {

  #' Transform vectors of all types safely to integer vectors
  #' @param vec A vector
  #' @return An integer vector
  #' @noRd

  dreamerr::check_arg(vec, "MBT vector")

  unique_vec <- unique(vec)
  int_vec <- rep(NA, length(vec))
  for (x in seq_along(unique_vec)) {
    int_vec[which(vec == unique_vec[x])] <- x
  }
  as.integer(int_vec)
}
