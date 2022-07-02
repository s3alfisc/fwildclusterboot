get_bootstrap_pvalue <- function(p_val_type, t_stat, t_boot) {

  #' get bootstrapped p-value based on bootstrapped t-stats
  #' @param p_val_type Character vector of length 1. Type of p-value.
  #'        Options include "two-tailed", "equal-tailed", ">" and "<".
  #' @param t_stat The original t-statistic
  #' @param t_boot The bootstrapped t-statistics
  #' @return A bootstrapped p-value
  #' @noRd

  if (p_val_type == "two-tailed") {
    p_val <- mean(abs(t_stat) < abs(t_boot), na.rm = FALSE)
  } else if (p_val_type == "equal-tailed") {
    p_l <- mean(t_stat < t_boot, na.rm = FALSE)
    p_h <- mean(t_stat > t_boot, na.rm = FALSE)
    p_val <- 2 * min(p_l, p_h, na.rm = FALSE)
  } else if (p_val_type == ">") {
    p_l <- mean(t_stat < t_boot, na.rm = FALSE)
    p_val <- p_l
  } else if (p_val_type == "<") {
    p_h <- mean(t_stat > t_boot, na.rm = FALSE)
    p_val <- p_h
  }

  p_val
}
