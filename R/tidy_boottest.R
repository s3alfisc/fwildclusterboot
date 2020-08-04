tidy.boottest <- function(object){
  
  estimate <- lm$regression$coefficients[names(lm$regression$coefficients) == object$param]
  t_stat <- object$t_stat
  p_val <- object$p_val
  conf_int_lower <- min(object$conf_int)
  conf_int_upper <- max(object$conf_int)
  
  res <- data.frame(estimate, t_stat, p_val, conf_int_lower, conf_int_upper)
  colnames(res) <- c("Estimate", "t value", "Pr(>|t|)", "CI Lower", "CI Upper")
  #rownames(res) <- NA
  
  print(res, digits = 4)
  
  
}

summary.boottest <- function(object){
  
  stopifnot(inherits(object, "boottest"))

  
  N <- object$N
  B <- object$B
  depvar <- object$depvar
  #clustid <- 
  estim_function <- class(object$regression)
  numb_clusters <- object$N_G
  
  if(class(object$regression) %in% c("lm", "lm_robust", "felm")){
    adj_r_squared <- summary(lm_fit)$adj.r.squared
  } else{
    adj_r_squared <- NA
  }

  
  cat("\t\n", 
      sprintf("OLS estimation, Dep.Var: %s\n", depvar), 
      sprintf("Estimation Function: %s\n", estim_function), 
      sprintf("Observations:%s\n", N), 
      sprintf("Standard-errors: Clustered  %s\n", ""), 
      sprintf("Number of Clusters:  %s\n", numb_clusters), 
      sprintf("Adj. R-Squared: %s\n", round(adj_r_squared,6)), 
      sprintf("%s\n", "")) 
  tidy(object)
  
  
}


