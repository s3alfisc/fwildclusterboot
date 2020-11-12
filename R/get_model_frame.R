# get_model_frame <- function(mod) {
#   model.frame(formula = mod$call$formula, 
#               data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
#               drop.unused.levels = TRUE)
# }

get_model_frame <- function(mod) {
  # why do I not simply use model.frame? because it fails for
  # objects of class fixest. therefore take from global environment
  
  if(class(mod) %in% c("lm", "lm_robust")){
    res <- model.frame(formula = eval(mod$call$formula, envir =  attr(mod$terms, ".Environment")), 
                data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                drop.unused.levels = TRUE)    
  } else if(class(mod) == "felm"){
    formula <- formula(Formula::Formula(eval(mod$call$formula, envir =  attr(mod$terms, ".Environment"))), lhs = 1, rhs = 1)
    res <- model.frame(formula = formula, 
                data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                drop.unused.levels = TRUE)    
  } else if(class(mod) == "fixest"){
    formula <- formula(Formula::Formula(eval(mod$call$fml, envir =  attr(mod$terms, ".Environment"))), lhs = 1, rhs = 1)
    res <- model.frame(formula = formula, 
                data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                drop.unused.levels = TRUE)    
  } else{
    stop("The boottest method is only defined for objects of class lm, lm_robust, felm and feols.")
  }
  res
}

get_model_fe <- function(mod) {
  if(class(mod) == "lm_robust"){
    res <- model.frame(formula = eval(mod$call$fixed_effects, envir =  attr(mod$terms, ".Environment")), 
                data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                drop.unused.levels = TRUE)    
  } else if(class(mod) == "felm"){
    formula <- formula(Formula::Formula(eval(mod$call$formula, envir =  attr(mod$terms, ".Environment"))), lhs = 0, rhs = 2)
    res <- model.frame(formula = formula, 
                data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                drop.unused.levels = TRUE)    
  } else if(class(mod) == "fixest"){
    formula <- as.formula(paste(" ~ ", paste(mod$fixef_vars, collapse= "+")))
    res <- model.frame(formula = formula, 
                       data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                       drop.unused.levels = TRUE)     
  } else{
    stop("The boottest method is only defined for objects of class lm, lm_robust, felm and feols.")
  }
  res
}


get_model_clusters <- function(mod) {
  if(class(mod) == "lm_robust"){
    res <- model.frame(formula = eval(mod$call$clusters, envir =  attr(mod$terms, ".Environment")), 
                       data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                       drop.unused.levels = TRUE)    
  } else if(class(mod) == "felm"){
    formula <- formula(Formula::Formula(eval(mod$call$formula, envir =  attr(mod$terms, ".Environment"))), lhs = 0, rhs = 2)
    res <- model.frame(formula = formula, 
                       data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                       drop.unused.levels = TRUE)    
  } else if(class(mod) == "fixest"){
    formula <- as.formula(paste(" ~ ", paste(eval(mod$call$fixef), collapse= "+")))
    res <- model.frame(formula = formula, 
                       data = eval(mod$call$data, envir =  attr(mod$terms, ".Environment")),
                       drop.unused.levels = TRUE)     
  } else{
    stop("The boottest method is only defined for objects of class lm, lm_robust, felm and feols.")
  }
  res
}

# get_model_fe(lm_robust_fit)
# 
# res <- demean(cbind(get_model_frame(lm_robust_fit)), get_model_fe(lm_robust_fit)) 
# 
# head(res)
# unique(res$intercept)
# head(
# get_model_frame(cbind(1, lm_robust_fit))
# )
