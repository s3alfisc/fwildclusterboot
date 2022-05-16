model_matrix <- function(object, ...) {
  
  #' enhanced model.matrix functionalities
  #' @param object An object of class `lm` or `felm``
  #' @param ... Other arguments
  #' @export

  UseMethod("model_matrix")
  
}


model_matrix.lm <- function(object, collin.rm = TRUE, ...){
  
  #' @method model_matrix lm
  #' @export
  #' @param object An object of class lm
  #' @param collin.rm Should collinear variables be dropped?
  #' @param ... Other arguments
  
  X <- model.matrix(object)
  if(collin.rm == TRUE){
    bn <- names(na.omit(coef(object)))
    X[,colnames(X) %in% bn]
  }

  X
  
}

model_matrix.felm <- function(object, type, collin.rm = TRUE, ...){
  
  #' @method model_matrix felm
  #' @export
  #' @param object An object of class felm
  #' @param collin.rm Should collinear variables be dropped?
  #' @param type 'rhs' for right-hand side variables, 'fixef' for fixed effects
  #' @param ... Other arguments
  
  dreamerr::check_arg(type, "charin(rhs, fixef)" )
  
    if(type == "rhs"){
      mm <- model.matrix(object)
      if(collin.rm == TRUE){
        bn <- names(na.omit(coef(object)))
        mm[,colnames(mm) %in% bn]
      }
    } else if(type == "fixef"){
      mm <- as.data.frame(object$fe) 
    } 
    
    mm
  
}



model_matrix.fixest <- function(object, type, collin.rm = TRUE, ...){
  
  #' @method model_matrix fixest
  #' @export
  #' @param object An object of class fixest
  #' @param collin.rm Should collinear variables be dropped?
  #' @param ... Other arguments
  
  dreamerr::check_arg(type, "charin(rhs, fixef)" )
  
  if(type == "rhs"){
    mm <- model.matrix(object, type = "rhs", na.rm = TRUE, collin.rm = collin.rm, as.df = TRUE)
  } else if(type == "fixef"){
    mm <- model.matrix(object, type = type, na.rm = TRUE, collin.rm = collin.rm, as.df = TRUE)
  }
  
  mm
  
}


model_matrix.ivreg <- function(object, collin.rm = TRUE, type){
  
  dreamerr::check_arg(type, "charing(endo, exo)")
  
  if(type == "endo"){
    mm <- model.matrix(object, component = "regressors", na.rm = TRUE)[, object$endogenous, drop = FALSE]
  } else if(type == "exo"){
    mm <- model.matrix(object, component = "instruments", na.rm = TRUE)[, object$exogenous, drop = FALSE]
  } else{
    stop("type needs to be 'endo' or 'exo'.")
  }
  
  mm
  
  

  
}

