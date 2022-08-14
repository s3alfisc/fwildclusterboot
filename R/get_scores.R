get_scores <- function(bootstrap_type, 
                       G, 
                       tXgyg, 
                       tXgXg, 
                       tXgX1g, 
                       beta_hat, 
                       beta_tilde, 
                       beta_g_hat, 
                       beta_1g_tilde){
  
  # scores_template <- function(g, a, b, beta){
  #   a[[g]] - b[[g]] %*% beta
  # }
  
  
  score_num <- 
    switch(
      bootstrap_type, 
      WCR11 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_tilde, 
      WCR13 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_tilde, 
      WCR31 = function(g) tXgyg[[g]] - tXgX1g[[g]] %*% beta_1g_tilde[[g]], 
      WCR33 = function(g) tXgyg[[g]] - tXgX1g[[g]] %*% beta_1g_tilde[[g]], 
      WCU11 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat, 
      WCU13 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat, 
      WCU31 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_g_hat[[g]],
      WCU33 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_g_hat[[g]]
    )
  
  score_denom <- 
    switch(
      bootstrap_type, 
      WCR11 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_tilde, 
      WCR13 = function(g) tXgyg[[g]] - tXgX1g[[g]] %*% beta_1g_tilde[[g]], 
      WCR31 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_tilde,
      WCR33 = function(g) tXgyg[[g]] - tXgX1g[[g]] %*% beta_1g_tilde[[g]], 
      WCU11 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat, 
      WCU13 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_g_hat[[g]],
      WCU31 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat, 
      WCU33 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_g_hat[[g]]
    )
  
  score_num <- lapply(
    1:G, 
    function(g) eval(score_num(g))
  )
  
  score_denom <- lapply(
    1:G, 
    function(g) eval(score_denom(g))
  )
  
  
  res <- list(
    score_num = score_num, 
    score_denom = score_denom
  )
  
  res
  
}