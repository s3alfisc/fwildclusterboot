get_scores <- function(bootstrap_type, 
                       G, 
                       tXgyg, 
                       tXgXg, 
                       tXgX1g, 
                       beta_hat, 
                       beta_tilde, 
                       beta_g_hat, 
                       beta_1g_tilde){
  

  bootstrap_type <- paste0(substr(bootstrap_type, 1, 4), "x")
  
  scores <- 
    switch(
      bootstrap_type, 
      WCR1x = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_tilde, 
      # WCR13 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_tilde, 
      WCU1x = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat, #Xg'x u_g
      # WCU13 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat, 
      WCR3x = function(g) tXgyg[[g]] - tXgX1g[[g]] %*% beta_1g_tilde[[g]], 
      # WCR33 = function(g) tXgyg[[g]] - tXgX1g[[g]] %*% beta_1g_tilde[[g]], 
      WCU3x = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_g_hat[[g]],
      # WCU33 = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_g_hat[[g]]
    )
  

  scores_list <- lapply(
    1:G, 
    function(g) eval(scores(g))
  )

  
  scores_list
  
}