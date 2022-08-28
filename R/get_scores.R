get_scores <- function(bootstrap_type, 
                       G, 
                       tXgyg, 
                       tXgXg, 
                       tXgX1g, 
                       beta_hat, 
                       beta_tilde, 
                       beta_g_hat, 
                       beta_1g_tilde){
  
  
  #' @noRd

  scores <- 
    switch(
      bootstrap_type, 
      WCR1x = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_tilde, 
      WCU1x = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_hat, #Xg'x u_g
      WCR3x = function(g) tXgyg[[g]] - tXgX1g[[g]] %*% beta_1g_tilde[[g]], 
      WCU3x = function(g) tXgyg[[g]] - tXgXg[[g]] %*% beta_g_hat[[g]],
    )


  scores_list <- lapply(
    1:G, 
    function(g) eval(scores(g))
  )
  
  scores_list
  
}