get_weights <- function(type, 
                        full_enumeration, 
                        N_G_bootcluster, 
                        boot_iter){
  
  
  #' draw bootstrap weights
  #' @param type the type of the weights distribution. Either 'rademacher', 
  #' 'mammen', 'norm' or 'webb
  #' @param full_enumeration Logical. Should (deterministic) full enumeration be
  #' employed
  #' @param N_G_bootcluster Integer. The number of bootstrap clusters
  #' @param boot_iter The number of bootstrap iterations
  #' @return A matrix of dimension N_G_bootcluster x (boot_iter + 1)
  
  
  cat("type:", type, "\n")
  cat("full_enumeration:", full_enumeration, "\n")
  cat("N_G_bootcluster:", N_G_bootcluster, "\n")
  cat("boot_iter:", boot_iter, "\n")
  
  
  wild_draw_fun <- switch(type,
                          # note: for randemacher, create integer matrix
                          # (uses less memory
                          # than numeric)
                          rademacher = function(n) {
                            dqrng::dqsample(
                              x = c(-1L, 1L),
                              size = n,
                              replace = TRUE
                            )
                          },
                          mammen = function(n) {
                            sample(
                              c(-1, 1) * (sqrt(5) + c(-1, 1)) / 2,
                              n,
                              replace = TRUE,
                              prob = (sqrt(5) + c(1, -1)) / (2 * sqrt(5))
                            )
                          },
                          norm = function(n) {
                            dqrng::dqrnorm(n = n)
                          },
                          webb = function(n) {
                            dqrng::dqsample(
                              x = c(-sqrt((3:1) / 2), sqrt((1:3) / 2)),
                              size = n,
                              replace = TRUE
                            )
                          },
                          wild_draw_fun
  )
  
  # do full enumeration for rademacher weights if bootstrap iterations
  # B exceed number of possible permutations else random sampling
  
  if (type %in% c("rademacher") && full_enumeration == TRUE) {
    v0 <-
      gtools::permutations(
        n = 2,
        r = N_G_bootcluster,
        v = c(1, -1),
        repeats.allowed = TRUE
      )
    v <- cbind(1, t(v0))
  } else {
    # else: just draw with replacement - by chance, some permutations
    # might occur more than once
    v <- wild_draw_fun(n = N_G_bootcluster * (boot_iter + 1))
    dim(v) <- c(N_G_bootcluster, boot_iter +1)
    v[, 1] <- 1
  }
  
  v
  
  
}
