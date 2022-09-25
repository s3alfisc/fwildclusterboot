run_simulation <- function(k, G, gamma, alpha = 0.05){
  
  library(fwildclusterboot)
  library(fabricatr)
  library(parallel)
  # gamma between 0 and 4
  
  # k <- 10
  # G <- 24
  # gamma <- 2
  # alpha <- 0.05
  
  N <- 400*G
  N_g_denom <- sum(exp(1:G * gamma / G))
  N_g <- c()
  for(g in 1:(G-1)){
    N_g[g] <- floor(N * exp(gamma * g / G) / N_g_denom)
  }
  N_g <- c(N_g, N - sum(N_g))
  cluster <- c()
  for(x in seq_along(N_g)){
    cluster <- c(cluster, rep(x, N_g[x]))
  }
  cluster <- factor(cluster)

  random_effect <- rnorm(G, 0, 1)
  random_effect_long <- random_effect[cluster]
  
  u <- rnorm(N)
  #Y <- random_effect_long + u
  icc <- var(random_effect_long) / (var(u) + var(random_effect_long))
  
  beta <- rnorm(k-1)
  X <- MASS::mvrnorm(n = N, mu = rep(0,k-1), Sigma = diag(k-1))
  X_k <- rnorm(N, 0, 1)
  X_all <- cbind(X, X_k)
  
  y <- 0.1 + X_all %*% c(beta, 0) + random_effect_long + u
  
  df <- data.frame(y = y, cluster = cluster, X = X_all)
  names(df) <- c("y", "cluster",paste0("X", 1:k))
  fml <- update(y~., reformulate(paste0("X", 1:k, collapse = "+")  ))
  fit <- lm(formula = fml, data = df)
  
  bootstrap_types <- c("11", "13", "31", "33")
  reject <- rep(NA, 11)
  names(reject) <- c(
    paste0("WCR", bootstrap_types), 
    paste0("WCU", bootstrap_types), 
    "CRV1", "CRV3", "CRV3J"
  )

  counter <- 1
  for(y in c(TRUE, FALSE)){
    for(x in seq_along(bootstrap_types)){
      res <- 
      boottest(
        fit, 
        param = paste0("X", k),
        B = 399, 
        clustid = "cluster", 
        impose_null = y, 
        bootstrap_type = bootstrap_types[x], 
        conf_int=FALSE
      )
      reject[counter] <- pval(res) < alpha
       counter <- counter+1
    }
  }
  
  crv1 <- sandwich::vcovCL(fit, ~cluster, cadjust = TRUE)
  crv3 <- summclust::vcov_CR3J(fit, ~cluster, type = "CRV3")
  crv3J <- summclust::vcov_CR3J(fit, ~cluster, type = "CRV3J")
  
  cftbl_1 <- broom::tidy(
    lmtest::coeftest(fit, crv1, df = G - 1))[k+1, "p.value"]
  cftbl_3 <- broom::tidy(
    lmtest::coeftest(fit, crv3, df = G - 1))[k+1, "p.value"]
  cftbl_3J <- broom::tidy(
    lmtest::coeftest(fit, crv3J, df = G - 1))[k+1, "p.value"]
  
  reject["CRV1"] <- cftbl_1 < alpha
  reject["CRV3"] <- cftbl_3 < alpha
  reject["CRV3J"] <- cftbl_3J < alpha
  
  
  reject
  
}


get_figure2_mnw <- function(n_sims, gamma_values = seq(0, 4, 0.5), nthreads = 1){
  
  gamma_values <- gamma_values
  n_sims <- n_sims
  
  clust <- parallel::makeCluster(nthreads)
  parallel::clusterExport(clust, list("gamma_values","n_sims"))
  parallel::clusterExport(clust, list("run_simulation"))
  
  parallel::parLapply(
    clust,
    seq_along(gamma_values), 
      function(y){
        res <- matrix(NA, n_sims, 11)
        for(x in 1:n_sims){
          res[x, ] <- run_simulation(k = 10, G = 24, gamma = gamma_values[y])
        }
        colMeans(res)  
      }
  )
  
}

#set.seed(12312312)
#get_figure2_mnw(n_sims= 1000, gamma_values = seq(0, 4, 0.5), nthreads = 4)
