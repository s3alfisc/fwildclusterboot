# this code compares wild cluster bootstrap inference 
# from fwildclusterboot with the clusterSEs package for 
# one-way clustering. Note that clusterSEs does not impose the 
# null, so boottest() will run the WCU. Further, small sample
# corrections in both packages are N / (N-1)


# if (length(strsplit(packageDescription("fwildclusterboot")$Version, "\\.")[[1]]) > 3) { 
#   #Sys.setenv("RunAllfwildclusterbootTests"="yes")
#   runThisTest <- TRUE
# } else {
#   runThisTest <- FALSE
# }

# always set to false - test takes a super long time

runThisTest <- FALSE

if (runThisTest) {
 
  
  data(EmplUK)
  EmplUK$firm <- as.factor(EmplUK$firm)
  # fit
  lm_fit <- lm(emp ~ wage + capital + output + as.factor(firm) + as.factor(year), data = EmplUK)
  plm_fit <- plm::plm(emp ~ wage + capital + output, index = c("firm", "year"), data = EmplUK, effect = "twoways", model = "within")
  
  boot_iter <- 29999
  n_mc <- 10
  
  no_cores <- 5
  clust <- parallel::makeCluster(no_cores)
  parallel::setDefaultCluster(cl= clust)
  parallel::clusterExport(clust, c("boot_iter",
                                   "n_mc", 
                                   "lm_fit", 
                                   "plm_fit", 
                                   "EmplUK"))
  parallel::clusterEvalQ(clust, c("fwildclusterboot", 
                                  #"sandwich", 
                                  "clusterSEs", 
                                  "lmtest"))
  
  res_all <- 
    parallel::parLapply(clust, boot_iter, function(x){
      res_inner <- 
        lapply(1:n_mc, function(y){
          
          p_vals <- vector(mode = "numeric", length = 6L)
          ses <- matrix(NA, 6, 2)
          
          boot1 <- fwildclusterboot::boottest(lm_fit, clustid = c("firm"), B = x, param = "wage", nthreads = 1, seed = y, impose_null = FALSE)
          boot2 <- fwildclusterboot::boottest(lm_fit, clustid = c("firm"), B = x, param = "capital", nthreads = 1, seed = y, impose_null = FALSE)
          boot3 <- fwildclusterboot::boottest(lm_fit, clustid = c("firm"), B = x, param = "output", nthreads = 1, seed = y, impose_null = FALSE)
          
          p_vals[1] <- boot1$p_val
          p_vals[2] <- boot2$p_val
          p_vals[3] <- boot3$p_val
          
          ses[1,] <- boot1$conf_int
          ses[2,] <- boot2$conf_int
          ses[3,] <- boot3$conf_int
          
    
          cSEs <- clusterSEs::cluster.wild.plm(plm_fit, dat = EmplUK, cluster = "group", ci.level = 0.95, boot.reps = x, seed = y)
          p_vals[4:6] <- cSEs$p.values[c("wage", "capital", "output"),]
          ses[4:6, ] <- cSEs$ci[c("wage", "capital", "output"),]
          
          res <- list(p_vals = p_vals, 
                      ses = ses)
          
          res
        })
      
      p_vals <- lapply(1:n_mc, function(x) res_inner[[x]][["p_vals"]])
      p_vals <- Reduce("+",p_vals) / n_mc
      
      ses <- lapply(1:n_mc, function(x) res_inner[[x]][["ses"]])
      ses <- Reduce("+",ses) / n_mc
      
      #res <- matrix(p_vals, ses)
      res <- list(p_vals = p_vals, ses = ses)
      res
      
    })
  
  parallel::stopCluster(clust)
  
  p_vals <- res_all[[1]][["p_vals"]]
  ses <- res_all[[1]][["ses"]]
  
  # relative tolerance 2.5%
  expect_equal(p_vals[1], p_vals[4], 0.01)
  expect_equal(p_vals[2], p_vals[5], 0.01)
  expect_equal(p_vals[3], p_vals[6], 0.01)
  
  # note: expect_equal tests for relative 
  expect_equal(ses[1,1], ses[4,1], 0.01)
  expect_equal(ses[1,2], ses[4,2], 0.02)  # 4% tolerance
  expect_equal(ses[2,], ses[5,], 0.01)
  expect_equal(ses[3,], ses[6,], 0.01)  
}
