# test if reported bootstrap t-statistic is equal to fixest t-statistic with 
# dof = dof(adj = FALSE, cluster.adj = TRUE) -> this is the type of "small 
# sample adjustment" employed by fwildclusterboot::boottest
# see fixest vignette: https://cran.r-project.org/web/packages/fixest/vignettes/standard_errors.html

runThisTest <- TRUE

if (runThisTest) {

library(fixest)
library(fwildclusterboot)  

#data(voters)
#adj <- cluster.adj <- TRUE; cluster.df <- "conventional"; impose_null = TRUE


lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income ,
   data = fwildclusterboot:::create_data(N = 1000, 
                                         N_G1 = 20, 
                                         icc1 = 0.81,
                                         N_G2 = 10,
                                         icc2 = 0.01, 
                                         numb_fe1 = 10,
                                         numb_fe2 = 10, 
                                         seed = 97069))

B <- 999

# adj= FALSE 
# cluster.adj= FALSE 
# cluster.df= "conventional" 
# impose_null= TRUE 

for(adj in c(TRUE, FALSE)){
  for(cluster.adj in c(TRUE, FALSE)){
    for(cluster.df in c("conventional", "min")){
      for(impose_null in c(TRUE, FALSE)){
        
        cat("--------------------------------", "\n")
        
        cat("adj:", adj, "\n")
        cat("cluster.adj:", cluster.adj, "\n")
        cat("cluster.df:", cluster.df, "\n")
        #cat("impose_null:", impose_null, "\n")
        
        cat("--------------------------------", "\n")
        cat("oneway:", "\n")
        
        # oneway clustering 
        feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income,
                                   data = fwildclusterboot:::create_data(N = 1000, 
                                                                         N_G1 = 20, 
                                                                         icc1 = 0.81,
                                                                         N_G2 = 10,
                                                                         icc2 = 0.01, 
                                                                         numb_fe1 = 10,
                                                                         numb_fe2 = 10, 
                                                                         seed = 97069), 
                                    cluster = ~group_id1, 
                                    ssc = ssc(adj = adj, 
                                              cluster.adj = cluster.adj, 
                                              cluster.df = cluster.df))
        
        dof_tstat <- fixest::coeftable(feols_fit)[c("treatment", "log_income", "ideology1"), 3]
        
  
        boot1 <- fwildclusterboot::boottest(lm_fit, 
                                            clustid = c("group_id1"),
                                            B = B, 
                                            param = "treatment", 
                                            ssc = boot_ssc(adj = adj, 
                                                          cluster.adj = cluster.adj, 
                                                          cluster.df = cluster.df),  
                                            impose_null = impose_null)
                                                                                                                                                                                                    
        boot2 <- fwildclusterboot::boottest(lm_fit, clustid = c("group_id1"), B = B, param = "log_income", ssc = boot_ssc(adj = adj, 
                                                                                                                                 cluster.adj = cluster.adj, 
                                                                                                                                 cluster.df = cluster.df) ,
                                            impose_null = impose_null)                                                                          
        boot3 <- fwildclusterboot::boottest(lm_fit, clustid = c("group_id1"), B = B, param = "ideology1", ssc = boot_ssc(adj = adj, 
                                                                                                                                cluster.adj = cluster.adj, 
                                                                                                                                cluster.df = cluster.df) ,
                                            impose_null = impose_null)                               
                                                                                                                                                                                                           
        print(expect_equivalent(abs(boot1$t_stat), abs(dof_tstat[1])))
        print(expect_equivalent(abs(boot2$t_stat), abs(dof_tstat[2])))
        print(expect_equivalent(abs(boot3$t_stat), abs(dof_tstat[3])))
        
        
        cat("--------------------------------", "\n")
        cat("twoway:", "\n")
        cat("--------------------------------", "\n")
        
        # twoway clustering
        feols_fit <- fixest::feols(proposition_vote ~ treatment + ideology1 + log_income ,
                                   data = fwildclusterboot:::create_data(N = 1000, 
                                                                         N_G1 = 20, 
                                                                         icc1 = 0.81,
                                                                         N_G2 = 10,
                                                                         icc2 = 0.01, 
                                                                         numb_fe1 = 10,
                                                                         numb_fe2 = 10, 
                                                                         seed = 97069),
                                   cluster = ~group_id1 + group_id2,
                                   ssc = ssc(adj = adj,
                                             #fixef.K = "full",
                                             cluster.adj = cluster.adj,
                                             cluster.df = cluster.df))
  
        
        dof_tstat <- fixest::coeftable(feols_fit)[c("treatment", "log_income", "ideology1"), 3]
  
  
        boot1 <- fwildclusterboot::boottest(lm_fit, 
                                            clustid = c("group_id1", "group_id2"), 
                                            B = B, 
                                            param = "treatment", 
                                            ssc = boot_ssc(adj = adj,
                                                           cluster.adj = cluster.adj,
                                                           cluster.df = cluster.df), 
                                            impose_null = impose_null)
        boot2 <- fwildclusterboot::boottest(lm_fit, 
                                            clustid = c("group_id1", "group_id2"),
                                            B = B, 
                                            param = "log_income",
                                            ssc = boot_ssc(adj = adj,
                                                           cluster.adj = cluster.adj,
                                                           cluster.df = cluster.df),
                                            impose_null = impose_null) 
        boot3 <- fwildclusterboot::boottest(lm_fit, 
                                            clustid = c("group_id1", "group_id2"), 
                                            B = B, 
                                            param = "ideology1", 
                                            ssc = boot_ssc(adj = adj,
                                                           cluster.adj = cluster.adj,
                                                           cluster.df = cluster.df), 
                                            impose_null = impose_null)
        
        print(expect_equivalent(abs(boot1$t_stat), abs(dof_tstat[1])))
        print(expect_equivalent(abs(boot2$t_stat), abs(dof_tstat[2])))
        print(expect_equivalent(abs(boot3$t_stat), abs(dof_tstat[3])))

      }
    }
  }
}

}
