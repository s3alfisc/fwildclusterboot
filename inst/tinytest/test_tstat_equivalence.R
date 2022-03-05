# test for equivalence of non-bootstrapped t-stats for
# single-equation (q=1), multi-equation (q>1) and WRE tests
# all against fixest::feols()


library(fixest)
library(ivreg)
library(sandwich)
library(lmtest)
library(data.table)
library(fwildclusterboot)

ols_test <- function(run_this_test){

if (run_this_test) {

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

  for(boot_algo in c("R", "WildBootTests.jl")){
    for(adj in c(TRUE, FALSE)){
      for(cluster.adj in c(TRUE, FALSE)){
        for(cluster.df in c("conventional", "min")){
          for(impose_null in c(TRUE, FALSE)){

            if(boot_algo == "WildBootTests.jl"){
              cluster.adj <- FALSE
              adj <- FALSE
            }
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
                                                impose_null = impose_null,
                                                boot_algo = boot_algo)

            boot2 <- fwildclusterboot::boottest(lm_fit,
                                                clustid = c("group_id1"),
                                                B = B,
                                                param = "log_income",
                                                ssc = boot_ssc(adj = adj,
                                                               cluster.adj = cluster.adj,
                                                               cluster.df = cluster.df) ,
                                                impose_null = impose_null,
                                                boot_algo = boot_algo)
            boot3 <- fwildclusterboot::boottest(lm_fit,
                                                clustid = c("group_id1"),
                                                B = B,
                                                param = "ideology1",
                                                ssc = boot_ssc(adj = adj,
                                                               cluster.adj = cluster.adj,
                                                               cluster.df = cluster.df) ,
                                                impose_null = impose_null,
                                                boot_algo = boot_algo)

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
                                                impose_null = impose_null,
                                                boot_algo = boot_algo)
            boot2 <- fwildclusterboot::boottest(lm_fit,
                                                clustid = c("group_id1", "group_id2"),
                                                B = B,
                                                param = "log_income",
                                                ssc = boot_ssc(adj = adj,
                                                               cluster.adj = cluster.adj,
                                                               cluster.df = cluster.df),
                                                impose_null = impose_null,
                                                boot_algo = boot_algo)
            boot3 <- fwildclusterboot::boottest(lm_fit,
                                                clustid = c("group_id1", "group_id2"),
                                                B = B,
                                                param = "ideology1",
                                                ssc = boot_ssc(adj = adj,
                                                               cluster.adj = cluster.adj,
                                                               cluster.df = cluster.df),
                                                impose_null = impose_null,
                                                boot_algo = boot_algo)

            print(expect_equivalent(abs(boot1$t_stat), abs(dof_tstat[1])))
            print(expect_equivalent(abs(boot2$t_stat), abs(dof_tstat[2])))
            print(expect_equivalent(abs(boot3$t_stat), abs(dof_tstat[3])))

          }
        }
      }
    }



  }

}
}
wald_test <- function(run_this_test){

  if(run_this_test){

    reltol <- 0.002

    N <- 1000
    data1 <<- wildboottestjlr:::create_data(N = 1000,
                                            N_G1 = 20,
                                            icc1 = 0.5,
                                            N_G2 = 20,
                                            icc2 = 0.2,
                                            numb_fe1 = 10,
                                            numb_fe2 = 10,
                                            seed = 90864369,
                                            weights = 1:N / N)


    feols_fit <- fixest::feols(proposition_vote ~ treatment  + log_income + year,
                               data = data1)
    lm_fit <- lm(proposition_vote ~ treatment  + log_income + year,
                 data = data1)

    feols_fit_weights <- fixest::feols(proposition_vote ~ treatment  + log_income + year,
                                       data = data1,
                                       weights = data1$weights)
    lm_fit_weights <- lm(proposition_vote ~ treatment  + log_income + year,
                         data = data1,
                         weights = data1$weights)



    N <- nrow(data1)
    k <- length(coef(lm_fit))
    G <- length(unique(data1$group_id1))



    type <- "rademacher"
    p_val_type <- "two-tailed"
    # impose_null <- TRUE
    wildboottestjlr::set_julia_seed(12345)
    set.seed(12391786)
    dqrng::dqset.seed(8723467)


    # OLS


    # 1) oneway clustering

    # one hypothesis
    R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit))
    boot_jl <- suppressWarnings(waldboottest(floattype = "Float64", lm_fit, R = R, clustid = "group_id1", B = 999))

    #sW <- coeftest(object, vcov = sandwich::vcovCL(object, cluster = ~ group_id1))
    wald_stat <- fixest::wald(feols_fit, "treatment", cluster = ~ group_id1)

    expect_equivalent(boot_jl$t_stat, sqrt(wald_stat$stat))

    # two hypotheses
    R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit))
    boot_jl <- suppressWarnings(
      wildboottestjlr::waldtest(floattype = "Float64",
                                object = lm_fit,
                                R = R,
                                clustid = "group_id1",
                                B = 999,
                                ssc = wildboottestjlr::boot_ssc(adj = TRUE,
                                                                cluster.adj = TRUE))
    )

    wald_stat <- fixest::wald(feols_fit, "Inter|treatment", cluster = ~ group_id1)
    expect_equivalent(boot_jl$t_stat, wald_stat$stat)


    # one hypothesis
    R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit))
    boot_jl <- suppressWarnings(wildboottestjlr::waldtest(floattype = "Float64",
                                                          lm_fit,
                                                          R = R,
                                                          clustid = c("group_id1", "group_id2"),
                                                          B = 999,
                                                          ssc = boot_ssc(cluster.df = "min")))

    #sW <- coeftest(object, vcov = sandwich::vcovCL(object, cluster = ~ group_id1))
    wald_stat <- fixest::wald(feols_fit,
                              "treatment",
                              cluster = ~ group_id1 + group_id2,
                              ssc = ssc(cluster.df = "conventional"))

    expect_equivalent(boot_jl$t_stat, sqrt(wald_stat$stat))

    # two hypotheses
    R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit))
    boot_jl <- suppressWarnings(
      wildboottestjlr::waldtest(floattype = "Float64",
                                object = lm_fit,
                                R = R,
                                clustid = "group_id1",
                                B = 999,
                                ssc = wildboottestjlr::boot_ssc(adj = TRUE,
                                                                cluster.adj = TRUE))
    )

    wald_stat <- fixest::wald(feols_fit, "Inter|treatment",
                              cluster = ~ group_id1,
                              cluster.df = "conventional")
    expect_equivalent(boot_jl$t_stat, wald_stat$stat)



    # WLS


    # 1) oneway clustering

    # one hypothesis
    R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit_weights))
    boot_jl <- suppressWarnings(wildboottestjlr::waldtest(floattype = "Float64", lm_fit_weights, R = R, clustid = "group_id1", B = 999))

    #sW <- coeftest(object, vcov = sandwich::vcovCL(object, cluster = ~ group_id1))
    wald_stat <- fixest::wald(feols_fit_weights, "treatment", cluster = ~ group_id1)

    expect_equivalent(boot_jl$t_stat, sqrt(wald_stat$stat))

    # two hypotheses
    R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit_weights))
    boot_jl <- suppressWarnings(
      wildboottestjlr::waldtest(floattype = "Float64",
                                object = lm_fit_weights,
                                R = R,
                                clustid = "group_id1",
                                B = 999,
                                ssc = wildboottestjlr::boot_ssc(adj = TRUE,
                                                                cluster.adj = TRUE))
    )

    wald_stat <- fixest::wald(feols_fit_weights, "Inter|treatment", cluster = ~ group_id1)
    expect_equivalent(boot_jl$t_stat, wald_stat$stat)


    # one hypothesis
    R <- clubSandwich::constrain_zero(constraints = 2, coefs = coef(lm_fit_weights))
    boot_jl <- suppressWarnings(wildboottestjlr::waldtest(floattype = "Float64",
                                                          lm_fit_weights,
                                                          R = R,
                                                          clustid = c("group_id1", "group_id2"),
                                                          B = 999,
                                                          type = type,
                                                          p_val_type = p_val_type,

                                                          ssc = boot_ssc(cluster.df = "min")))

    #sW <- coeftest(object, vcov = sandwich::vcovCL(object, cluster = ~ group_id1))
    wald_stat <- fixest::wald(feols_fit_weights,
                              "treatment",
                              cluster = ~ group_id1 + group_id2,
                              ssc = ssc(cluster.df = "conventional"))

    expect_equivalent(boot_jl$t_stat, sqrt(wald_stat$stat))

    # two hypotheses
    R <- clubSandwich::constrain_zero(constraints = 1:2, coefs = coef(lm_fit_weights))
    boot_jl <- suppressWarnings(
      wildboottestjlr::waldtest(floattype = "Float64",
                                object = lm_fit_weights,
                                R = R,
                                clustid = "group_id1",
                                B = 999,
                                ssc = boot_ssc(cluster.adj = TRUE))
    )

    wald_stat <- fixest::wald(feols_fit_weights, "Inter|treatment",
                              cluster = ~ group_id1,
                              cluster.df = "conventional")
    expect_equivalent(boot_jl$t_stat, wald_stat$stat)



  }

}

iv_test <- function(run_this_test){

  # Note: Test with Float64 for exact match
  if(run_this_test){
    
    library(data.table)
    fwildclusterboot::set_julia_seed(123)
    
    data("SchoolingReturns", package = "ivreg")
    
    # drop all NA values from SchoolingReturns
    data1 <<- SchoolingReturns[rowMeans(sapply(SchoolingReturns, is.na)) == 0,]
    ivreg_fit <- ivreg(log(wage) ~ education + age + ethnicity + smsa + south + parents14 |
                         nearcollege + age  + ethnicity + smsa + south + parents14,
                       data = data1)
    vcov1 <- sandwich::vcovCL(ivreg_fit,
                              cluster = ~ kww,
                              cadjust = TRUE,
                              type = "HC1")
    vcov2 <- sandwich::vcovCL(ivreg_fit,
                              cluster = ~ smsa + kww,
                              cadjust = TRUE,
                              type = "HC1")
    
    res1 <- coeftest(ivreg_fit, vcov1)
    res_df1 <- broom::tidy(res1)
    
    res2 <- coeftest(ivreg_fit, vcov2)
    res_df2 <- broom::tidy(res2)
    
    boot_ivreg1 <- boottest(floattype = "Float64",
                            object = ivreg_fit,
                            B = 999,
                            param = "education",
                            clustid = "kww",
                            type = "mammen",
                            impose_null = TRUE)
    
    
    expect_equivalent(boot_ivreg1$t_stat, res_df1[res_df1$term == "education",'statistic'])
    
    
    # two-way clustering currently fails
    # boot_ivreg2 <- boottest(floattype = "Float64",
    #                         object = ivreg_fit,
    #                         B = 999,
    #                         param = "education",
    #                         clustid = c("kww", "age"),
    #                         type = "rademacher")
    # 
    # 
    # expect_equivalent(boot_ivreg2$t_stat, res_df1[res_df2$term == "education",'statistic'])
    
  }
}

ols_test(run_this_test = TRUE)
iv_test(run_this_test = TRUE)
wald_test(run_this_test = TRUE)
