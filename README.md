
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwildclusterboot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
<!-- badges: end -->

The goal of fwildclusterboot is to estimate a fast wild cluster
bootstrap for linear regression models of classes “lm”, “lm\_robust” and
“felm” (from packages base, estimatr and lfe).

## Installation

You can install the released version of fwildclusterboot from github…

In a first step, simulate some data

``` r


# library(fwildclusterboot)
B <- 10000
seed <- 1345671
set.seed(seed)

voters <- create_data(N = 2000, N_G = 20)
head(voters)
#>      ID group_id    ideology ideological_label      income       Q1_immigration
#> 1: 0001        1 -0.05943174      Conservative   16208.622 Don't Know / Neutral
#> 2: 0002        2  2.11075601      Very Liberal   27050.976                Agree
#> 3: 0003        3 -0.57228225      Conservative  287686.820        Lean Disagree
#> 4: 0004        4 -0.98017940      Conservative 2943631.268        Lean Disagree
#> 5: 0005        5  1.42551925      Very Liberal    2557.386           Lean Agree
#> 6: 0006        6 -1.10655250 Very Conservative  631037.107        Lean Disagree
#>              Q2_defence treatment proposition_vote log_income
#> 1: Don't Know / Neutral         1                1   9.693299
#> 2:         Strong Agree         0                1  10.205478
#> 3: Don't Know / Neutral         0                0  12.569628
#> 4: Don't Know / Neutral         1                0  14.895155
#> 5:                Agree         0                0   7.846741
#> 6:        Lean Disagree         1                0  13.355120
```

The fwildclusterboot package supports estimation of linear models based
on base R’s lm() function, estimatr’s lm\_robust function, lfe’s felm()
function and fixest’s feols() function.

``` r
library(estimatr)
library(lfe)
#> Loading required package: Matrix
#> 
#> Attaching package: 'Matrix'
#> The following objects are masked from 'package:pracma':
#> 
#>     expm, lu, tril, triu
#> 
#> Attaching package: 'lfe'
#> The following object is masked from 'package:lmtest':
#> 
#>     waldtest
library(fixest)

lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income +Q1_immigration + Q2_defence, weights = NULL, data = voters)
lm_robust_fit <- lm_robust(proposition_vote ~ treatment + ideology + log_income, fixed_effects = ~ Q1_immigration + Q2_defence, weights = NULL, data = voters)
lm_robust_fit1 <- lm_robust(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, weights = NULL, data = voters )
feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income, fixef = c("Q1_immigration", "Q2_defence"), weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration + Q2_defence, weights = NULL, data = voters)
```

The boottest command offers two functions. First, it calculates p-values
for a given null hypothesis of the form HO: \(\beta = 0\) vs H1:
\(\beta \neq 1\). In order to work, an object from a regression model of
class lm, lm\_robust, felm or feols needs to be passed to the boottest
function. Currently, the user is still required to pass a vector with
information on clusters to the function. As of now, the function only
supports one-dimensional clustering. By default, the boottest function
calculates confidence intervals by inversion. The user can considerably
speed up the inference procedure by setting the argument conf\_int to
FALSE, in which case no confidence intervals are
computed.

``` r
lm = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
estimatr_fe = boottest(lm_robust_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
estimatr = boottest(lm_robust_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
felm = boottest(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
fixest = boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
```

Secondly, the user may specify to obtain confidence
intervals.

``` r
res_lm = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
#res_estimatr_fe = boottest(lm_robust_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = #TRUE)
#res_estimatr = boottest(lm_robust_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = #TRUE)
res_felm = boottest(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
res_fixest = boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
```

A summary method collects the results.

``` r
summary(res_lm)
#>  
#>  OLS estimation, Dep.Var: proposition_vote
#>  Estimation Function: lm
#>  Observations:2000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#>  Adj. R-Squared: 0.350656
#>  
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment -0.01471   1.161   0.2709 -0.04193   0.0128
#summary(res_estimatr_fe)
#summary(res_estimatr)
summary(res_felm)
#>  
#>   Estimation Function: felm
#>  Observations:2000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#>  Adj. R-Squared: 0.350656
#>  
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment -0.01471   1.161   0.2716 -0.04204  0.01279
summary(res_fixest)
#>  
#>   Estimation Function: fixest
#>  Observations:2000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#>  Adj. R-Squared: NA
#>  
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment -0.01471   1.161   0.2716 -0.04204  0.01279
```

Furthermore, boottest comes with a tidy method which, in analogy with
the broom-package, returns the estimation results as a data.frame.

``` r
tidy(res_lm)
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment -0.01471   1.161   0.2709 -0.04193   0.0128
#summary(res_estimatr_fe)
#summary(res_estimatr)
tidy(res_felm)
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment -0.01471   1.161   0.2716 -0.04204  0.01279
tidy(res_fixest)
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment -0.01471   1.161   0.2716 -0.04204  0.01279
```

## Benchmarks

``` r

# seed <- 1
# set.seed(seed)
# N <- 5000
# N_G <- 10
# B <- 1000
# 
# data <- create_data(N = N, N_G = N_G)
# 
# lapply(c(100, 1000, 5000, 1000), function(B){
#       lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income +Q1_immigration + Q2_defence, weights = NULL, data = data)
#       bench <- benchmark(
#           boot =  multiwayvcov::cluster.boot(lm_fit, 
#                                              as.factor(data$group_id), 
#                                              R = B, 
#                                              boot_type = "residual", 
#                                              wild_type = "rademacher", 
#                                              parallel = FALSE), 
#           fast_boot_1 = boottest(lm_fit, clustid = data$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE),
#           fast_boot_1_2 = boottest(lm_fit, clustid = data$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE), 
#           replications = 10)
#       c(N, N_G, bench$elapsed)
# })
# 
# 
# res <- data.frame()
# 
# data <- create_data(N = N, N_G = 20)
# i <- 0
# 
# res <- 
# lapply(c(1000, 5000, 10000, 20000), function(N){
#   lapply(c(10, 20, 50, 100, 200), function(N_G){
#     data <- create_data(N = N, N_G = N_G)
#     lapply(c(100, 1000, 5000, 1000), function(B){
#       lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income +Q1_immigration + Q2_defence, weights = NULL, data = data)
#       bench <- benchmark(
#         boot =  multiwayvcov::cluster.boot(lm_fit, 
#                                                        as.factor(data$group_id), 
#                                                        R = B, 
#                                                        boot_type = "residual", 
#                                                        wild_type = "rademacher", 
#                                                        parallel = FALSE), 
#         fast_boot_1 = boottest(lm_fit, clustid = data$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE),
#         fast_boot_1_2 = boottest(lm_fit, clustid = data$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE), 
#         replications = 10, 
#         columns = c("bootstrap", "fast bootstrap 1", "fast bootstrap 2")#,
#         #relative = c("bootstrap")
#       )
#       c(N, N_G, bench$elapsed)
#     })
#   })
# })
```
