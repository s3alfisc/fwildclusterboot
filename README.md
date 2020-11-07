
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwildclusterboot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
<!-- badges: end -->

The `fwildclusterboot` package is an R port to Stata’s `boottest`
package.

It implements the fast wild cluster bootstrap algorithm developed in
Roodman et al for regression objects in R. It currently works for
regression objects of type `lm`, `felm` and `fixest` from the base R and
the `lfe` and `fixest` packages.

The package’s central function is `boottest()`. It allows the user to
test two-sided, univariate hypotheses using a wild cluster bootstrap.
Importantly, it uses the “fast” algorithm developed in Roodman et al,
which makes it feasible to calculate test statistics based on a large
number of bootstrap draws even for large samples.

The `fwildclusterboot` package currently only supports one-dimensional
clustering and one-dimensional hypotheses, but allows for an arbitrary
number of fixed effects.

The package is highly experimental and currently does not include any
unit tests.

## Installation

You can install the released version of `fwildclusterboot` from github
by running

``` r
library(devtools)
#> Loading required package: usethis
#install_github("al_fisc/fwildclusterboot")
```

## The boottest function

In a first step, simulate a data set with 10000 individual observations
that are grouped in 20 clusters and with a small intra-cluster
correlation of 0.01. The small intra-cluster correlation implies that,
in theory, inference based on cluster-robust covariance estimates should
lead to results that are very similar to the bootstrap.

``` r
library(lfe)
library(fixest)

library(fwildclusterboot)
B <- 1000
seed <- 421
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 20, icc = 0.01)
head(voters)
#>       ID group_id    ideology ideological_label     income       Q1_immigration
#> 1: 00001        1 -1.81640013 Very Conservative 5552077.83             Disagree
#> 2: 00002        2  1.73220900      Very Liberal   57090.33                Agree
#> 3: 00003        3  1.09039484      Very Liberal   50366.35           Lean Agree
#> 4: 00004        4 -0.04439857      Conservative  123046.47 Don't Know / Neutral
#> 5: 00005        5  0.04503661           Liberal   22861.60 Don't Know / Neutral
#> 6: 00006        6 -1.44487347 Very Conservative  685074.18        Lean Disagree
#>    treatment proposition_vote log_income
#> 1:         0                0   15.52968
#> 2:         1                1   10.95239
#> 3:         0                1   10.82708
#> 4:         1                1   11.72032
#> 5:         0                0   10.03721
#> 6:         1                0   13.43728
```

The `fwildclusterboot` package supports estimation of linear models
based on - `lm()` from `base` R - `felm()` from `lfe` - `feols()` from
`fixest`

``` r
# 1) boottest based on object of class lm
lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration , weights = NULL, data = voters)

# 2) boottest based on object of class fixest
feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
feols_fit1 <- feols(proposition_vote ~ treatment + ideology + log_income + Q1_immigration, weights = NULL, data = voters)
feols_fit2 <- feols(proposition_vote ~ treatment + ideology + log_income + as.factor(Q1_immigration), weights = NULL, data = voters)

# 3) bootest based on object of class felm
felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration, weights = NULL, data = voters)
felm_fit1 <- felm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration, weights = NULL, data = voters)
```

The `boottest` command offers two options First, it always calculates
p-values for a given univariate hypothesis test. Second and by default,
the boottest function calculates confidence intervals by inversion of
the p-value. The user can considerably speed up the inference procedure
by setting the argument `conf_int = FALSE`, in which case no confidence
intervals are computed.

``` r
# 1) boottest based on object of class lm
boot_lm = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)

# 2) bootest based on object of class feols
boot_fixest = boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
boot_fixest1 = boottest(feols_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0)
# boot_fixest2 = boottest(feols_fit2, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0)

# 3) boottest based on object of class felm
boot_felm = boottest(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
boot_felm1 = boottest(felm_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
```

A `summary` method collects the results.

``` r
summary(boot_lm)
#>  
#>  OLS estimation, Dep.Var: proposition_vote
#>  Estimation Function: lm
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.017   1.672     0.13   -0.037    0.005
summary(boot_felm)
#>  
#>   Estimation Function: felm
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1   -0.017   1.672     0.13   -0.037    0.005
summary(boot_felm1)
#>  
#>   Estimation Function: felm
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1   -0.017   1.672     0.13   -0.037    0.005
summary(boot_fixest)
#>  
#>   Estimation Function: fixest
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.017   1.672     0.13   -0.037    0.005
summary(boot_fixest1)
#>  
#>   Estimation Function: fixest
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.017   1.672     0.13   -0.037    0.005
```

These estimates are very close to estimates using sandwich cluster
robust estimators:

``` r
fixest:::summary.fixest(feols_fit, se = "cluster", cluster = "group_id")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id) 
#>             Estimate Std. Error   t value    Pr(>|t|)    
#> treatment  -0.016559   0.010164 -1.629200 1.19731e-01    
#> ideology    0.278681   0.016264 17.135000 5.19000e-13 ***
#> log_income  0.002676   0.002873  0.931516 3.63273e-01    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -5,276.98   Adj. R2: 0.32637 
#>                           R2-Within: 0.03462
```

Boottest further comes with a `tidy` method which, in analogy with the
`broom` package, returns the estimation results as a data.frame.

``` r
tidy(boot_lm)
#>              Estimate  t value Pr(>|t|)    CI Lower    CI Upper
#> treatment -0.01655949 1.672313     0.13 -0.03691685 0.004929245
tidy(boot_felm)
#>      Estimate  t value Pr(>|t|)   CI Lower    CI Upper
#> 1 -0.01655949 1.672313     0.13 -0.0369068 0.004969819
tidy(boot_fixest)
#>              Estimate  t value Pr(>|t|)   CI Lower    CI Upper
#> treatment -0.01655949 1.672313     0.13 -0.0369068 0.004969819
```

## Comparison to cluster.boot()

``` r
library(multiwayvcov)
library(lmtest)
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> 
#> Attaching package: 'lmtest'
#> The following object is masked from 'package:lfe':
#> 
#>     waldtest
res <- cluster.boot(lm_fit, cluster = voters$group_id, parallel = TRUE, R = 10000, wild_type = "rademacher")
coeftest(lm_fit, res)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)                         0.7471023  0.0555905  13.4394 < 2.2e-16 ***
#> treatment                          -0.0165595  0.0099104  -1.6709 0.0947696 .  
#> ideology                            0.2786811  0.0158264  17.6086 < 2.2e-16 ***
#> log_income                          0.0026758  0.0028196   0.9490 0.3426474    
#> Q1_immigrationDisagree             -0.2118899  0.0151017 -14.0309 < 2.2e-16 ***
#> Q1_immigrationLean Disagree        -0.3295491  0.0290362 -11.3496 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral -0.2670825  0.0407878  -6.5481 6.114e-11 ***
#> Q1_immigrationLean Agree           -0.2078264  0.0560511  -3.7078 0.0002102 ***
#> Q1_immigrationAgree                -0.3167597  0.0694429  -4.5614 5.141e-06 ***
#> Q1_immigrationStrong Agree         -0.5503678  0.0868162  -6.3395 2.405e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
summary(boottest(lm_fit, B = 20000, clustid = voters$group_id, param = "treatment"))
#>  
#>  OLS estimation, Dep.Var: proposition_vote
#>  Estimation Function: lm
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.017   1.672    0.118   -0.037    0.005
fixest:::summary.fixest(feols_fit, se = "cluster", cluster = "group_id")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id) 
#>             Estimate Std. Error   t value    Pr(>|t|)    
#> treatment  -0.016559   0.010164 -1.629200 1.19731e-01    
#> ideology    0.278681   0.016264 17.135000 5.19000e-13 ***
#> log_income  0.002676   0.002873  0.931516 3.63273e-01    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -5,276.98   Adj. R2: 0.32637 
#>                           R2-Within: 0.03462
```
