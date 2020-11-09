
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
Roodman et al (2019) for regression objects in R. It currently works for
regression objects of type `lm`, `felm` and `fixest` from base R and the
`lfe` and `fixest` packages.

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

The following will be added in the future: - support for clustering of
higher dimensions - support for multivariate hypotheses - bootstrap
distributions other then the rademacher distribution

## Installation

You can install the released version of `fwildclusterboot` from github
by running

``` r
library(devtools)
#> Loading required package: usethis
#install_github("al_fisc/fwildclusterboot")
```

## The `boottest()` function

In a first step, simulate a data set with 10000 individual observations
that are grouped in 50 clusters and with a small intra-cluster
correlation of 0.01. The small intra-cluster correlation implies that,
in theory, inference based on cluster-robust covariance estimates should
lead to results that are very similar to the bootstrap.

``` r
library(fwildclusterboot)


B <- 1000
seed <- 42180019
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 50, icc = 0.01)
head(voters)
#>       ID group_id   ideology ideological_label      income       Q1_immigration
#> 1: 00001        1  0.5847759           Liberal    9364.046           Lean Agree
#> 2: 00002        2  0.7364480           Liberal  291244.351           Lean Agree
#> 3: 00003        3 -0.2303791      Conservative 1351961.739 Don't Know / Neutral
#> 4: 00004        4  1.5154769      Very Liberal   18369.441                Agree
#> 5: 00005        5 -0.8919297      Conservative  198933.176        Lean Disagree
#> 6: 00006        6 -0.1031671      Conservative    9360.530 Don't Know / Neutral
#>    treatment proposition_vote log_income
#> 1:         1                0   9.144633
#> 2:         1                1  12.581918
#> 3:         1                1  14.117067
#> 4:         0                1   9.818444
#> 5:         0                0  12.200724
#> 6:         0                0   9.144257
```

The `fwildclusterboot` package supports estimation of linear models
based on - `lm()` from `base` R - `felm()` from `lfe` - `feols()` from
`fixest`

``` r
library(lfe)
library(fixest)

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

The `boottest` function always calculates p-values for a given
univariate hypothesis test. Second and by default, the boottest function
calculates confidence intervals by inversion of the p-value. The user
can considerably speed up the inference procedure by setting the
argument `conf_int = FALSE`, in which case no confidence intervals are
computed.

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

The function `summarize_boot` collects the results. Boottest further
comes with a `tidy` method which, in analogy with the `broom` package,
returns the estimation results as a data.frame.

``` r
summarize_boot(boot_lm)
#>  
#>  OLS estimation, Dep.Var: proposition_vote
#>  Estimation Function: lm
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.016   2.115    0.047        0    0.033

tidy(boot_lm)
#>             Estimate t value Pr(>|t|)     CI Lower   CI Upper
#> treatment 0.01641059 2.11481    0.047 0.0003776367 0.03260776
```

## Comparison to `cluster.boot()` from `multiwayvcov`

The `multiwayvcov` package offers an alternative implementation of the
wild bootstrap. As can be seen, `multiwayvcov::cluster.boot()`,
`boottest()` and sandwich standard errors produce similar results:

``` r
library(multiwayvcov)
library(lmtest)
res <- cluster.boot(lm_fit, cluster = voters$group_id, parallel = TRUE, R = 1000, wild_type = "rademacher")

# 1) results from multiwayvcov
coeftest(lm_fit, res)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)                         0.8489377  0.0535625  15.8495 < 2.2e-16 ***
#> treatment                           0.0164106  0.0077022   2.1306   0.03314 *  
#> ideology                            0.2755487  0.0155510  17.7191 < 2.2e-16 ***
#> log_income                         -0.0018217  0.0030307  -0.6011   0.54781    
#> Q1_immigrationDisagree             -0.2675849  0.0268051  -9.9826 < 2.2e-16 ***
#> Q1_immigrationLean Disagree        -0.3831930  0.0336418 -11.3904 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral -0.3257175  0.0462373  -7.0445 1.984e-12 ***
#> Q1_immigrationLean Agree           -0.2617430  0.0607877  -4.3059 1.679e-05 ***
#> Q1_immigrationAgree                -0.3850310  0.0707498  -5.4422 5.389e-08 ***
#> Q1_immigrationStrong Agree         -0.6146294  0.0842930  -7.2916 3.296e-13 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 2) results from fwildclusterboot
summarize_boot(boot_lm)
#>  
#>  OLS estimation, Dep.Var: proposition_vote
#>  Estimation Function: lm
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.016   2.115    0.047        0    0.033
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: fixest
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.016   2.115    0.047        0    0.033

# 3) sandwich standard errors from fixest
summary(feols_fit, se = "cluster", cluster = "group_id")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id) 
#>             Estimate Std. Error   t value  Pr(>|t|)    
#> treatment   0.016411   0.007842  2.092600  0.041585 *  
#> ideology    0.275549   0.015834 17.402000 < 2.2e-16 ***
#> log_income -0.001822   0.002964 -0.614579  0.541676    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -5,235.54   Adj. R2: 0.33063 
#>                           R2-Within: 0.03514
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcov`
(on 4 cores) with - N = 10000 observations - b = 10000 bootstrap
iterations - n\_g = 40 clusters

<img src="C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.png" width="50%" />
