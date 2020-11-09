
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
seed <- 4218001
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 50, icc = 0.01)
head(voters)
#>       ID group_id    ideology ideological_label     income       Q1_immigration
#> 1: 00001        1  0.09165946           Liberal  139110.97 Don't Know / Neutral
#> 2: 00002        2 -1.14156606 Very Conservative  236286.11        Lean Disagree
#> 3: 00003        3 -0.12900503      Conservative   33481.27 Don't Know / Neutral
#> 4: 00004        4 -0.70932438      Conservative 4725456.31        Lean Disagree
#> 5: 00005        5  0.52097999           Liberal   56214.56           Lean Agree
#> 6: 00006        6 -0.36693380      Conservative  250774.40 Don't Know / Neutral
#>    treatment proposition_vote log_income
#> 1:         0                1   11.84303
#> 2:         1                0   12.37280
#> 3:         1                1   10.41874
#> 4:         1                0   15.36847
#> 5:         1                0   10.93693
#> 6:         1                1   12.43231
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
#> treatment   -0.012   1.629     0.11   -0.027    0.003

tidy(boot_lm)
#>              Estimate  t value Pr(>|t|)    CI Lower    CI Upper
#> treatment -0.01211134 1.629137     0.11 -0.02734796 0.003213481
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
#> (Intercept)                         0.8687348  0.0550820  15.7717 < 2.2e-16 ***
#> treatment                          -0.0121113  0.0074507  -1.6255    0.1041    
#> ideology                            0.2911888  0.0166839  17.4533 < 2.2e-16 ***
#> log_income                         -0.0028594  0.0027860  -1.0264    0.3048    
#> Q1_immigrationDisagree             -0.2517596  0.0195443 -12.8815 < 2.2e-16 ***
#> Q1_immigrationLean Disagree        -0.3760881  0.0302854 -12.4181 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral -0.3293095  0.0473033  -6.9617 3.574e-12 ***
#> Q1_immigrationLean Agree           -0.2928206  0.0626473  -4.6741 2.991e-06 ***
#> Q1_immigrationAgree                -0.4187955  0.0783035  -5.3484 9.071e-08 ***
#> Q1_immigrationStrong Agree         -0.6414487  0.0926725  -6.9217 4.738e-12 ***
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
#> treatment   -0.012   1.629     0.11   -0.027    0.003
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: fixest
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.012   1.629    0.111   -0.027    0.003

# 3) sandwich standard errors from fixest
summary(feols_fit, se = "cluster", cluster = "group_id")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id) 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> treatment  -0.012111   0.007513  -1.612  0.113377    
#> ideology    0.291189   0.017542  16.599 < 2.2e-16 ***
#> log_income -0.002859   0.002779  -1.029  0.308546    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -5,236.74   Adj. R2: 0.33187 
#>                           R2-Within: 0.03885
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcov`
with - N = 10000 observations - b = 10000 bootstrap iterations - n\_g =
40 clusters and 4 cores for the parallel option.

``` r
readRDS("C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.rds")
#>                test replications elapsed relative user.self sys.self user.child
#> 4    boottest_feols           10   70.14    1.000     63.59     4.84         NA
#> 3       boottest_lm           10   72.19    1.029     66.36     3.98         NA
#> 1          multiway           10  982.67   14.010    981.69     0.64         NA
#> 2 multiway_parallel           10  372.48    5.311      1.00     0.50         NA
#>   sys.child
#> 4        NA
#> 3        NA
#> 1        NA
#> 2        NA
```
