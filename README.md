
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
that are grouped in 20 clusters and with a small intra-cluster
correlation of 0.01. The small intra-cluster correlation implies that,
in theory, inference based on cluster-robust covariance estimates should
lead to results that are very similar to the bootstrap.

``` r
library(fwildclusterboot)


B <- 1000
seed <- 421800
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 50, icc = 0.01)
head(voters)
#>       ID group_id   ideology ideological_label     income       Q1_immigration
#> 1: 00001        1  0.4871312           Liberal  18489.800 Don't Know / Neutral
#> 2: 00002        2 -0.6971323      Conservative 991866.614        Lean Disagree
#> 3: 00003        3  1.2008411      Very Liberal 157013.089           Lean Agree
#> 4: 00004        4  0.9677738           Liberal   4007.378           Lean Agree
#> 5: 00005        5 -0.8324578      Conservative 330196.564        Lean Disagree
#> 6: 00006        6 -0.6340254      Conservative 141973.893        Lean Disagree
#>    treatment proposition_vote log_income
#> 1:         1                1   9.824975
#> 2:         0                0  13.807344
#> 3:         0                1  11.964084
#> 4:         1                1   8.295892
#> 5:         1                0  12.707443
#> 6:         0                0  11.863398
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

The `boottest` always calculates p-values for a given univariate
hypothesis test. Second and by default, the boottest function calculates
confidence intervals by inversion of the p-value. The user can
considerably speed up the inference procedure by setting the argument
`conf_int = FALSE`, in which case no confidence intervals are computed.

``` r
# 1) boottest based on object of class lm
boot_lm = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)

# 2) bootest based on object of class feols
boot_fixest = boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
boot_fixest1 = boottest(feols_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE, beta = 0)
# boot_fixest2 = boottest(feols_fit2, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE, beta = 0)

# 3) boottest based on object of class felm
boot_felm = boottest(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
boot_felm1 = boottest(felm_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = FALSE)
```

The function `summarize_boot` collects the results. Boottest further
comes with a `tidy` method which, in analogy with the `broom` package,
returns the estimation results as a data.frame.

    #>              Estimate t value Pr(>|t|)    CI Lower   CI Upper
    #> treatment 0.007899373 0.79631    0.438 -0.01378557 0.02825885
    #>  
    #>  OLS estimation, Dep.Var: proposition_vote
    #>  Estimation Function: lm
    #>  Observations:10000
    #>  Standard-errors: Clustered  
    #>  Number of Clusters:  50
    #> 
    #>           Estimate t value Pr(>|t|) CI Lower CI Upper
    #> treatment    0.008   0.796    0.438   -0.014    0.028

## Comparison to `cluster.boot()` from `multiwayvcov`

The `multiwayvcov` package offers an alternative implementation of the
wild bootstrap. As can be seen, `multiwayvcov::cluster.boot()`,
`boottest()` and sandwich standard errors produce similar results:

``` r
library(multiwayvcov)
library(lmtest)
res <- cluster.boot(lm_fit, cluster = voters$group_id, parallel = TRUE, R = 1000, wild_type = "rademacher")
coeftest(lm_fit, res)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)                         0.7568334  0.0462710  16.3565 < 2.2e-16 ***
#> treatment                           0.0078994  0.0094034   0.8401  0.400895    
#> ideology                            0.2635313  0.0141726  18.5945 < 2.2e-16 ***
#> log_income                         -0.0019768  0.0027072  -0.7302  0.465284    
#> Q1_immigrationDisagree             -0.2084371  0.0145163 -14.3588 < 2.2e-16 ***
#> Q1_immigrationLean Disagree        -0.2992484  0.0260510 -11.4870 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral -0.2455378  0.0378556  -6.4862 9.222e-11 ***
#> Q1_immigrationLean Agree           -0.1648203  0.0502522  -3.2799  0.001042 ** 
#> Q1_immigrationAgree                -0.2725906  0.0637902  -4.2732 1.944e-05 ***
#> Q1_immigrationStrong Agree         -0.5057432  0.0777175  -6.5075 8.010e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
summarize_boot(boot_lm)
#>  
#>  OLS estimation, Dep.Var: proposition_vote
#>  Estimation Function: lm
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.008   0.796    0.438   -0.014    0.028
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: fixest
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.008   0.796    0.438   -0.014    0.028
summary(feols_fit, se = "cluster", cluster = "group_id")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id) 
#>             Estimate Std. Error   t value  Pr(>|t|)    
#> treatment   0.007899   0.010025  0.787952   0.43452    
#> ideology    0.263531   0.014365 18.346000 < 2.2e-16 ***
#> log_income -0.001977   0.002641 -0.748384  0.457806    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -5,314.56   Adj. R2: 0.32141 
#>                           R2-Within: 0.03117
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcic`
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
