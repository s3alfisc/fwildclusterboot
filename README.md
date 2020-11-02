
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
#> Loading required package: Matrix
library(fixest)
#> Warning: package 'fixest' was built under R version 4.0.3

library(fwildclusterboot)
#> 
#> Attaching package: 'fwildclusterboot'
#> The following objects are masked _by_ '.GlobalEnv':
#> 
#>     boottest, create_data_1, create_data_2
B <- 1000
seed <- 421
set.seed(seed)
voters <- create_data_1(N = 3000, N_G = 20, icc = 0.01)
head(voters)
#>      ID group_id    ideology ideological_label      income       Q1_immigration
#> 1: 0001        1 -1.81640013 Very Conservative 1549473.029             Disagree
#> 2: 0002        2  1.73220900      Very Liberal    1563.684                Agree
#> 3: 0003        3  1.09039484      Very Liberal    6332.606           Lean Agree
#> 4: 0004        4 -0.04439857      Conservative  119397.062 Don't Know / Neutral
#> 5: 0005        5  0.04503661           Liberal   57125.405 Don't Know / Neutral
#> 6: 0006        6 -1.44487347 Very Conservative  548098.456        Lean Disagree
#>    treatment proposition_vote log_income
#> 1:         1                0  14.253425
#> 2:         0                1   7.354800
#> 3:         1                1   8.753467
#> 4:         0                1  11.690210
#> 5:         0                0  10.953004
#> 6:         0                1  13.214210
```

The `fwildclusterboot` package supports estimation of linear models
based on - `lm()` from `base` R - `felm()` from `lfe` - `feols()` from
`fixest`

``` r

lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income + Q1_immigration , weights = NULL, data = voters)

feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
feols_fit1 <- feols(proposition_vote ~ treatment + ideology + log_income + Q1_immigration, weights = NULL, data = voters)

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
boot_lm = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)

boot_felm = boottest(felm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)

boot_felm1 = boottest(felm_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)

boot_fixest = boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)

boot_fixest1 = boottest(feols_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE)
```

A `summary` method collects the results.

``` r
summary(boot_lm)
#>  
#>  OLS estimation, Dep.Var: proposition_vote
#>  Estimation Function: lm
#>  Observations:3000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.013   0.923    0.396   -0.018    0.046
summary(boot_felm)
#>  
#>   Estimation Function: felm
#>  Observations:3000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1    0.013   0.923    0.396   -0.018    0.046
summary(boot_felm1)
#>  
#>   Estimation Function: felm
#>  Observations:3000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1    0.013   0.923    0.396   -0.018    0.046
summary(boot_fixest)
#>  
#>   Estimation Function: fixest
#>  Observations:3000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.013   0.923    0.396   -0.018    0.046
summary(boot_fixest1)
#>  
#>   Estimation Function: fixest
#>  Observations:3000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.013   0.923    0.396   -0.018    0.046
```

These estimates are very close to estimates using sandwich cluster
robust estimators:

``` r
fixest:::summary.fixest(feols_fit, se = "cluster", cluster = "group_id")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 3,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id) 
#>             Estimate Std. Error   t value    Pr(>|t|)    
#> treatment   0.013395   0.014910  0.898433 3.80200e-01    
#> ideology    0.316626   0.021535 14.703000 7.82000e-12 ***
#> log_income -0.005926   0.005949 -0.996230 3.31658e-01    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -1,565.19   Adj. R2: 0.3323 
#>                           R2-Within: 0.04756
```

Boottest comes with a `tidy` method which, in analogy with the `broom`
package, returns the estimation results as a data.frame.

``` r
tidy(boot_lm)
#>             Estimate  t value Pr(>|t|)    CI Lower   CI Upper
#> treatment 0.01339528 0.923159    0.396 -0.01833772 0.04630242
tidy(boot_felm)
#>     Estimate  t value Pr(>|t|)    CI Lower   CI Upper
#> 1 0.01339528 0.923159    0.396 -0.01849444 0.04625549
tidy(boot_fixest)
#>             Estimate  t value Pr(>|t|)    CI Lower   CI Upper
#> treatment 0.01339528 0.923159    0.396 -0.01849847 0.04626049
```
