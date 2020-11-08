
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
seed <- 4218
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 50, icc = 0.01)
head(voters)
#>       ID group_id   ideology ideological_label   income       Q1_immigration
#> 1: 00001        1 -0.7989567      Conservative 76637.04        Lean Disagree
#> 2: 00002        2  0.9313345           Liberal 60723.90           Lean Agree
#> 3: 00003        3  0.9716515           Liberal 14010.80           Lean Agree
#> 4: 00004        4  0.1280615           Liberal 68454.32 Don't Know / Neutral
#> 5: 00005        5  1.6851580      Very Liberal 19400.30                Agree
#> 6: 00006        6 -0.9016660      Conservative 12303.54        Lean Disagree
#>    treatment proposition_vote log_income
#> 1:         0                1  11.246836
#> 2:         0                1  11.014093
#> 3:         1                1   9.547584
#> 4:         1                1  11.133922
#> 5:         1                1   9.873044
#> 6:         1                0   9.417643
```

The `fwildclusterboot` package supports estimation of linear models
based on - `lm()` from `base` R - `felm()` from `lfe` - `feols()` from
`fixest`

The `boottest` always calculates p-values for a given univariate
hypothesis test. Second and by default, the boottest function calculates
confidence intervals by inversion of the p-value. The user can
considerably speed up the inference procedure by setting the argument
`conf_int = FALSE`, in which case no confidence intervals are computed.

A `summary` method collects the results. Boottest further comes with a
`tidy` method which, in analogy with the `broom` package, returns the
estimation results as a data.frame.

    #>               Estimate   t value Pr(>|t|)   CI Lower   CI Upper
    #> treatment -0.002069159 0.3432983    0.739 -0.0144383 0.01041703
    #>            Length Class      Mode     
    #> p_val       1     -none-     numeric  
    #> conf_int    2     -none-     numeric  
    #> t_stat      1     -none-     numeric  
    #> regression 13     lm         list     
    #> param       1     -none-     character
    #> N           1     -none-     numeric  
    #> B           1     -none-     numeric  
    #> clustid     1     data.frame list     
    #> depvar      1     -none-     character
    #> N_G         1     -none-     numeric

## Comparison to `cluster.boot()` from `multiwayvcov`

The `multiwayvcov` package offers an alternative implementation of the
wild bootstrap. As can be seen, `multiwayvcov::cluster.boot()`,
`boottest()` and sandwich standard errors produce similar results:

    #> 
    #> t test of coefficients:
    #> 
    #>                                      Estimate Std. Error  t value  Pr(>|t|)    
    #> (Intercept)                         0.7883209  0.0582431  13.5350 < 2.2e-16 ***
    #> treatment                          -0.0020692  0.0060415  -0.3425    0.7320    
    #> ideology                            0.2795615  0.0152069  18.3839 < 2.2e-16 ***
    #> log_income                          0.0030321  0.0026428   1.1473    0.2513    
    #> Q1_immigrationDisagree             -0.2607537  0.0228752 -11.3990 < 2.2e-16 ***
    #> Q1_immigrationLean Disagree        -0.3879762  0.0321257 -12.0768 < 2.2e-16 ***
    #> Q1_immigrationDon't Know / Neutral -0.3239716  0.0453695  -7.1407 9.930e-13 ***
    #> Q1_immigrationLean Agree           -0.2732158  0.0607674  -4.4961 6.999e-06 ***
    #> Q1_immigrationAgree                -0.3566562  0.0703159  -5.0722 4.003e-07 ***
    #> Q1_immigrationStrong Agree         -0.6153047  0.0852054  -7.2214 5.520e-13 ***
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #>            Length Class      Mode     
    #> p_val       1     -none-     numeric  
    #> conf_int    2     -none-     numeric  
    #> t_stat      1     -none-     numeric  
    #> regression 13     lm         list     
    #> param       1     -none-     character
    #> N           1     -none-     numeric  
    #> B           1     -none-     numeric  
    #> clustid     1     data.frame list     
    #> depvar      1     -none-     character
    #> N_G         1     -none-     numeric
    #>            Length Class      Mode     
    #> p_val       1     -none-     numeric  
    #> conf_int    2     -none-     numeric  
    #> t_stat      1     -none-     numeric  
    #> regression 28     fixest     list     
    #> param       1     -none-     character
    #> N           1     -none-     numeric  
    #> B           1     -none-     numeric  
    #> clustid     1     data.frame list     
    #> N_G         1     -none-     numeric
    #> OLS estimation, Dep. Var.: proposition_vote
    #> Observations: 10,000 
    #> Fixed-effects: Q1_immigration: 7
    #> Standard-errors: Clustered (group_id) 
    #>             Estimate Std. Error   t value  Pr(>|t|)    
    #> treatment  -0.002069   0.006091 -0.339695  0.735538    
    #> ideology    0.279561   0.015153 18.449000 < 2.2e-16 ***
    #> log_income  0.003032   0.002600  1.166200  0.249195    
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> Log-likelihood: -5,267.28   Adj. R2: 0.32779 
    #>                           R2-Within: 0.03391

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
