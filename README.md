
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwildclusterboot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
<!-- badges: end -->

The `fwildclusterboot` package is an R port of Stata’s `boottest`
package.

It implements the fast wild cluster bootstrap algorithm developed in
Roodman et al (2019) for regression objects in R. It currently works for
regression objects of type `lm`, `felm` and `fixest` from base R and the
`lfe` and `fixest` packages.

The package’s central function is `boottest()`. It allows the user to
test two-sided, univariate hypotheses using a wild cluster bootstrap.
Importantly, it uses the “fast” algorithm developed in Roodman et al,
which makes it feasible to calculate test statistics based on a large
number of bootstrap draws even for large samples–as long as the number
of bootstrapping clusters is not too large.

The `fwildclusterboot` package currently supports one- and
two-dimensional clustering and one-dimensional hypotheses.

The package is highly experimental and only includes few unit tests.

The following features will be added in the future:

  - support for multivariate hypotheses
  - bootstrap distributions beyond the rademacher distribution

## The `boottest()` function

In a first step, simulate a data set with 10000 individual observations
that are grouped in 50 clusters and with a small intra-cluster
correlation of 0.01. The small intra-cluster correlation implies that,
in theory, inference based on cluster-robust covariance estimates should
lead to results that are very similar to the bootstrap.

``` r
library(fwildclusterboot)

B <- 10000
seed <- 942413
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)
```

The `fwildclusterboot` package supports estimation of linear models
based on - `lm()` from `base` R - `felm()` from `lfe` - `feols()` from
`fixest`

``` r
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)
```

The `boottest` function always calculates p-values for a given
univariate hypothesis test. Second and by default, the boottest function
calculates confidence intervals by inversion of the p-value. The user
can considerably speed up the inference procedure by setting the
argument `conf_int = FALSE`, in which case no confidence intervals are
computed.

``` r
# 1) boottest based on object of class lm
boot_lm = boottest(lm_fit, clustid = "group_id1", B = B, seed = seed, param = "treatment", conf_int = TRUE)
```

The function `summarize_boot` collects the results. Boottest further
comes with a `tidy` method which, in analogy with the `broom` package,
returns the estimation results as a data.frame.

``` r
summarize_boot(boot_lm)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.011   1.118    0.292    -0.01    0.032
tidy(boot_lm)
#>             Estimate  t value Pr(>|t|)     CI Lower   CI Upper
#> treatment 0.01080738 1.118153   0.2918 -0.009939883 0.03185713
```

## Installation

You can install the released version of `fwildclusterboot` from github
by running

``` r
library(devtools)
#> Loading required package: usethis
#> Warning: package 'usethis' was built under R version 4.0.3
#install_github("al_fisc/fwildclusterboot")
```

## Benchmarks

Results of timing benchmarks of `fwildclusterboot` with
`sandwich::vcovBS`.

  - Experiment 1: N = 10000, B = 10000, one cluster with N\_G = 20
  - Experiment 2: N = 10000, B = 10000, one cluster with N\_G = 60
  - Experiment 3: N = 10000, B = 10000, two closters with N\_G1 = 20 and
    N\_G2 = 20, N\_G = 400

<!-- end list -->

    #> Unit: milliseconds
    #>   expr        min         lq       mean     median         uq        max neval
    #>   sw_1 56904.3184 56917.2663 57097.6597 56930.2142 57194.3304 57458.4466     3
    #>   sw_2 30837.2081 32418.2051 33080.5527 33999.2021 34202.2250 34405.2479     3
    #>   sw_3 23760.4220 23873.5572 23919.2916 23986.6923 23998.7263 24010.7604     3
    #>  fwc_1   346.8469   360.3023   375.7814   373.7578   390.2486   406.7395     3
    #> Unit: seconds
    #>   expr       min        lq      mean    median        uq       max neval
    #>   sw_1 57.047972 57.169244 57.304909 57.290516 57.433377 57.576239     3
    #>   sw_2 34.497928 34.518632 34.692708 34.539336 34.790098 35.040861     3
    #>   sw_3 24.066847 24.108285 24.130017 24.149722 24.161601 24.173481     3
    #>  fwc_1  1.308145  1.315983  1.319418  1.323822  1.325055  1.326288     3
    #>   |                                                                              |                                                                      |   0%  |                                                                              |------                                                                |   8%  |                                                                              |---------                                                             |  12%  |                                                                              |------------                                                          |  17%  |                                                                              |---------------                                                       |  21%  |                                                                              |------------------                                                    |  25%  |                                                                              |--------------------                                                  |  29%  |                                                                              |-----------------------                                               |  33%  |                                                                              |--------------------------                                            |  38%  |                                                                              |-----------------------------                                         |  42%  |                                                                              |--------------------------------                                      |  46%  |                                                                              |-----------------------------------                                   |  50%  |                                                                              |--------------------------------------                                |  54%  |                                                                              |-----------------------------------------                             |  58%  |                                                                              |--------------------------------------------                          |  62%  |                                                                              |-----------------------------------------------                       |  67%  |                                                                              |--------------------------------------------------                    |  71%  |                                                                              |----------------------------------------------------                  |  75%  |                                                                              |-------------------------------------------------------               |  79%  |                                                                              |----------------------------------------------------------            |  83%  |                                                                              |-------------------------------------------------------------         |  88%  |                                                                              |----------------------------------------------------------------      |  92%  |                                                                              |-------------------------------------------------------------------   |  96%  |                                                                              |----------------------------------------------------------------------| 100%
    #>   |                                                                              |                                                                      |   0%  |                                                                              |------                                                                |   8%  |                                                                              |---------                                                             |  12%  |                                                                              |------------                                                          |  17%  |                                                                              |---------------                                                       |  21%  |                                                                              |------------------                                                    |  25%  |                                                                              |--------------------                                                  |  29%  |                                                                              |-----------------------                                               |  33%  |                                                                              |--------------------------                                            |  38%  |                                                                              |-----------------------------                                         |  42%  |                                                                              |--------------------------------                                      |  46%  |                                                                              |-----------------------------------                                   |  50%  |                                                                              |--------------------------------------                                |  54%  |                                                                              |-----------------------------------------                             |  58%  |                                                                              |--------------------------------------------                          |  62%  |                                                                              |-----------------------------------------------                       |  67%  |                                                                              |--------------------------------------------------                    |  71%  |                                                                              |----------------------------------------------------                  |  75%  |                                                                              |-------------------------------------------------------               |  79%  |                                                                              |----------------------------------------------------------            |  83%  |                                                                              |-------------------------------------------------------------         |  88%  |                                                                              |----------------------------------------------------------------      |  92%  |                                                                              |-------------------------------------------------------------------   |  96%  |                                                                              |----------------------------------------------------------------------| 100%
    #>   |                                                                              |                                                                      |   0%  |                                                                              |------                                                                |   8%  |                                                                              |---------                                                             |  12%  |                                                                              |------------                                                          |  17%  |                                                                              |---------------                                                       |  21%  |                                                                              |------------------                                                    |  25%  |                                                                              |--------------------                                                  |  29%  |                                                                              |-----------------------                                               |  33%  |                                                                              |--------------------------                                            |  38%  |                                                                              |-----------------------------                                         |  42%  |                                                                              |--------------------------------                                      |  46%  |                                                                              |-----------------------------------                                   |  50%  |                                                                              |--------------------------------------                                |  54%  |                                                                              |-----------------------------------------                             |  58%  |                                                                              |--------------------------------------------                          |  62%  |                                                                              |-----------------------------------------------                       |  67%  |                                                                              |--------------------------------------------------                    |  71%  |                                                                              |----------------------------------------------------                  |  75%  |                                                                              |-------------------------------------------------------               |  79%  |                                                                              |----------------------------------------------------------            |  83%  |                                                                              |-------------------------------------------------------------         |  88%  |                                                                              |----------------------------------------------------------------      |  92%  |                                                                              |-------------------------------------------------------------------   |  96%  |                                                                              |----------------------------------------------------------------------| 100%
    #> Unit: seconds
    #>   expr       min        lq      mean    median        uq      max neval
    #>   sw_3 275.48260 294.60570 303.55139 313.72880 317.58579 321.4428     3
    #>  fwc_1  20.59066  20.80572  25.03011  21.02077  27.24983  33.4789     3
