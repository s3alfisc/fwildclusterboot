
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
#> treatment    0.011    1.09    0.292    -0.01    0.032
tidy(boot_lm)
#>             Estimate  t value Pr(>|t|)     CI Lower   CI Upper
#> treatment 0.01080738 1.089841   0.2918 -0.009933565 0.03186458
```

## Installation

You can install the released version of `fwildclusterboot` from github
by running

``` r
library(devtools)
#> Loading required package: usethis
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
    #>   expr        min        lq       mean     median        uq        max neval
    #>   sw_1 61681.2375 62013.246 63593.7106 62345.2553 64549.947 66754.6391     3
    #>   sw_2 38121.1677 38632.709 40150.0504 39144.2496 41164.492 43184.7338     3
    #>   sw_3 26525.2845 26571.477 34477.6982 26617.6700 38453.905 50290.1400     3
    #>  fwc_1   340.7766   371.233   520.1882   401.6895   609.894   818.0985     3
    #> Unit: seconds
    #>   expr       min        lq      mean    median         uq       max neval
    #>   sw_1 57.636572 77.194044 86.449840 96.751515 100.856474 104.96143     3
    #>   sw_2 31.415596 32.500917 33.031202 33.586238  33.839004  34.09177     3
    #>   sw_3 20.988515 23.154911 23.925154 25.321308  25.393473  25.46564     3
    #>  fwc_1  1.093759  1.141636  1.419141  1.189514   1.581832   1.97415     3
    #>   |                                                                              |                                                                      |   0%  |                                                                              |------                                                                |   8%  |                                                                              |---------                                                             |  12%  |                                                                              |------------                                                          |  17%  |                                                                              |---------------                                                       |  21%  |                                                                              |------------------                                                    |  25%  |                                                                              |--------------------                                                  |  29%  |                                                                              |-----------------------                                               |  33%  |                                                                              |--------------------------                                            |  38%  |                                                                              |-----------------------------                                         |  42%  |                                                                              |--------------------------------                                      |  46%  |                                                                              |-----------------------------------                                   |  50%  |                                                                              |--------------------------------------                                |  54%  |                                                                              |-----------------------------------------                             |  58%  |                                                                              |--------------------------------------------                          |  62%  |                                                                              |-----------------------------------------------                       |  67%  |                                                                              |--------------------------------------------------                    |  71%  |                                                                              |----------------------------------------------------                  |  75%  |                                                                              |-------------------------------------------------------               |  79%  |                                                                              |----------------------------------------------------------            |  83%  |                                                                              |-------------------------------------------------------------         |  88%  |                                                                              |----------------------------------------------------------------      |  92%  |                                                                              |-------------------------------------------------------------------   |  96%  |                                                                              |----------------------------------------------------------------------| 100%
    #>   |                                                                              |                                                                      |   0%  |                                                                              |------                                                                |   8%  |                                                                              |---------                                                             |  12%  |                                                                              |------------                                                          |  17%  |                                                                              |---------------                                                       |  21%  |                                                                              |------------------                                                    |  25%  |                                                                              |--------------------                                                  |  29%  |                                                                              |-----------------------                                               |  33%  |                                                                              |--------------------------                                            |  38%  |                                                                              |-----------------------------                                         |  42%  |                                                                              |--------------------------------                                      |  46%  |                                                                              |-----------------------------------                                   |  50%  |                                                                              |--------------------------------------                                |  54%  |                                                                              |-----------------------------------------                             |  58%  |                                                                              |--------------------------------------------                          |  62%  |                                                                              |-----------------------------------------------                       |  67%  |                                                                              |--------------------------------------------------                    |  71%  |                                                                              |----------------------------------------------------                  |  75%  |                                                                              |-------------------------------------------------------               |  79%  |                                                                              |----------------------------------------------------------            |  83%  |                                                                              |-------------------------------------------------------------         |  88%  |                                                                              |----------------------------------------------------------------      |  92%  |                                                                              |-------------------------------------------------------------------   |  96%  |                                                                              |----------------------------------------------------------------------| 100%
    #>   |                                                                              |                                                                      |   0%  |                                                                              |------                                                                |   8%  |                                                                              |---------                                                             |  12%  |                                                                              |------------                                                          |  17%  |                                                                              |---------------                                                       |  21%  |                                                                              |------------------                                                    |  25%  |                                                                              |--------------------                                                  |  29%  |                                                                              |-----------------------                                               |  33%  |                                                                              |--------------------------                                            |  38%  |                                                                              |-----------------------------                                         |  42%  |                                                                              |--------------------------------                                      |  46%  |                                                                              |-----------------------------------                                   |  50%  |                                                                              |--------------------------------------                                |  54%  |                                                                              |-----------------------------------------                             |  58%  |                                                                              |--------------------------------------------                          |  62%  |                                                                              |-----------------------------------------------                       |  67%  |                                                                              |--------------------------------------------------                    |  71%  |                                                                              |----------------------------------------------------                  |  75%  |                                                                              |-------------------------------------------------------               |  79%  |                                                                              |----------------------------------------------------------            |  83%  |                                                                              |-------------------------------------------------------------         |  88%  |                                                                              |----------------------------------------------------------------      |  92%  |                                                                              |-------------------------------------------------------------------   |  96%  |                                                                              |----------------------------------------------------------------------| 100%
    #> Unit: seconds
    #>   expr        min         lq       mean     median         uq        max neval
    #>   sw_3 293.757910 307.052657 316.673433 320.347404 328.131194 335.914984     3
    #>  fwc_1   4.099668   4.295642   5.042379   4.491616   5.513734   6.535851     3
