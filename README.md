
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwildclusterboot

<!-- badges: start -->

<!-- [![packageversion](https://img.shields.io/badge/Package%20version-x86_64-w64-mingw32, x86_64, mingw32, x86_64, mingw32, , 4, 0.3, 2020, 10, 10, 79318, R, R version 4.0.3 (2020-10-10), Bunny-Wunnies Freak Out-orange.svg?style=flat-square)](commits/master) -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
[![R-CMD-check](https://github.com/s3alfisc/fwildclusterboot/workflows/R-CMD-check/badge.svg)](https://github.com/s3alfisc/fwildclusterboot/actions)
[![Codecov test
coverage](https://codecov.io/gh/s3alfisc/fwildclusterboot/branch/master/graph/badge.svg)](https://codecov.io/gh/s3alfisc/fwildclusterboot?branch=master)
<!-- [![minimal R version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/) -->

<!-- badges: end -->

The `fwildclusterboot` package is an R port of Stata’s
[boottest](https://github.com/droodman/boottest) package.

It implements the fast wild cluster bootstrap algorithm developed in
[Roodman et al
(2019)](https://econpapers.repec.org/paper/qedwpaper/1406.htm) for
regression objects in R. It currently works for regression objects of
type `lm`, `felm` and `fixest` from base R and the `lfe` and `fixest`
packages.

The package’s central function is `boottest()`. It allows the user to
test two-sided, univariate hypotheses using a wild cluster bootstrap.
Importantly, it uses the “fast” algorithm developed in Roodman et al,
which makes it feasible to calculate test statistics based on a large
number of bootstrap draws even for large samples – as long as the number
of bootstrapping clusters is not too large.

The `fwildclusterboot` package currently supports multi-dimensional
clustering and one-dimensional, two-sided hypotheses. It supports
regression weights, multiple distributions of bootstrap weights, fixed
effects, restricted (WCR) and unrestricted (WCU) bootstrap inference and
subcluster bootstrapping for few treated clusters [(MacKinnon & Webb,
(2018))](https://academic.oup.com/ectj/article-abstract/21/2/114/5078969).

For a quick introduction to the package’s key function, `boottest()`,
please follow this
[link](https://s3alfisc.github.io/fwildclusterboot/articles/fwildclusterboot.html).

<!-- The following features will be added in the future:  -->

<!-- * support for multivariate hypotheses  -->

<!-- * bootstrap distributions beyond the rademacher distribution -->

## Benchmarks

Results of timing benchmarks of `fwildclusterboot` with
`sandwich::vcovBS` (one replication):

  - Benchmark 1: one cluster with dimension \(N_G = 40\)
  - Benchmark 2: two clusters with dimensions \(N_{G1}= 40\),
    \(N_{G2} = 20\), \(N_{G12} = 800\)

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

<!-- ## The `boottest` function  -->

<!-- The `fwildclusterboot` package supports wild cluster bootstrap inference for linear models based on  -->

<!-- - `lm()` from `base` R -->

<!-- - `felm()` from `lfe` -->

<!-- - `feols()` from `fixest` -->

<!-- ```{r, warning = FALSE, message = FALSE} -->

<!-- library(fwildclusterboot) -->

<!-- B <- 99999 -->

<!-- seed <- 942413 -->

<!-- set.seed(seed) -->

<!-- voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 10, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed) -->

<!-- # estimate the regression model -->

<!-- lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters) -->

<!-- # bootstrap estimation -->

<!-- boot_lm <- boottest(lm_fit, clustid = "group_id1", B = B, seed = seed, param = "treatment", conf_int = TRUE) -->

<!-- # summarize the results -->

<!-- summary(boot_lm) -->

<!-- tidy(boot_lm) -->

<!-- ``` -->

## Installation

You can install the development version of `fwildclusterboot` from
github by following the steps below.

``` r
# note: installation requires Rtools
library(devtools)
install_github("s3alfisc/fwildclusterboot")
```
