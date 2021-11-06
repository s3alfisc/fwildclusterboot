
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwildclusterboot

<!-- <img src="man/figures/logo.png" width="160px" align="right" /> -->
<!-- badges: start -->
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-x86_64-w64-mingw32, x86_64, mingw32, x86_64, mingw32, , 4, 0.5, 2021, 03, 31, 80133, R, R version 4.0.5 (2021-03-31), Shake and Throw-orange.svg?style=flat-square)](commits/master) -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
![runiverse-package](https://s3alfisc.r-universe.dev/badges/fwildclusterboot)
[![R-CMD-check](https://github.com/s3alfisc/fwildclusterboot/workflows/R-CMD-check/badge.svg)](https://github.com/s3alfisc/fwildclusterboot/actions)
[![Codecov test
coverage](https://codecov.io/gh/s3alfisc/fwildclusterboot/branch/master/graph/badge.svg)](https://app.codecov.io/gh/s3alfisc/fwildclusterboot?branch=master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/fwildclusterboot?color=blue)](https://cran.r-project.org/package=fwildclusterboot)
[![](http://cranlogs.r-pkg.org/badges/last-month/fwildclusterboot?color=green)](https://cran.r-project.org/package=fwildclusterboot)
<!-- [![minimal R version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/) -->

<!-- badges: end -->

The `fwildclusterboot` package is an R port of STATA’s
[boottest](https://github.com/droodman/boottest) package.

It implements the fast wild cluster bootstrap algorithm developed in
[Roodman et al
(2019)](https://econpapers.repec.org/paper/qedwpaper/1406.htm) for
regression objects in R. It currently works for regression objects of
type `lm`, `felm` and `fixest` from base R and the `lfe` and `fixest`
packages.

The package’s central function is `boottest()`. It allows the user to
test univariate hypotheses using a wild cluster bootstrap. The “fast”
algorithm developed in Roodman et al makes it feasible to calculate test
statistics based on a large number of bootstrap draws even for large
samples – as long as the number of bootstrapping clusters is not too
large.

The `fwildclusterboot` package currently supports multi-dimensional
clustering and one-dimensional hypotheses. It supports regression
weights, multiple distributions of bootstrap weights, fixed effects,
restricted (WCR) and unrestricted (WCU) bootstrap inference and
subcluster bootstrapping for few treated clusters [(MacKinnon & Webb,
(2018))](https://academic.oup.com/ectj/article-abstract/21/2/114/5078969).

<!-- The following features will be added in the future:  -->
<!-- * support for multivariate hypotheses  -->
<!-- * bootstrap distributions beyond the rademacher distribution -->

### The `boottest()` function

``` r
# note: for performance reasons, the sampling of the bootstrap weights of types Rademacher, Webb and Normal within
# fwildclusterboot are handled via the dqrng package, which is installed with the
# package as a dependency. To set a global for boottest() seed for these weight types, you must use dqrng's dqset.seed() function
# For Mammen weights, you need to set a global seed via the set.seed() function.

# set global seed for Rademacher, Webb and Normal weights
library(dqrng)
dqrng::dqset.seed(965326)
# set a global seed for Mammen weights
set.seed(23325)

library(fwildclusterboot)

data(voters)

# fit the model via fixest::feols(), lfe::felm() or stats::lm()

lm_fit <- lm(proposition_vote ~ treatment  + log_income + as.factor(Q1_immigration) + as.factor(Q2_defense), data = voters)
# bootstrap inference via boottest()
lm_boot <- boottest(lm_fit, clustid = c("group_id1"), B = 9999, param = "treatment", seed = 1)
summary(lm_boot)
#> boottest.lm(object = lm_fit, clustid = c("group_id1"), param = "treatment", 
#>     B = 9999, seed = 1)
#>  
#>  Hypothesis: 1*treatment = 0
#>  Observations: 300
#>  Bootstr. Iter: 9999
#>  Bootstr. Type: rademacher
#>  Clustering: 1-way
#>  Confidence Sets: 95%
#>  Number of Clusters: 40
#> 
#>              term estimate statistic p.value conf.low conf.high
#> 1 1*treatment = 0    0.079     4.123       0     0.04     0.118

library(fixest)
feols_fit <- feols(proposition_vote ~ treatment  + log_income | Q1_immigration + Q2_defense, data = voters)
# bootstrap inference via boottest()
feols_boot <- boottest(feols_fit, clustid = c("group_id1"), B = 9999, param = "treatment", seed = 1)
summary(feols_boot)
#> boottest.fixest(object = feols_fit, clustid = c("group_id1"), 
#>     param = "treatment", B = 9999, seed = 1)
#>  
#>  Hypothesis: 1*treatment = 0
#>  Observations: 300
#>   Bootstr. Type: rademacher
#>  Clustering: 1-way
#>  Confidence Sets: 95%
#>  Number of Clusters: 40
#> 
#>              term estimate statistic p.value conf.low conf.high
#> 1 1*treatment = 0    0.079     4.123       0     0.04     0.118
```

For a longer introduction to the package’s key function, `boottest()`,
please follow this
[link](https://s3alfisc.github.io/fwildclusterboot/articles/fwildclusterboot.html).

### Benchmarks

Results of timing benchmarks of `boottest()`, with a sample of N =
50000, k = 19 covariates and one cluster of dimension N\_G (10
iterations each).

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

<!-- For a small number of clusters, `fwildclusterboot` is in generally faster than implementations of the wild cluster bootstrap in the `sandwich` and `clusterSEs` packages.  -->
<!-- ![Benchmark](man/figures/bench_ggplot.png) -->

### Installation

You can install compiled versions of`fwildclusterboot` from CRAN and the
development version from R-universe (compiled) or github by following
one of the steps below:

``` r
# from CRAN 
install.packages("fwildclusterboot")

# from r-universe (windows & mac, compiled R > 4.0 required)
install.packages('fwildclusterboot', repos ='https://s3alfisc.r-universe.dev')

# dev version from github
# note: installation requires Rtools
library(devtools)
install_github("s3alfisc/fwildclusterboot")
```
