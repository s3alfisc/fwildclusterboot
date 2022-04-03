
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwildclusterboot

<img src="man/figures/logo.png" width="200px" align="right" />

<!-- badges: start -->
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-x86_64-w64-mingw32, x86_64, mingw32, x86_64, mingw32, , 4, 1.3, 2022, 03, 10, 81868, R, R version 4.1.3 (2022-03-10), One Push-Up-orange.svg?style=flat-square)](commits/master) -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
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

The `fwildclusterboot` provides a native R implementation of the fast
wild cluster bootstrap algorithm developed in [Roodman et al
(2019)](https://econpapers.repec.org/paper/qedwpaper/1406.htm) for
regression objects in R.

It also ports functionality of
[WildBootTests.jl](https://github.com/droodman/WildBootTests.jl) to R
via the
[JuliaConnectoR](https://github.com/stefan-m-lenz/JuliaConnectoR).

The package’s central function is `boottest()`. It allows to test
univariate hypotheses using a wild cluster bootstrap at extreme speed:
via the ‘fast’ algorithm, it is possible to run a wild cluster bootstrap
with
![B = 100.000](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B%20%3D%20100.000 "B = 100.000")
iterations in less than a second!

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

`fwildclusterboot` supports the following features:

-   The wild bootstrap for OLS (Wu 1986).
-   The wild cluster bootstrap for OLS (Cameron, Gelbach & Miller 2008,
    Roodman et al, 2019)
-   The subcluster bootstrap (MacKinnon and Webb 2018).
-   Confidence intervals formed by inverting the test and iteratively
    searching for bounds.
-   Multiway clustering.
-   One-way fixed effects.

Additional features are provided through `WildBootTests.jl`:

-   The Wild Restricted Efficient bootstrap (WRE) for IV/2SLS/LIML
    [(Davidson &
    MacKinnon, 2010)](https://www.tandfonline.com/doi/abs/10.1198/jbes.2009.07221).
-   Arbitrary and multiple linear hypotheses in the parameters.

`fwildclusterboot` supports the following models:

-   OLS: `lm` (from stats), `fixest` (from fixest), `felm` from (lfe)
-   IV: `ivreg` (from ivreg).

### The `boottest()` function

For a longer introduction to `fwildclusterboot`, take a look at the
[vignette](https://s3alfisc.github.io/fwildclusterboot/articles/fwildclusterboot.html).

``` r
library(fwildclusterboot)

data(voters)
set.seed(23325)

# fit the model via fixest::feols(), lfe::felm() or stats::lm()
lm_fit <- lm(proposition_vote ~ treatment  + log_income + as.factor(Q1_immigration) + as.factor(Q2_defense), data = voters)
# bootstrap inference via boottest()
lm_boot <- boottest(lm_fit, clustid = c("group_id1"), B = 9999, param = "treatment", seed = 1)
summary(lm_boot)
#> boottest.lm(object = lm_fit, param = "treatment", B = 9999, clustid = c("group_id1"), 
#>     seed = 1)
#>  
#>  Hypothesis: 1*treatment = 0
#>  Observations: 300
#>   Bootstr. Type: rademacher
#>  Clustering: 1-way
#>  Confidence Sets: 95%
#>  Number of Clusters: 40
#> 
#>              term estimate statistic p.value conf.low conf.high
#> 1 1*treatment = 0    0.079     3.983   0.001    0.039      0.12
```

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

To run `WildBootTests.jl` through `fwildclusterboot`, `Julia` and
`WildBootTests.jl` need to be installed.

You can install Julia by following the steps described on the official
`Julia` homepage: <https://julialang.org/downloads/>. `WildBootTests.jl`
can then be installed via Julia’s package management system.

To install `WildBootTests.jl` and Julia from within R, you can
alternatively use the `JuliaConnectoR.utils` package.

``` r
devtools::install_github("s3alfisc/JuliaConnectoR.utils")
library(JuliaConnectoR.utils)
install_julia() # install Julia
connect_julia_r() # instructions to connect Julia and R
install_julia_packages("WildBootTests.jl") # install WildBootTests.jl
set_julia_ntreads() # instructions to set nthreads for Julia
```
