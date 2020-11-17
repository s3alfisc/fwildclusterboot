
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


B <- 10000
seed <- 12394
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 20, icc2 = 0.5)
head(voters)
#>       ID group_id1 group_id2   ideology1  ideology2 ideological_label
#> 1: 00001         9         8  0.07782188  1.3373391           Liberal
#> 2: 00002         1         9 -0.84245921 -0.3644535      Conservative
#> 3: 00003        11         2 -0.89288320  0.7224659      Conservative
#> 4: 00004        15         4  1.21421475 -0.6124951      Very Liberal
#> 5: 00005         1         6 -1.80231527 -0.8245564 Very Conservative
#> 6: 00006        14         8  0.39554242  2.7268235           Liberal
#>        income       Q1_immigration treatment proposition_vote log_income
#> 1:   22499.80           Lean Agree         0                1  10.021262
#> 2:   19383.48 Don't Know / Neutral         0                0   9.872176
#> 3:  495361.09           Lean Agree         0                1  13.113042
#> 4:   72164.76        Lean Disagree         0                0  11.186707
#> 5: 1084713.28        Lean Disagree         1                0  13.896826
#> 6:   11291.59         Strong Agree         1                1   9.331813
```

The `fwildclusterboot` package supports estimation of linear models
based on - `lm()` from `base` R - `felm()` from `lfe` - `feols()` from
`fixest`

``` r
library(lfe)
library(fixest)

# 1) boottest based on object of class lm
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , weights = NULL, data = voters)

# 2) boottest based on object of class fixest
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income , fixef = c("Q1_immigration"), weights = NULL, data = voters)
feols_fit1 <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
feols_fit2 <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration, weights = NULL, data = voters)

# 3) bootest based on object of class felm
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration | 0 |  group_id1, weights = NULL, data = voters)
felm_fit1 <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration, weights = NULL, data = voters)
```

The `boottest` function always calculates p-values for a given
univariate hypothesis test. Second and by default, the boottest function
calculates confidence intervals by inversion of the p-value. The user
can considerably speed up the inference procedure by setting the
argument `conf_int = FALSE`, in which case no confidence intervals are
computed.

``` r
# 1) boottest based on object of class lm
boot_lm = boottest(lm_fit, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)

# 2) bootest based on object of class feols
boot_fixest = boottest(feols_fit, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)
boot_fixest = boottest(feols_fit, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, demean = TRUE)

boot_fixest1 = boottest(feols_fit1, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.05)
# boot_fixest2 = boottest(feols_fit2, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0)

# 3) boottest based on object of class felm
boot_felm = boottest(felm_fit, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)
boot_felm1 = boottest(felm_fit1, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)
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
#> treatment    0.014   1.574    0.145   -0.005    0.034

tidy(boot_lm)
#>            Estimate  t value Pr(>|t|)     CI Lower   CI Upper
#> treatment 0.0142842 1.573655   0.1454 -0.005376009 0.03415132
```

Change the confidence level:

``` r
boot_lm_5 = boottest(lm_fit, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.05)

boot_lm_20 = boottest(lm_fit, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.20)

summarize_boot(boot_lm_5)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.014   1.574    0.145   -0.005    0.034
summarize_boot(boot_lm_20)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.014   1.574    0.145    0.002    0.027

confint(feols_fit, "treatment", level = 0.95, se = "cluster", cluster = "group_id1")
#>                  2.5 %     97.5 %
#> treatment -0.003976954 0.03254536
confint(feols_fit, "treatment", level = 0.80, se = "cluster", cluster = "group_id1")
#>                  10 %       90 %
#> treatment 0.002343874 0.02622453
```

Plot the confidence sets:

``` r
plot_boot(boot_lm)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="50%" />

## Comparison to `cluster.boot()` from `multiwayvcov`

The `multiwayvcov` package offers an alternative implementation of the
wild bootstrap. As can be seen, `multiwayvcov::cluster.boot()`,
`boottest()` and sandwich standard errors produce similar results:

``` r
library(multiwayvcov)
library(lmtest)
res <- cluster.boot(lm_fit, cluster = voters$group_id1, parallel = TRUE, R = 1000, wild_type = "rademacher")

# 1) results from multiwayvcov
coeftest(lm_fit, res)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                         0.0318851  0.0295409  1.0794    0.2805    
#> treatment                           0.0142842  0.0091455  1.5619    0.1183    
#> ideology1                           0.1933811  0.0036041 53.6556 < 2.2e-16 ***
#> log_income                         -0.0028942  0.0021191 -1.3658    0.1720    
#> Q1_immigrationDisagree              0.0797053  0.0194414  4.0998 4.168e-05 ***
#> Q1_immigrationLean Disagree         0.2546432  0.0220345 11.5565 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral  0.4845188  0.0213659 22.6772 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.7561481  0.0159346 47.4533 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.9073336  0.0208381 43.5421 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9772280  0.0191047 51.1513 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 2) results from fwildclusterboot
summarize_boot(boot_lm)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.014   1.574    0.145   -0.005    0.034
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  20
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.014   1.574    0.145   -0.005    0.034

# 3) sandwich standard errors from fixest
summary(feols_fit, se = "cluster", cluster = "group_id1")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id1) 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> treatment   0.014284   0.009317  1.5331  0.141729    
#> ideology1   0.193381   0.003603 53.6690 < 2.2e-16 ***
#> log_income -0.002894   0.002134 -1.3563  0.190904    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -3,813.08   Adj. R2: 0.47585 
#>                           R2-Within: 0.23806
```

## Some Tests with 2-way clustering

``` r
library(sandwich)
library(lmtest)

boot_lm = boottest(lm_fit, clustid = ~group_id1 + group_id2, B = B, seed = seed, param = "treatment", conf_int = FALSE)
#> Warning in preprocess.lm(object = object, param = param, clustid = clustid, :
#> You are estimating a model with more than 200 clusters. Are you sure you want to
#> proceed with bootstrap standard errors instead of asymptotic sandwich standard
#> errors? The more clusters in the data, the longer the estimation process.

summarize_boot(boot_lm)
#> Warning in min(object$conf_int): no non-missing arguments to min; returning Inf
#> Warning in max(object$conf_int): no non-missing arguments to max; returning -Inf
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  400
#> 
#>           Estimate  t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.014 29975.56    0.114      Inf     -Inf
vocv <- vcovCL(lm_fit, ~ group_id1 + group_id2)
coeftest(lm_fit, vcov)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                         0.0318851  0.0445451  0.7158   0.47414    
#> treatment                           0.0142842  0.0070910  2.0144   0.04399 *  
#> ideology1                           0.1933811  0.0045387 42.6073 < 2.2e-16 ***
#> log_income                         -0.0028942  0.0025920 -1.1166   0.26420    
#> Q1_immigrationDisagree              0.0797053  0.0364923  2.1842   0.02897 *  
#> Q1_immigrationLean Disagree         0.2546432  0.0346009  7.3594 1.992e-13 ***
#> Q1_immigrationDon't Know / Neutral  0.4845188  0.0342160 14.1606 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.7561481  0.0341920 22.1148 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.9073336  0.0345839 26.2357 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9772280  0.0362021 26.9937 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcov`
(on 4 cores) with - N = 10000 observations - b = 10000 bootstrap
iterations - n\_g = 50 clusters
