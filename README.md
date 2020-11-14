
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
seed <- 1234
set.seed(seed)
voters <- create_data_1(N = 10000, N_G = 50, icc = 0.01)
head(voters)
#>       ID group_id   ideology ideological_label     income       Q1_immigration
#> 1: 00001        1 -1.9273459 Very Conservative 1071192.34             Disagree
#> 2: 00002        2 -0.5541932      Conservative 4352465.37        Lean Disagree
#> 3: 00003        3 -0.9998992      Conservative   59227.56        Lean Disagree
#> 4: 00004        4 -1.2507135 Very Conservative   61804.34        Lean Disagree
#> 5: 00005        5 -0.1191809      Conservative  153259.82 Don't Know / Neutral
#> 6: 00006        6  0.6139163           Liberal   29196.40           Lean Agree
#>    treatment proposition_vote log_income
#> 1:         1                0   13.88428
#> 2:         0                1   15.28625
#> 3:         0                0   10.98914
#> 4:         1                0   11.03173
#> 5:         1                0   11.93989
#> 6:         0                0   10.28180
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
feols_fit2 <- feols(proposition_vote ~ treatment + ideology + log_income | Q1_immigration, weights = NULL, data = voters)

# 3) bootest based on object of class felm
felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration | 0 |  group_id, weights = NULL, data = voters)
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
boot_fixest = boottest(feols_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE, demean = TRUE)

boot_fixest1 = boottest(feols_fit1, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.05)
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
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.007   0.838    0.413   -0.025    0.011

tidy(boot_lm)
#>               Estimate   t value Pr(>|t|)    CI Lower   CI Upper
#> treatment -0.007398227 0.8383189   0.4126 -0.02509855 0.01060567
```

Change the confidence level:

``` r
boot_lm_5 = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.05)

boot_lm_20 = boottest(lm_fit, clustid = voters$group_id, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.20)

summarize_boot(boot_lm_5)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.007   0.838    0.413   -0.025    0.011
summarize_boot(boot_lm_20)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.007   0.838    0.413   -0.019    0.004

confint(feols_fit, "treatment", level = 0.95, se = "cluster", cluster = "group_id")
#>                 2.5 %     97.5 %
#> treatment -0.02487853 0.01008208
confint(feols_fit, "treatment", level = 0.80, se = "cluster", cluster = "group_id")
#>                  10 %        90 %
#> treatment -0.01882798 0.004031529
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
res <- cluster.boot(lm_fit, cluster = voters$group_id, parallel = TRUE, R = 1000, wild_type = "rademacher")

# 1) results from multiwayvcov
coeftest(lm_fit, res)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)                         0.7488263  0.0606419  12.3483 < 2.2e-16 ***
#> treatment                          -0.0073982  0.0088904  -0.8322 0.4053398    
#> ideology                            0.2736145  0.0129758  21.0866 < 2.2e-16 ***
#> log_income                          0.0015247  0.0030064   0.5072 0.6120571    
#> Q1_immigrationDisagree             -0.2309745  0.0145481 -15.8767 < 2.2e-16 ***
#> Q1_immigrationLean Disagree        -0.3275506  0.0267708 -12.2354 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral -0.2625187  0.0392023  -6.6965 2.249e-11 ***
#> Q1_immigrationLean Agree           -0.1831527  0.0531607  -3.4453 0.0005728 ***
#> Q1_immigrationAgree                -0.3150768  0.0667058  -4.7234 2.351e-06 ***
#> Q1_immigrationStrong Agree         -0.5356336  0.0799848  -6.6967 2.247e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 2) results from fwildclusterboot
summarize_boot(boot_lm)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.007   0.838    0.413   -0.025    0.011
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1   -0.007   0.838    0.412   -0.025    0.011

# 3) sandwich standard errors from fixest
summary(feols_fit, se = "cluster", cluster = "group_id")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id) 
#>             Estimate Std. Error   t value  Pr(>|t|)    
#> treatment  -0.007398   0.008919 -0.829520  0.410833    
#> ideology    0.273615   0.012975 21.088000 < 2.2e-16 ***
#> log_income  0.001525   0.002887  0.528165  0.599769    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -5,210.17   Adj. R2: 0.33527 
#>                           R2-Within: 0.03328
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcov`
(on 4 cores) with - N = 10000 observations - b = 10000 bootstrap
iterations - n\_g = 40 clusters

<img src="C:/Users/alexa/Dropbox/fwildclusterboot/benchmarks/bench_boottest.png" width="50%" />
