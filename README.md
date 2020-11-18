
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
seed <- 424
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 40, icc1 = 0.01, N_G2 = 40, icc2 = 0.01)
head(voters)
#>       ID group_id1 group_id2  ideology1   ideology2 ideological_label
#> 1: 00001        16         2  0.2373664  1.11626703           Liberal
#> 2: 00002         6        35  0.7117321  1.50861755           Liberal
#> 3: 00003        29         4  0.7717061  0.06122789           Liberal
#> 4: 00004        26        30 -1.2255063 -1.15157327 Very Conservative
#> 5: 00005        17        32 -0.5452784 -0.30425149      Conservative
#> 6: 00006        23        25 -1.3032564 -1.40352322 Very Conservative
#>        income       Q1_immigration treatment proposition_vote log_income
#> 1:  111686.88           Lean Agree         1                1  11.623455
#> 2:   70011.06                Agree         0                1  11.156409
#> 3:   13593.90 Don't Know / Neutral         0                1   9.517377
#> 4:  160943.76        Lean Disagree         0                0  11.988810
#> 5:  895801.52 Don't Know / Neutral         0                1  13.705474
#> 6: 1495359.92        Lean Disagree         0                0  14.217877
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
boot_lm = boottest(lm_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)

# 2) bootest based on object of class feols
boot_fixest = boottest(feols_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)
boot_fixest = boottest(feols_fit, clustid = ~group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, demean = TRUE)

boot_fixest1 = boottest(feols_fit1, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.05)
# boot_fixest2 = boottest(feols_fit2, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0)

# 3) boottest based on object of class felm
boot_felm = boottest(felm_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)
#boot_felm = boottest(felm_fit, clustid = ~ group_id1, B = B, seed = seed, param = #"treatment", conf_int = TRUE)


boot_felm1 = boottest(felm_fit1, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)
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
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    -0.01   1.545    0.136   -0.024    0.003

tidy(boot_lm)
#>              Estimate  t value Pr(>|t|)   CI Lower    CI Upper
#> treatment -0.01027798 1.545212   0.1363 -0.0236955 0.003416845
```

Change the confidence level:

``` r
boot_lm_5 = boottest(lm_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.05)

boot_lm_20 = boottest(lm_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.20)

summarize_boot(boot_lm_5)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    -0.01   1.545    0.136   -0.024    0.003
summarize_boot(boot_lm_20)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    -0.01   1.545    0.136   -0.019   -0.001

confint(feols_fit, "treatment", level = 0.95, se = "cluster", cluster = "group_id1")
#>                 2.5 %      97.5 %
#> treatment -0.02348671 0.002930745
confint(feols_fit, "treatment", level = 0.80, se = "cluster", cluster = "group_id1")
#>                 10 %         90 %
#> treatment -0.0189147 -0.001641259
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
res <- cluster.boot(lm_fit, cluster = ~ group_id1, parallel = TRUE, R = 1000, wild_type = "rademacher")

# 1) results from multiwayvcov
coeftest(lm_fit, res)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                         0.0326410  0.0360644  0.9051  0.365448    
#> treatment                          -0.0102780  0.0064957 -1.5823  0.113619    
#> ideology1                           0.2289206  0.0053191 43.0376 < 2.2e-16 ***
#> log_income                         -0.0014688  0.0025447 -0.5772  0.563807    
#> Q1_immigrationDisagree              0.0894429  0.0290703  3.0768  0.002098 ** 
#> Q1_immigrationLean Disagree         0.2501223  0.0288902  8.6577 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral  0.4854261  0.0272656 17.8036 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.7261462  0.0251573 28.8643 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.9186321  0.0267120 34.3902 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9721868  0.0304312 31.9471 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 2) results from fwildclusterboot
summarize_boot(boot_lm)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    -0.01   1.545    0.136   -0.024    0.003
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    -0.01   1.545    0.136   -0.024    0.003

# 3) sandwich standard errors from fixest
summary(feols_fit, se = "cluster", cluster = "group_id1")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id1) 
#>             Estimate Std. Error   t value  Pr(>|t|)    
#> treatment  -0.010278   0.006739 -1.525100  0.135306    
#> ideology1   0.228921   0.005343 42.848000 < 2.2e-16 ***
#> log_income -0.001469   0.002673 -0.549447  0.585831    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -4,490.24   Adj. R2: 0.42446 
#>                           R2-Within: 0.27262
```

## Some Tests with 2-way clustering

``` r
library(sandwich)
library(lmtest)
library(multiwayvcov)
options(boot.ncpus = 4)
rm(boot_lm); rm(boot_fixest); rm(boot_felm); rm(res)

boot_lm <-  boottest(lm_fit, clustid = ~group_id1 + group_id2, B = 1000, seed = seed, param = "treatment", conf_int = FALSE)
#> Warning in preprocess.lm(object = object, param = param, clustid = clustid, :
#> You are estimating a model with more than 200 clusters. Are you sure you want to
#> proceed with bootstrap standard errors instead of asymptotic sandwich standard
#> errors? The more clusters in the data, the longer the estimation process.
boot_fixest <-  boottest(feols_fit, clustid = ~group_id1 + group_id2, B = 1000, seed = seed, param = "treatment", conf_int = FALSE)
#> Warning in preprocess.fixest(object = object, param = param, clustid =
#> clustid, : You are estimating a model with more than 200 clusters. Are you
#> sure you want to proceed with bootstrap standard errors instead of asymptotic
#> sandwich standard errors? The more clusters in the data, the longer the
#> estimation process.
boot_felm <-  boottest(felm_fit, clustid = ~group_id1 + group_id2, B = 1000, seed = seed, param = "treatment", conf_int = FALSE)
#> Warning in Ops.factor(treatment + ideology1 + log_income, Q1_immigration): '|'
#> not meaningful for factors
#> Warning in preprocess.felm(object = object, param = param, clustid = clustid, :
#> You are estimating a model with more than 200 clusters. Are you sure you want to
#> proceed with bootstrap standard errors instead of asymptotic sandwich standard
#> errors? The more clusters in the data, the longer the estimation process.

#> Warning in preprocess.felm(object = object, param = param, clustid = clustid, :
#> You are estimating a model with more than 200 clusters. Are you sure you want to
#> proceed with bootstrap standard errors instead of asymptotic sandwich standard
#> errors? The more clusters in the data, the longer the estimation process.
summarize_boot(boot_lm)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  1593
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    -0.01  59.319    0.177       NA       NA
summarize_boot(boot_fixest)
#> Warning in min(object$conf_int): no non-missing arguments to min; returning Inf
#> Warning in max(object$conf_int): no non-missing arguments to max; returning -Inf
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  1593
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    -0.01  59.319    0.177      Inf     -Inf
summarize_boot(boot_felm)
#> Warning in min(object$conf_int): no non-missing arguments to min; returning Inf

#> Warning in min(object$conf_int): no non-missing arguments to max; returning -Inf
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  1593
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1    -0.01  59.319    0.177      Inf     -Inf

res <- cluster.boot(lm_fit, cluster = ~group_id1 + group_id2, R = 1000, wild_type = "rademacher", parallel = TRUE)
# # 1) results from multiwayvcov

coeftest(lm_fit, res)
#> Warning in sqrt(diag(se)): NaNs produced
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)                         0.0326410         NA       NA        NA    
#> treatment                          -0.0102780  0.0027535  -3.7327 0.0001905 ***
#> ideology1                           0.2289206  0.0025622  89.3442 < 2.2e-16 ***
#> log_income                         -0.0014688  0.0013198  -1.1129 0.2657628    
#> Q1_immigrationDisagree              0.0894429  0.0171194   5.2247  1.78e-07 ***
#> Q1_immigrationLean Disagree         0.2501223  0.0186605  13.4039 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral  0.4854261  0.0077154  62.9169 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.7261462  0.0054067 134.3041 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.9186321  0.0077272 118.8823 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9721868  0.0115552  84.1340 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

vocv <- vcovCL(lm_fit, ~ group_id1 + group_id2)
coeftest(lm_fit, vcov)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                         0.0326410  0.0575787  0.5669    0.5708    
#> treatment                          -0.0102780  0.0075893 -1.3543    0.1757    
#> ideology1                           0.2289206  0.0048950 46.7666 < 2.2e-16 ***
#> log_income                         -0.0014688  0.0027989 -0.5248    0.5997    
#> Q1_immigrationDisagree              0.0894429  0.0502652  1.7794    0.0752 .  
#> Q1_immigrationLean Disagree         0.2501223  0.0484329  5.1643  2.46e-07 ***
#> Q1_immigrationDon't Know / Neutral  0.4854261  0.0482006 10.0710 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.7261462  0.0484178 14.9975 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.9186321  0.0500019 18.3719 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9721868  0.0661064 14.7064 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
confint(feols_fit, "treatment", level = 0.95, se = "twoway", cluster = c("group_id1", "group_id2"))
#>                 2.5 %      97.5 %
#> treatment -0.02484579 0.004289832
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcov`
(on 4 cores) with - N = 10000 observations - b = 10000 bootstrap
iterations - n\_g = 50 clusters
