
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
voters <- create_data_2(N = 10000, N_G1 = 40, icc1 = 0.01, N_G2 = 50, icc2 = 0.01)
head(voters)
#>       ID group_id1 group_id2  ideology1  ideology2 ideological_label    income
#> 1: 00001        16         2  0.1197102  1.0923647           Liberal 165814.96
#> 2: 00002         6        35 -0.1062267  1.5309846      Conservative 249251.16
#> 3: 00003        29         4  1.4691768 -1.3218695      Very Liberal  37540.53
#> 4: 00004        26        42  0.8595596 -0.8070506           Liberal  10950.59
#> 5: 00005        17        30 -0.9375545  1.2895105      Conservative 113074.26
#> 6: 00006        23        32  0.4222435 -0.6962489           Liberal  40426.99
#>    Q1_immigration treatment proposition_vote log_income
#> 1:     Lean Agree         0                1  12.018628
#> 2:          Agree         0                1  12.426216
#> 3:  Lean Disagree         1                1  10.533176
#> 4:  Lean Disagree         1                1   9.301149
#> 5:     Lean Agree         1                1  11.635800
#> 6:  Lean Disagree         1                0  10.607253
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
#> treatment    0.002   0.191    0.845   -0.015    0.018

tidy(boot_lm)
#>           Estimate  t value Pr(>|t|)   CI Lower   CI Upper
#> treatment 0.001532 0.191164   0.8447 -0.0147164 0.01778489
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
#> treatment    0.002   0.191    0.845   -0.015    0.018
summarize_boot(boot_lm_20)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.002   0.191    0.845   -0.009    0.012

confint(feols_fit, "treatment", level = 0.95, se = "cluster", cluster = "group_id1")
#>                 2.5 %     97.5 %
#> treatment -0.01438254 0.01744654
confint(feols_fit, "treatment", level = 0.80, se = "cluster", cluster = "group_id1")
#>                   10 %       90 %
#> treatment -0.008873959 0.01193796
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
#> (Intercept)                        -0.0294851  0.0440658 -0.6691  0.503437    
#> treatment                           0.0015320  0.0076677  0.1998  0.841641    
#> ideology1                           0.2296782  0.0054249 42.3381 < 2.2e-16 ***
#> log_income                          0.0016967  0.0034879  0.4864  0.626660    
#> Q1_immigrationDisagree              0.0916079  0.0355488  2.5770  0.009982 ** 
#> Q1_immigrationLean Disagree         0.2568469  0.0328651  7.8152 6.041e-15 ***
#> Q1_immigrationDon't Know / Neutral  0.5090462  0.0346585 14.6875 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.7538540  0.0338502 22.2703 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.9152413  0.0354173 25.8417 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9725943  0.0580294 16.7604 < 2.2e-16 ***
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
#> treatment    0.002   0.191    0.845   -0.015    0.018
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.002   0.191    0.845   -0.015    0.018

# 3) sandwich standard errors from fixest
summary(feols_fit, se = "cluster", cluster = "group_id1")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id1) 
#>            Estimate Std. Error   t value  Pr(>|t|)    
#> treatment  0.001532   0.008120  0.188674  0.851326    
#> ideology1  0.229678   0.005440 42.223000 < 2.2e-16 ***
#> log_income 0.001697   0.003396  0.499553  0.620197    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -4,438.04   Adj. R2: 0.43053 
#>                           R2-Within: 0.26744
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
#>  Number of Clusters:  1994
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.002   8.414    0.853       NA       NA
summarize_boot(boot_fixest)
#> Warning in min(object$conf_int): no non-missing arguments to min; returning Inf
#> Warning in max(object$conf_int): no non-missing arguments to max; returning -Inf
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  1994
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment    0.002   8.414    0.853      Inf     -Inf
summarize_boot(boot_felm)
#> Warning in min(object$conf_int): no non-missing arguments to min; returning Inf

#> Warning in min(object$conf_int): no non-missing arguments to max; returning -Inf
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  1994
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1    0.002   8.414    0.853      Inf     -Inf

res <- cluster.boot(lm_fit, cluster = ~group_id1 + group_id2, R = 1000, wild_type = "rademacher", parallel = TRUE)
# # 1) results from multiwayvcov

coeftest(lm_fit, res)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                        -0.0294851  0.0200918 -1.4675 0.1422654    
#> treatment                           0.0015320  0.0052020  0.2945 0.7683818    
#> ideology1                           0.2296782  0.0034128 67.2987 < 2.2e-16 ***
#> log_income                          0.0016967  0.0022064  0.7690 0.4419265    
#> Q1_immigrationDisagree              0.0916079  0.0266235  3.4409 0.0005822 ***
#> Q1_immigrationLean Disagree         0.2568469  0.0255692 10.0452 < 2.2e-16 ***
#> Q1_immigrationDon't Know / Neutral  0.5090462  0.0242219 21.0160 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.7538540  0.0257416 29.2854 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.9152413  0.0246152 37.1819 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9725943  0.0356832 27.2564 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

vocv <- vcovCL(lm_fit, ~ group_id1 + group_id2)
coeftest(lm_fit, vcov)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                        -0.0294851  0.0570938 -0.5164    0.6056    
#> treatment                           0.0015320  0.0075480  0.2030    0.8392    
#> ideology1                           0.2296782  0.0048984 46.8883 < 2.2e-16 ***
#> log_income                          0.0016967  0.0027677  0.6130    0.5399    
#> Q1_immigrationDisagree              0.0916079  0.0498423  1.8380    0.0661 .  
#> Q1_immigrationLean Disagree         0.2568469  0.0481599  5.3332  9.86e-08 ***
#> Q1_immigrationDon't Know / Neutral  0.5090462  0.0479436 10.6176 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.7538540  0.0481611 15.6527 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.9152413  0.0498077 18.3755 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9725943  0.0657599 14.7901 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
confint(feols_fit, "treatment", level = 0.95, se = "twoway", cluster = c("group_id1", "group_id2"))
#>                 2.5 %     97.5 %
#> treatment -0.01417262 0.01723662
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcov`
(on 4 cores) with - N = 10000 observations - b = 10000 bootstrap
iterations - n\_g = 50 clusters
