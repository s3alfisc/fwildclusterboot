
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
seed <- 9123949
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 50, icc1 = 0.01, N_G2 = 50, icc2 = 0.01)
head(voters)
#>       ID group_id1 group_id2    ideology1   ideology2 ideological_label
#> 1: 00001        22         7 -0.020461690 -0.44845287      Conservative
#> 2: 00002        30        22 -0.686944986  1.21979398      Conservative
#> 3: 00003        12        22  0.153266263 -0.30139018           Liberal
#> 4: 00004        47         5  0.587644662  0.75471814           Liberal
#> 5: 00005         1        30 -1.041845529 -0.01226449 Very Conservative
#> 6: 00006        15        32 -0.003864857 -1.66748386      Conservative
#>        income       Q1_immigration treatment proposition_vote log_income
#> 1:  341106.76 Don't Know / Neutral         0                0  12.739951
#> 2:   80723.27           Lean Agree         0                0  11.298782
#> 3:   20252.26 Don't Know / Neutral         0                1   9.916022
#> 4:   10323.55           Lean Agree         0                1   9.242183
#> 5: 1157778.09 Don't Know / Neutral         0                0  13.962013
#> 6:  160847.88             Disagree         1                0  11.988214
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
boot_fixest = boottest(feols_fit, clustid = ~group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, demean = TRUE)

boot_fixest1 = boottest(feols_fit1, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.05)
# boot_fixest2 = boottest(feols_fit2, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0)

# 3) boottest based on object of class felm
boot_felm = boottest(felm_fit, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)
#boot_felm = boottest(felm_fit, clustid = ~ group_id1, B = B, seed = seed, param = #"treatment", conf_int = TRUE)


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
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.006   0.969    0.333    -0.02    0.007

tidy(boot_lm)
#>               Estimate   t value Pr(>|t|)    CI Lower    CI Upper
#> treatment -0.006355802 0.9693211   0.3334 -0.01953921 0.006849275
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
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.006   0.969    0.333    -0.02    0.007
summarize_boot(boot_lm_20)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.006   0.969    0.333   -0.015    0.002

confint(feols_fit, "treatment", level = 0.95, se = "cluster", cluster = "group_id1")
#>                 2.5 %     97.5 %
#> treatment -0.01934353 0.00663193
confint(feols_fit, "treatment", level = 0.80, se = "cluster", cluster = "group_id1")
#>                  10 %        90 %
#> treatment -0.01484802 0.002136419
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
#> (Intercept)                         0.0701714  0.0428857  1.6362   0.10182    
#> treatment                          -0.0063558  0.0065486 -0.9706   0.33179    
#> ideology1                           0.2303478  0.0040293 57.1681 < 2.2e-16 ***
#> log_income                         -0.0013794  0.0025157 -0.5483   0.58349    
#> Q1_immigrationDisagree              0.0709486  0.0387969  1.8287   0.06747 .  
#> Q1_immigrationLean Disagree         0.2202329  0.0356895  6.1708 7.058e-10 ***
#> Q1_immigrationDon't Know / Neutral  0.4408495  0.0358561 12.2950 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.6835924  0.0353473 19.3393 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.8534074  0.0367588 23.2164 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9154456  0.0470317 19.4644 < 2.2e-16 ***
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
#> treatment   -0.006   0.969    0.333    -0.02    0.007
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  50
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.006   0.969    0.333    -0.02    0.007

# 3) sandwich standard errors from fixest
summary(feols_fit, se = "cluster", cluster = "group_id1")
#> OLS estimation, Dep. Var.: proposition_vote
#> Observations: 10,000 
#> Fixed-effects: Q1_immigration: 7
#> Standard-errors: Clustered (group_id1) 
#>             Estimate Std. Error   t value  Pr(>|t|)    
#> treatment  -0.006356   0.006627 -0.959147  0.342194    
#> ideology1   0.230348   0.004116 55.958000 < 2.2e-16 ***
#> log_income -0.001379   0.002546 -0.541736  0.590455    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log-likelihood: -4,602.75   Adj. R2: 0.41144 
#>                           R2-Within: 0.27457
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
#>  Number of Clusters:  2449
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.006  40.709    0.339       NA       NA
summarize_boot(boot_fixest)
#> Warning in min(object$conf_int): no non-missing arguments to min; returning Inf
#> Warning in max(object$conf_int): no non-missing arguments to max; returning -Inf
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  2449
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.006  40.709    0.339      Inf     -Inf
summarize_boot(boot_felm)
#> Warning in min(object$conf_int): no non-missing arguments to min; returning Inf

#> Warning in min(object$conf_int): no non-missing arguments to max; returning -Inf
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  2449
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1   -0.006  40.709    0.339      Inf     -Inf

res <- cluster.boot(lm_fit, cluster = ~group_id1 + group_id2, R = 1000, wild_type = "rademacher", parallel = TRUE)
# # 1) results from multiwayvcov

coeftest(lm_fit, res)
#> Warning in sqrt(diag(se)): NaNs produced
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)                         0.0701714  0.0028169 24.9105  < 2e-16 ***
#> treatment                          -0.0063558         NA      NA       NA    
#> ideology1                           0.2303478         NA      NA       NA    
#> log_income                         -0.0013794         NA      NA       NA    
#> Q1_immigrationDisagree              0.0709486  0.0249632  2.8421  0.00449 ** 
#> Q1_immigrationLean Disagree         0.2202329  0.0240692  9.1500  < 2e-16 ***
#> Q1_immigrationDon't Know / Neutral  0.4408495  0.0276537 15.9418  < 2e-16 ***
#> Q1_immigrationLean Agree            0.6835924  0.0234277 29.1788  < 2e-16 ***
#> Q1_immigrationAgree                 0.8534074  0.0258691 32.9895  < 2e-16 ***
#> Q1_immigrationStrong Agree          0.9154456  0.0442934 20.6678  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

vocv <- vcovCL(lm_fit, ~ group_id1 + group_id2)
coeftest(lm_fit, vcov)
#> 
#> t test of coefficients:
#> 
#>                                      Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                         0.0701714  0.0564245  1.2436    0.2137    
#> treatment                          -0.0063558  0.0076781 -0.8278    0.4078    
#> ideology1                           0.2303478  0.0049376 46.6522 < 2.2e-16 ***
#> log_income                         -0.0013794  0.0028365 -0.4863    0.6268    
#> Q1_immigrationDisagree              0.0709486  0.0486521  1.4583    0.1448    
#> Q1_immigrationLean Disagree         0.2202329  0.0468309  4.7027 2.602e-06 ***
#> Q1_immigrationDon't Know / Neutral  0.4408495  0.0466038  9.4595 < 2.2e-16 ***
#> Q1_immigrationLean Agree            0.6835924  0.0468512 14.5907 < 2.2e-16 ***
#> Q1_immigrationAgree                 0.8534074  0.0486574 17.5391 < 2.2e-16 ***
#> Q1_immigrationStrong Agree          0.9154456  0.0680477 13.4530 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
confint(feols_fit, "treatment", level = 0.95, se = "twoway", cluster = c("group_id1", "group_id2"))
#>                 2.5 %      97.5 %
#> treatment -0.01745633 0.004744721
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcov`
(on 4 cores) with - N = 10000 observations - b = 10000 bootstrap
iterations - n\_g = 50 clusters
