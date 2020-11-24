
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

The `fwildclusterboot` package currently only supports one- and
two-dimensional clustering and one-dimensional hypotheses, but allows
for an arbitrary number of fixed effects in the estimation procedure.

The package is highly experimental and currently does not include any
unit tests.

The following will be added in the future: - support for multivariate
hypotheses - bootstrap distributions other then the rademacher
distribution

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
seed <- 942413
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 40, icc1 = 0.01, N_G2 = 40, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10)
head(voters)
#>       ID group_id1 group_id2   ideology1  ideology2 ideological_label
#> 1: 00001        31        17 -2.04507128 -0.3729857 Very Conservative
#> 2: 00002        14        15 -0.67982036 -1.2890736      Conservative
#> 3: 00003        22        11 -0.99053855  1.3178908      Conservative
#> 4: 00004        40         5  0.09852821  1.1891511           Liberal
#> 5: 00005        39        33 -0.85274945 -1.2437729      Conservative
#> 6: 00006        21        38 -0.08314477  1.8352925      Conservative
#>        income Q1_immigration Q2_defence treatment proposition_vote log_income
#> 1: 1353245.43              2          1         0                0   14.11802
#> 2:  374763.17              4          6         0                0   12.83405
#> 3:  172449.61              9          4         1                1   12.05786
#> 4:   38931.96              8          8         0                0   10.56957
#> 5:  216052.39              1          5         0                0   12.28328
#> 6:   28985.85              7         10         0                1   10.27456
```

The `fwildclusterboot` package supports estimation of linear models
based on - `lm()` from `base` R - `felm()` from `lfe` - `feols()` from
`fixest`

``` r
library(lfe)
library(fixest)
library(sandwich)
library(multiwayvcov)

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

boot_fixest1 = boottest(feols_fit1, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0, alpha = 0.05)
# boot_fixest2 = boottest(feols_fit2, clustid = voters$group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE, beta = 0)

# 3) boottest based on object of class felm
boot_felm = boottest(felm_fit, clustid = ~ group_id1, B = B, seed = seed, param = "treatment", conf_int = TRUE)
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
#> treatment   -0.011   1.176    0.255   -0.029    0.008

tidy(boot_lm)
#>              Estimate  t value Pr(>|t|)    CI Lower    CI Upper
#> treatment -0.01056342 1.176082   0.2553 -0.02860011 0.008111164
tidy(boot_fixest)
#>              Estimate  t value Pr(>|t|)    CI Lower    CI Upper
#> treatment -0.01056342 1.166462    0.259 -0.02859596 0.008063582
tidy(boot_felm)
#>      Estimate  t value Pr(>|t|)    CI Lower    CI Upper
#> 1 -0.01056342 1.166462    0.259 -0.02859604 0.008063247
tidy(boot_fixest1)
#>              Estimate  t value Pr(>|t|)    CI Lower    CI Upper
#> treatment -0.01056342 1.176082   0.2553 -0.02859894 0.008112148
tidy(boot_felm1)
#>      Estimate  t value Pr(>|t|)    CI Lower    CI Upper
#> 1 -0.01056342 1.176082   0.2553 -0.02859873 0.008110625
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
#> treatment   -0.011   1.176    0.255   -0.029    0.008
summarize_boot(boot_lm_20)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.011   1.176    0.255   -0.022    0.001

confint(feols_fit, "treatment", level = 0.95, se = "cluster", cluster = "group_id1")
#>                 2.5 %      97.5 %
#> treatment -0.02840255 0.007275703
confint(feols_fit, "treatment", level = 0.80, se = "cluster", cluster = "group_id1")
#>                 10 %        90 %
#> treatment -0.0222278 0.001100954
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

res <- cluster.boot(lm_fit, cluster = ~ group_id1, parallel = FALSE, R = 1000, wild_type = "rademacher")

# 1) results from multiwayvcov
coeftest(lm_fit, res)
#> 
#> t test of coefficients:
#> 
#>                     Estimate  Std. Error t value Pr(>|t|)    
#> (Intercept)       0.50387896  0.04100116 12.2894  < 2e-16 ***
#> treatment        -0.01056342  0.00901476 -1.1718  0.24131    
#> ideology1         0.23720178  0.00570985 41.5425  < 2e-16 ***
#> log_income        0.00063845  0.00345343  0.1849  0.85333    
#> Q1_immigration2  -0.01425349  0.01949950 -0.7310  0.46482    
#> Q1_immigration3   0.00174009  0.01660040  0.1048  0.91652    
#> Q1_immigration4  -0.02241118  0.01836469 -1.2203  0.22236    
#> Q1_immigration5   0.02906099  0.02064217  1.4078  0.15921    
#> Q1_immigration6  -0.00755032  0.01980571 -0.3812  0.70305    
#> Q1_immigration7  -0.02085593  0.02053917 -1.0154  0.30993    
#> Q1_immigration8   0.04190760  0.02196325  1.9081  0.05641 .  
#> Q1_immigration9   0.00385260  0.01820728  0.2116  0.83243    
#> Q1_immigration10 -0.02188604  0.02262187 -0.9675  0.33333    
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
#> treatment   -0.011   1.176    0.255   -0.029    0.008
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.011   1.166    0.259   -0.029    0.008


# 3) sandwich standard errors 
vocv <- vcovCL(lm_fit, ~ group_id1)
coeftest(lm_fit, vcov)
#> 
#> t test of coefficients:
#> 
#>                     Estimate  Std. Error t value Pr(>|t|)    
#> (Intercept)       0.50387896  0.03914933 12.8707  < 2e-16 ***
#> treatment        -0.01056342  0.00879478 -1.2011  0.22974    
#> ideology1         0.23720178  0.00565635 41.9355  < 2e-16 ***
#> log_income        0.00063845  0.00324763  0.1966  0.84415    
#> Q1_immigration2  -0.01425349  0.01988927 -0.7166  0.47361    
#> Q1_immigration3   0.00174009  0.01951449  0.0892  0.92895    
#> Q1_immigration4  -0.02241118  0.01973081 -1.1358  0.25605    
#> Q1_immigration5   0.02906099  0.01968756  1.4761  0.13995    
#> Q1_immigration6  -0.00755032  0.01966993 -0.3839  0.70110    
#> Q1_immigration7  -0.02085593  0.01969939 -1.0587  0.28976    
#> Q1_immigration8   0.04190760  0.01982859  2.1135  0.03458 *  
#> Q1_immigration9   0.00385260  0.01965981  0.1960  0.84464    
#> Q1_immigration10 -0.02188604  0.01977590 -1.1067  0.26845    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
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
 boot_felm <-  boottest(felm_fit, clustid = ~group_id1 + group_id2, B = 1000, seed = seed, param = "treatment", conf_int = FALSE)
#> Warning in Ops.factor(treatment + ideology1 + log_income, Q1_immigration): '|'
#> not meaningful for factors
#> Warning in preprocess.felm(object = object, param = param, clustid = clustid, :
#> You are estimating a model with more than 200 clusters. Are you sure you want to
#> proceed with bootstrap standard errors instead of asymptotic sandwich standard
#> errors? The more clusters in the data, the longer the estimation process.

summarize_boot(boot_lm)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#>  Number of Clusters:  40
#>  Number of Clusters:  1598
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.011  47.688    0.215       NA       NA
summarize_boot(boot_fixest)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#>  Number of Clusters:  40
#>  Number of Clusters:  1598
#> 
#>           Estimate t value Pr(>|t|) CI Lower CI Upper
#> treatment   -0.011  47.685    0.215       NA       NA
summarize_boot(boot_felm)
#>  
#>   Estimation Function: NULL
#>  Observations:10000
#>  Standard-errors: Clustered  
#>  Number of Clusters:  40
#>  Number of Clusters:  40
#>  Number of Clusters:  1598
#> 
#>   Estimate t value Pr(>|t|) CI Lower CI Upper
#> 1   -0.011  47.685    0.215       NA       NA

vocv <- vcovCL(lm_fit, ~ group_id1 + group_id2)
coeftest(lm_fit, vcov)
#> 
#> t test of coefficients:
#> 
#>                     Estimate  Std. Error t value Pr(>|t|)    
#> (Intercept)       0.50387896  0.03914933 12.8707  < 2e-16 ***
#> treatment        -0.01056342  0.00879478 -1.2011  0.22974    
#> ideology1         0.23720178  0.00565635 41.9355  < 2e-16 ***
#> log_income        0.00063845  0.00324763  0.1966  0.84415    
#> Q1_immigration2  -0.01425349  0.01988927 -0.7166  0.47361    
#> Q1_immigration3   0.00174009  0.01951449  0.0892  0.92895    
#> Q1_immigration4  -0.02241118  0.01973081 -1.1358  0.25605    
#> Q1_immigration5   0.02906099  0.01968756  1.4761  0.13995    
#> Q1_immigration6  -0.00755032  0.01966993 -0.3839  0.70110    
#> Q1_immigration7  -0.02085593  0.01969939 -1.0587  0.28976    
#> Q1_immigration8   0.04190760  0.01982859  2.1135  0.03458 *  
#> Q1_immigration9   0.00385260  0.01965981  0.1960  0.84464    
#> Q1_immigration10 -0.02188604  0.01977590 -1.1067  0.26845    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

res <- cluster.boot(lm_fit, cluster = ~group_id1 + group_id2, R = 1000, wild_type = "rademacher", parallel = FALSE)
# # 1) results from multiwayvcov

coeftest(lm_fit, res)
#> Warning in sqrt(diag(se)): NaNs produced
#> 
#> t test of coefficients:
#> 
#>                     Estimate  Std. Error t value  Pr(>|t|)    
#> (Intercept)       0.50387896  0.02249069 22.4039 < 2.2e-16 ***
#> treatment        -0.01056342  0.00165195 -6.3945 1.682e-10 ***
#> ideology1         0.23720178  0.00360040 65.8821 < 2.2e-16 ***
#> log_income        0.00063845  0.00166455  0.3836   0.70131    
#> Q1_immigration2  -0.01425349  0.00280925 -5.0738 3.970e-07 ***
#> Q1_immigration3   0.00174009  0.00261784  0.6647   0.50626    
#> Q1_immigration4  -0.02241118  0.01513372 -1.4809   0.13867    
#> Q1_immigration5   0.02906099  0.00500615  5.8051 6.631e-09 ***
#> Q1_immigration6  -0.00755032          NA      NA        NA    
#> Q1_immigration7  -0.02085593  0.01496348 -1.3938   0.16341    
#> Q1_immigration8   0.04190760          NA      NA        NA    
#> Q1_immigration9   0.00385260  0.00817263  0.4714   0.63736    
#> Q1_immigration10 -0.02188604  0.01171771 -1.8678   0.06182 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#confint(feols_fit, "treatment", level = 0.95, se = "twoway", cluster = c("group_id1", "group_id2"))
```

## Benchmark

Results of timing benchmarks of `fwildclusterboot` with `multiwayvcov`
(on 4 cores) with - N = 10000 observations - b = 10000 bootstrap
iterations - n\_g = 50 clusters
