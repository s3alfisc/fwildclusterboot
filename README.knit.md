---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# fwildclusterboot

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
<!-- badges: end -->

The `fwildclusterboot` package is an R port of Stata's `boottest` package. 

It implements the fast wild cluster bootstrap algorithm developed in Roodman et al (2019) for regression objects in R. It currently works for regression objects of type `lm`, `felm` and `fixest` from base R and the `lfe` and `fixest` packages. 

The package's central function is `boottest()`. It allows the user to test two-sided, univariate hypotheses using a wild cluster bootstrap. Importantly, it uses the "fast" algorithm developed in Roodman et al, which makes it feasible to calculate test statistics based on a large number of bootstrap draws even for large samples--as long as the number of bootstrapping clusters is not too large.

The `fwildclusterboot` package currently only supports one- and two-dimensional clustering and one-dimensional hypotheses, but allows for an arbitrary number of fixed effects in the estimation procedure.  

The package is highly experimental and currently does not include any unit tests.

The following will be added in the future: 
- support for multivariate hypotheses 
- bootstrap distributions other then the rademacher distribution

## Installation

You can install the released version of `fwildclusterboot` from github by running 


```r
library(devtools)
#> Loading required package: usethis
#install_github("al_fisc/fwildclusterboot")
```






## The `boottest()` function 

In a first step, simulate a data set with 10000 individual observations that are grouped in 50 clusters and with a small intra-cluster correlation of 0.01. The small intra-cluster correlation implies that, in theory, inference based on cluster-robust covariance estimates should lead to results that are very similar to the bootstrap. 


```r
library(fwildclusterboot)


B <- 10000
seed <- 942413
set.seed(seed)
voters <- create_data_2(N = 10000, N_G1 = 20, icc1 = 0.01, N_G2 = 40, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = seed)
head(voters)
#>       ID group_id1 group_id2   ideology1    ideology2 ideological_label
#> 1: 00001        14        30 -0.26420113 -0.939580003      Conservative
#> 2: 00002        20         8  0.05624777 -0.389350411           Liberal
#> 3: 00003        12        28  0.12515743  0.014351521           Liberal
#> 4: 00004         8        11 -0.98861768  1.344363457      Conservative
#> 5: 00005         7        18 -1.15088371  0.006652709 Very Conservative
#> 6: 00006        15        33  0.63456200  0.100307845           Liberal
#>       income Q1_immigration Q2_defence treatment proposition_vote log_income
#> 1: 261463.73              4          5         1                1  12.474051
#> 2:  34106.58              8          2         0                1  10.437245
#> 3: 120119.41              5          4         0                1  11.696242
#> 4: 456144.57              1          3         1                1  13.030565
#> 5: 108940.29              8          2         0                0  11.598555
#> 6:  12517.04             10          8         1                0   9.434846

data.table::fwrite(voters, "voters.csv")
```

The `fwildclusterboot` package supports estimation of linear models based on 
- `lm()` from `base` R
- `felm()` from `lfe`
- `feols()` from `fixest`


```r
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

The `boottest` function always calculates p-values for a given univariate hypothesis test.
Second and by default, the boottest function calculates confidence intervals by inversion of the p-value. The user can considerably speed up the inference procedure by setting the argument `conf_int = FALSE`, in which case no confidence intervals are computed.

















