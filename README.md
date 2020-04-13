
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwildclusterboot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
<!-- badges: end -->

The goal of fwildclusterboot is to estimate a fast wild cluster
bootstrap.

## Installation

You can install the released version of fwildclusterboot from github…

``` r
install.packages("fwildclusterboot")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#setwd("C:/Users/au563468/Dropbox")
#devtools::install("fwildclusterboot")
library(fwildclusterboot)

library(data.table)
#> Warning: package 'data.table' was built under R version 3.6.3
library(estimatr)
#> Warning: package 'estimatr' was built under R version 3.6.3
library(magrittr)
#> Warning: package 'magrittr' was built under R version 3.6.3
library(mvtnorm)
library(multiwayvcov)
#> Warning: package 'multiwayvcov' was built under R version 3.6.3
library(lmtest)
#> Warning: package 'lmtest' was built under R version 3.6.3
#> Loading required package: zoo
#> Warning: package 'zoo' was built under R version 3.6.3
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
library(lfe)
#> Warning: package 'lfe' was built under R version 3.6.3
#> Loading required package: Matrix
#> 
#> Attaching package: 'lfe'
#> The following object is masked from 'package:lmtest':
#> 
#>     waldtest
## basic example code

seed = sample(1:1000, 1)
 
gen_cluster <- function(param = c(1, 0), n = 10000, n_cluster = 20, rho = .8) {
 # source: https://yukiyanai.github.io/teaching/rm1/contents/R/clustered-data-analysis.html
 # Function to generate clustered data
 # Required package: mvtnorm
 
 # individual level
 Sigma_i <- matrix(c(1, 0, 0, 1 - rho), ncol = 2)
 values_i <- rmvnorm(n = n, sigma = Sigma_i)
 
 # cluster level
 cluster_name <- rep(1:n_cluster, each = n / n_cluster)
 Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
 values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
 
 # predictor var consists of individual- and cluster-level components
 x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
 
 # error consists of individual- and cluster-level components
 error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
 
 # data generating process
 y <- param[1] + param[2]*x + error
 
 df <- data.frame(x, y, cluster = cluster_name)
 data.table::setDT(df)
 return(df)
}
# 
data <- gen_cluster()
#head(data)
#data[, mean(y)]

 
lm_fit <- lm(y ~ x, data = data)
lm_fit %>% 
 summary()
#> 
#> Call:
#> lm(formula = y ~ x, data = data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.9833 -0.7429 -0.0272  0.6616  3.3676 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.960504   0.010618  90.462  < 2e-16 ***
#> x           -0.040790   0.007862  -5.188 2.16e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.06 on 9998 degrees of freedom
#> Multiple R-squared:  0.002685,   Adjusted R-squared:  0.002585 
#> F-statistic: 26.92 on 1 and 9998 DF,  p-value: 2.164e-07
 
# standard bootstrap
B <- 1000
 
# basic bootstrap, not parallel
system.time(boot_fit <- multiwayvcov::cluster.boot(lm_fit, 
                           as.factor(data$cluster), 
                           R = B, 
                           boot_type = "residual", 
                           wild_type = "rademacher", 
                           parallel = TRUE))
#>    user  system elapsed 
#>    7.47    0.16    9.64
 
 
# system.time(
#   boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
# )
```

And let’s have a look at the output:

``` r
# lmtest::coeftest(lm_fit, boot_fit)
# 
# boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)")
# boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
# 
# lm_robust_fit <- lm_robust(y ~ x, data = data, clusters = cluster)
# lm_robust_fit %>% 
#  summary()
# # does the method work with object lm_robust?
# #boottest.lm_robust(lm_robust_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)") # error: need to feed in data
# boottest.lm_robust(lm_robust_fit, data = data, clustid = data$cluster, B = B, seed = seed, param = "x") 
```
