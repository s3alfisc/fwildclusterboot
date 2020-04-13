
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

library(fwildclusterboot)
#> 
#> Attaching package: 'fwildclusterboot'
#> The following objects are masked _by_ '.GlobalEnv':
#> 
#>     boottest.lm, boottest.lm_robust

## basic example code

seed <- sample(1:1000, 1)
seed
#> [1] 720
 
gen_cluster <- function(param = c(1, 0), n = 10000, n_cluster = 50, rho = .8) {
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
#> -3.0867 -0.6375 -0.0903  0.5357  4.0759 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 0.815631   0.009986  81.677  < 2e-16 ***
#> x           0.028337   0.007511   3.772 0.000163 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.9778 on 9998 degrees of freedom
#> Multiple R-squared:  0.001421,   Adjusted R-squared:  0.001322 
#> F-statistic: 14.23 on 1 and 9998 DF,  p-value: 0.0001626
 
# standard bootstrap
B <- 2000
 
# basic bootstrap, not parallel
system.time(boot_fit <- multiwayvcov::cluster.boot(lm_fit, 
                           as.factor(data$cluster), 
                           R = B, 
                           boot_type = "residual", 
                           wild_type = "rademacher", 
                           parallel = TRUE))
#>    user  system elapsed 
#>   19.52    0.06   19.75
 
 
system.time(
  boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
)
#>    user  system elapsed 
#>    0.50    0.09    0.59
```

And let’s have a look at the output:

``` r
lmtest::coeftest(lm_fit, boot_fit)
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 0.815631   0.123455  6.6067 4.129e-11 ***
#> x           0.028337   0.059502  0.4762    0.6339    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)")
#> [1] "The wild cluster bootstrap p-value for the parameter (Intercept) is 0 , with B 2000 bootstrap iterations."
boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
#> [1] "The wild cluster bootstrap p-value for the parameter x is 0.5705 , with B 2000 bootstrap iterations."

lm_robust_fit <- lm_robust(y ~ x, data = data, clusters = cluster)
lm_robust_fit %>% 
summary()
#> 
#> Call:
#> lm_robust(formula = y ~ x, data = data, clusters = cluster)
#> 
#> Standard error type:  CR2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper    DF
#> (Intercept)  0.81563    0.12652  6.4466 6.383e-08  0.56090   1.0704 45.64
#> x            0.02834    0.04805  0.5898 5.588e-01 -0.06893   0.1256 38.02
#> 
#> Multiple R-squared:  0.001421 ,  Adjusted R-squared:  0.001322 
#> F-statistic: 0.3478 on 1 and 49 DF,  p-value: 0.5581
# does the method work with object lm_robust?
#boottest.lm_robust(lm_robust_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)") # ror: need to feed in data
boottest.lm_robust(lm_robust_fit, data = data, clustid = data$cluster, B = B, seed = seed, param = "x") 
#> [1] "The wild cluster bootstrap p-value for the parameter x is 0.5705 , with B 2000 bootstrap iterations."
```
