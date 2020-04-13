
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwildclusterboot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
<!-- badges: end -->

The goal of fwildclusterboot is to estimate a fast wild cluster
bootstrap for linear regression models of classes “lm”, “lm\_robust” and
“felm” (from packages base, estimatr and lfe).

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
#> [1] 774
 
gen_cluster <- function(param = c(1, 0), n = 20000, n_cluster = 50, rho = .8) {
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

 
# standard bootstrap
B <- 10000
 
```

As can be seen, the fast bootstrap is much faster.

``` r
# basic bootstrap, not parallel
system.time(boot_fit <- multiwayvcov::cluster.boot(lm_fit, 
                           as.factor(data$cluster), 
                           R = B, 
                           boot_type = "residual", 
                           wild_type = "rademacher", 
                           parallel = TRUE))
#>    user  system elapsed 
#>  182.13    0.45  193.40
 
 
system.time(
  boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
)
#>    user  system elapsed 
#>    1.88    7.38   11.95
```

And let’s have a look at the output:

``` r
lmtest::coeftest(lm_fit, boot_fit)
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 0.9294973  0.1324880  7.0157 2.361e-12 ***
#> x           0.0051399  0.0657761  0.0781    0.9377    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)")
#> [1] "The wild cluster bootstrap p-value for the parameter (Intercept) is 0 , with B 10000 bootstrap iterations."
boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
#> [1] "The wild cluster bootstrap p-value for the parameter x is 0.931 , with B 10000 bootstrap iterations."

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
#> (Intercept)  0.92950    0.13456 6.90746 9.591e-09   0.6590   1.2000 48.61
#> x            0.00514    0.05964 0.08618 9.318e-01  -0.1155   0.1258 38.62
#> 
#> Multiple R-squared:  4.755e-05 , Adjusted R-squared:  -2.452e-06 
#> F-statistic: 0.007426 on 1 and 49 DF,  p-value: 0.9317
# does the method work with object lm_robust?
#boottest.lm_robust(lm_robust_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)") # ror: need to feed in data
boottest.lm_robust(lm_robust_fit, data = data, clustid = data$cluster, B = B, seed = seed, param = "x") 
#> [1] "The wild cluster bootstrap p-value for the parameter x is 0.931 , with B 10000 bootstrap iterations."
```
