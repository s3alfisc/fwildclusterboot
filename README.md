
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
#>     boottest, boottest.lm, boottest.lm_robust

## basic example code

seed <- sample(1:1000, 1)
seed
#> [1] 694
 
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
B <- 1000
 
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
#>   16.50    0.04   16.87
 
 
system.time(
  boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
)
#>    user  system elapsed 
#>    1.90    0.36    2.31
```

And let’s have a look at the output:

``` r
lmtest::coeftest(lm_fit, boot_fit)
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 1.119350   0.105368 10.6232   <2e-16 ***
#> x           0.037111   0.052774  0.7032   0.4819    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "(Intercept)")
#> [1] "The wild cluster bootstrap p-value for the parameter (Intercept) is 0 , with B 1000 bootstrap iterations."
boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = "x")
#> [1] "The wild cluster bootstrap p-value for the parameter x is 0.495 , with B 1000 bootstrap iterations."

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
#> (Intercept)  1.11935    0.10673 10.4873 4.869e-14  0.90478   1.3339 48.27
#> x            0.03711    0.05329  0.6963 4.909e-01 -0.07112   0.1453 34.63
#> 
#> Multiple R-squared:  0.003702 ,  Adjusted R-squared:  0.003652 
#> F-statistic: 0.4849 on 1 and 49 DF,  p-value: 0.4895

boottest.lm_robust(lm_robust_fit, data = data, clustid = data$cluster, B = B, seed = seed, param = "x") 
#> [1] "The wild cluster bootstrap p-value for the parameter x is 0.495 , with B 1000 bootstrap iterations."
```

Now, with a real data set:

``` r

data("petersen")

# cluster by firmid
lm_robust_fit_real <- lm_robust(y ~ x, clusters = firmid, data = petersen)
lm_robust_fit_real %>% 
  summary()
#> 
#> Call:
#> lm_robust(formula = y ~ x, data = petersen, clusters = firmid)
#> 
#> Standard error type:  CR2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper    DF
#> (Intercept)  0.02968    0.06704  0.4427 6.582e-01  -0.1020   0.1614 498.7
#> x            1.03483    0.05068 20.4199 3.002e-59   0.9351   1.1346 308.8
#> 
#> Multiple R-squared:  0.2078 ,    Adjusted R-squared:  0.2076 
#> F-statistic:   417 on 1 and 499 DF,  p-value: < 2.2e-16
boottest.lm_robust(lm_robust_fit_real, data = petersen, clustid = petersen$firmid, B = B, param = "(Intercept)")
#> [1] "The wild cluster bootstrap p-value for the parameter (Intercept) is 0.656 , with B 1000 bootstrap iterations."
boottest.lm_robust(lm_robust_fit_real, data = petersen, clustid = petersen$firmid, B = B, param = "x")
#> [1] "The wild cluster bootstrap p-value for the parameter x is 0 , with B 1000 bootstrap iterations."

# cluster by year
lm_robust_fit_real <- lm_robust(y ~ x, clusters = year, data = petersen)
lm_robust_fit_real %>% 
  summary()
#> 
#> Call:
#> lm_robust(formula = y ~ x, data = petersen, clusters = year)
#> 
#> Standard error type:  CR2 
#> 
#> Coefficients:
#>             Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper    DF
#> (Intercept)  0.02968    0.02339   1.269 2.364e-01 -0.02324   0.0826 9.000
#> x            1.03483    0.03340  30.987 1.899e-10  0.95927   1.1104 8.989
#> 
#> Multiple R-squared:  0.2078 ,    Adjusted R-squared:  0.2076 
#> F-statistic: 960.2 on 1 and 9 DF,  p-value: 1.861e-10
boottest.lm_robust(lm_robust_fit_real, data = petersen, clustid = petersen$year, B = B, param = "(Intercept)")
#> [1] "The wild cluster bootstrap p-value for the parameter (Intercept) is 0.229 , with B 1000 bootstrap iterations."
boottest.lm_robust(lm_robust_fit_real, data = petersen, clustid = petersen$year, B = B, param = "x")
#> [1] "The wild cluster bootstrap p-value for the parameter x is 0 , with B 1000 bootstrap iterations."
```

How to get p-values for all
parameters?

``` r
# lapply(1:length(names(coef(lm_fit))), function(i) boottest.lm(lm_fit, clustid = data$cluster, B = B, seed = seed, param = names(coef(lm_fit))[i]))
# 
# vboot <- Vectorize(boottest.lm_robust, vectorize.args = "param")
# 
# vboot(lm_robust_fit, data = data, clustid = data$cluster, B = B, seed = seed, param = c("(Intercept)", "x"))
```
