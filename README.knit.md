---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# fwildclusterboot

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/fwildclusterboot)](https://CRAN.R-project.org/package=fwildclusterboot)
<!-- badges: end -->

The goal of fwildclusterboot is to estimate a fast wild cluster bootstrap for linear regression models of classes "lm", "lm_robust" and "felm" (from packages base, estimatr and lfe).

## Installation

You can install the released version of fwildclusterboot from github...

``` r
install.packages("fwildclusterboot")
```




In a first step, simulate some data


```r

library(fwildclusterboot)
B <- 10000
seed <- 1345671
set.seed(seed)


voters <- fabricate(
  N = 2000,
  group_id = rep(1:20, 100),
  ideology = draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = 0.1),
  ideological_label = draw_ordered(
    x = ideology,
    break_labels = c(
      "Very Conservative", "Conservative",
      "Liberal", "Very Liberal"
    )
  ),
  income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
  Q1_immigration = draw_likert(x = ideology, type = 7),
  Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
  treatment = draw_binary(0.5, N = N),
  proposition_vote = draw_binary(latent = ideology + 0.01 * treatment, link = "probit")
)

setDT(voters)
voters[, log_income := log(income)]
voters[, Q1_immigration := as.factor(Q1_immigration) ]
voters[, Q2_defence := as.factor(Q2_defence)]

head(voters)
#>      ID group_id    ideology ideological_label      income       Q1_immigration
#> 1: 0001        1 -0.05943174      Conservative   16208.622 Don't Know / Neutral
#> 2: 0002        2  2.11075601      Very Liberal   27050.976                Agree
#> 3: 0003        3 -0.57228225      Conservative  287686.820        Lean Disagree
#> 4: 0004        4 -0.98017940      Conservative 2943631.268        Lean Disagree
#> 5: 0005        5  1.42551925      Very Liberal    2557.386           Lean Agree
#> 6: 0006        6 -1.10655250 Very Conservative  631037.107        Lean Disagree
#>              Q2_defence treatment proposition_vote log_income
#> 1: Don't Know / Neutral         1                1   9.693299
#> 2:         Strong Agree         0                1  10.205478
#> 3: Don't Know / Neutral         0                0  12.569628
#> 4: Don't Know / Neutral         1                0  14.895155
#> 5:                Agree         0                0   7.846741
#> 6:        Lean Disagree         1                0  13.355120
```

The fwildclusterboot package supports estimation of linear models based on base R's lm() function, estimatr's lm_robust function, 
lfe's felm() function and fixest's feols() function. 


```r
library(estimatr)
library(lfe)
library(fixest)

lm_fit <- lm(proposition_vote ~ treatment + ideology + log_income +Q1_immigration + Q2_defence, weights = NULL, data = voters)
lm_robust_fit <- lm_robust(proposition_vote ~ treatment + ideology + log_income, fixed_effects = ~ Q1_immigration + Q2_defence, weights = NULL, data = voters)
lm_robust_fit1 <- lm_robust(proposition_vote ~ treatment + ideology + log_income + Q1_immigration + Q2_defence, weights = NULL, data = voters )
feols_fit <- feols(proposition_vote ~ treatment + ideology + log_income, fixef = c("Q1_immigration", "Q2_defence"), weights = NULL, data = voters)
felm_fit <- felm(proposition_vote ~ treatment + ideology + log_income | Q1_immigration + Q2_defence, weights = NULL, data = voters)
```

The boottest command offers two functions. First, it calculates p-values for a given null hypothesis. This is ususally extremely fast: 









